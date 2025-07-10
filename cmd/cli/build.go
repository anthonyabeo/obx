package cli

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/sema"
	"log"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/modgraph"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

var buildArgs struct {
	Entry    string
	Path     string
	TabWidth int
	Output   string
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module or definition (along with its dependencies) into an object file",
	Run: func(cmd *cobra.Command, args []string) {
		path, _ := cmd.Flags().GetString("path")
		tabWidth, _ := cmd.Flags().GetInt("tabWidth")

		files, err := modgraph.DiscoverModuleFiles(path)
		if err != nil {
			log.Fatal(err)
		}

		// 2. Extract headers
		var headers []modgraph.Header
		for _, file := range files {
			mods, err := modgraph.ScanModuleHeaders(file)
			if err != nil {
				log.Fatalf("error in %s: %v", file, err)
			}
			headers = append(headers, mods...)
		}

		// 3. Build graph
		graph, err := modgraph.BuildImportGraph(path, headers)
		if err != nil {
			log.Fatal(err)
		}

		// 4. Topologically sort
		sorted, err := modgraph.TopoSort(graph)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Println("Import order:")
		for _, h := range sorted {
			fmt.Printf("  %s from %s\n", h.Name, h.File)
		}

		obx := ast.NewOberonX()
		srcMgr := report.NewSourceManager()
		reporter := report.NewBufferedReporter(srcMgr, 32, report.StdoutSink{
			Source: srcMgr,
			Writer: os.Stdout,
		})

		ctx := &report.Context{
			Source:   srcMgr,
			Reporter: reporter,
			TabWidth: tabWidth,
			Env:      ast.NewEnv(),
		}

		for _, header := range sorted {
			data, err := os.ReadFile(header.File)
			if err != nil {
				log.Fatal(err)
			}

			ctx.FileName = filepath.Base(header.File)
			ctx.FilePath = header.File
			ctx.Content = data[header.StartPos:header.EndPos]

			p := parser.NewParser(ctx)
			unit := p.Parse()
			if ctx.Reporter.ErrorCount() > 0 {
				ctx.Reporter.Flush()
				log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
			}

			obx.AddUnit(unit)
		}

		// semantics analysis
		s := sema.NewSema(ctx, obx)
		s.Validate()
	},
}
