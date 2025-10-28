package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/modgraph"
	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/target/riscv"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

var buildArgs struct {
	Entry    string
	Path     string
	TabWidth int
	Output   string

	OptLevel      int
	EnablePasses  string
	DisablePasses string
	Verbose       bool

	Asm bool
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module or definition (along with its dependencies) into an object file",
	Run: func(cmd *cobra.Command, args []string) {
		path, _ := cmd.Flags().GetString("path")
		tabWidth, _ := cmd.Flags().GetInt("tabWidth")

		optLevel, _ := cmd.Flags().GetInt("optlevel")
		enablePasses, _ := cmd.Flags().GetString("passes")
		disablePasses, _ := cmd.Flags().GetString("disable-passes")
		verbose, _ := cmd.Flags().GetBool("verbose")
		asm, _ := cmd.Flags().GetBool("asm")

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
			Source:    srcMgr,
			Reporter:  reporter,
			TabWidth:  tabWidth,
			Env:       ast.NewEnv(),
			Names:     adt.NewStack[string](),
			ExprLists: adt.NewStack[[]ast.Expression](),
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

		// lowering & opt
		hirGen := hir.NewGenerator(ctx, obx)
		hirProgram := hirGen.Generate()

		mirGen := mir.NewGenerator(ctx)
		mirProgram := mirGen.Generate(hirProgram)

		for _, module := range mirProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// Optimization
		config := map[string]any{
			"verbose":       verbose,
			"optlevel":      optLevel,
			"enablePasses":  enablePasses,
			"disablePasses": disablePasses,
		}

		pm := opt.NewPassManager()
		pm.ConfigurePasses(config)

		for _, module := range mirProgram.Modules {
			root, err := modgraph.FindProjectRoot()
			if err != nil {
				log.Printf("failed to find project root: %v", err)
				root = "."
			}

			path := root + "/out/" + module.Name + ".s"
			asmFile, err := os.Create(path)
			if err != nil {
				log.Printf("failed to create assembly file: %v", err)
			}
			defer asmFile.Close()

			targetDesc := root + "/src/backend/target/desc"
			ss := backend.Compile(module, riscv.NewRV64IMAFDTarget(), targetDesc)
			if asm {
				fmt.Println(ss)
			}

			if _, err := asmFile.WriteString(ss + "\n\n"); err != nil {
				log.Printf("failed to write to assembly file: %v", err)
			}
		}
	},
}
