package cli

import (
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var buildArgs struct {
	Entry  string
	Path   string
	Output string
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module or definition (along with its dependencies) into an object file",
	Run: func(cmd *cobra.Command, args []string) {
		entry, _ := cmd.Flags().GetString("entry")
		path, _ := cmd.Flags().GetString("path")

		obx := ast.NewOberonX()
		sm := report.NewSourceManager()
		reporter := report.NewBufferedReporter(sm, 32, report.StdoutSink{
			Source: sm,
			Writer: os.Stdout,
		})

		ctx := &report.Context{
			Source:   sm,
			Reporter: reporter,
			TabWidth: 4,
			Envs:     map[string]*ast.Environment{},
		}
		visited := map[string]bool{}

		files := make(map[string]string) // moduleName → fullPath
		filepath.Walk(path, func(p string, info os.FileInfo, err error) error {
			if strings.HasSuffix(p, ".obx") || strings.HasSuffix(p, ".def") {
				name := strings.TrimSuffix(filepath.Base(p), filepath.Ext(p))
				files[name] = p
			}
			return nil
		})

		// Load and parse all modules recursively
		if err := loadModule(ctx, obx, visited, path, entry, files); err != nil {
			log.Fatal(err)
		}

		// Perform semantic analysis
		s := sema.NewSema(ctx, obx)
		s.Validate()
	},
}
