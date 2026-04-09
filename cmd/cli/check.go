package cli

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var checkArgs struct {
	Path      string
	TabWidth  int
	MaxErrors int
	Quiet     bool
}

func init() {
	checkCmd.Flags().StringVarP(&checkArgs.Path, "path", "p", "", "the filesystem path to the root source directory. Defaults to the current directory")
	checkCmd.Flags().IntVarP(&checkArgs.TabWidth, "tabWidth", "t", 4, "how many spaces should represent a tab")
	checkCmd.Flags().IntVar(&checkArgs.MaxErrors, "max-errors", 32, "maximum number of errors to report before stopping")
	checkCmd.Flags().BoolVarP(&checkArgs.Quiet, "quiet", "q", false, "suppress informational output; only show diagnostics")
}

var checkCmd = &cobra.Command{
	Use:   "check",
	Short: "parse and type-check modules without producing any output files",
	Long: `check runs the full front-end pipeline (module discovery, parsing, name
resolution, type checking, and control-flow analysis) and reports any
diagnostics found. No object files, assembly, or other artefacts are written.

Exit code is 0 when all modules are clean, 1 when errors are found.`,

	Run: func(cmd *cobra.Command, args []string) {
		path, _ := cmd.Flags().GetString("path")
		tabWidth, _ := cmd.Flags().GetInt("tabWidth")
		maxErrors, _ := cmd.Flags().GetInt("max-errors")
		quiet, _ := cmd.Flags().GetBool("quiet")

		// ── 1. Discover and order modules ────────────────────────────────
		sorted, err := resolveModules(path)
		if err != nil {
			fmt.Fprintf(os.Stderr, "check: %v\n", err)
			os.Exit(1)
		}

		if !quiet {
			fmt.Printf("Checking %d module(s) in %s\n", len(sorted), path)
			for _, h := range sorted {
				fmt.Printf("  %-20s  %s\n", h.Name, h.File)
			}
			fmt.Println()
		}

		// ── 2. Parse ─────────────────────────────────────────────────────
		ctx, _ := newContext(tabWidth, maxErrors)
		obx := ast.NewOberonX()

		if ok := parseModules(sorted, ctx, obx); !ok {
			n := ctx.Reporter.ErrorCount()
			fmt.Fprintf(os.Stderr, "check failed: %d parse error(s)\n", n)
			os.Exit(1)
		}

		// ── 3. Semantic analysis ──────────────────────────────────────────
		s := sema.NewSema(ctx, obx)
		s.Validate()

		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			n := ctx.Reporter.ErrorCount()
			fmt.Fprintf(os.Stderr, "check failed: %d error(s)\n", n)
			os.Exit(1)
		}

		// ── 4. Clean ──────────────────────────────────────────────────────
		if !quiet {
			fmt.Printf("ok\t%d module(s) checked, no errors\n", len(sorted))
		}
	},
}

