package cli

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var checkArgs struct {
	Path      string
	Target    string // --target (needed to inject platform directives)
	MaxErrors int
	Quiet     bool
	Defines   []string // --define (repeatable); NAME or NAME=VALUE
}

func init() {
	checkCmd.Flags().StringVarP(&checkArgs.Path, "path", "p", "",
		"source root directory (defaults to roots in obx.mod)")
	checkCmd.Flags().StringVarP(&checkArgs.Target, "target", "T", "rv64imafd",
		"target architecture (used to select the stdlib platform layer)")
	checkCmd.Flags().IntVar(&checkArgs.MaxErrors, "max-errors", 32, "maximum number of errors to report before stopping")
	checkCmd.Flags().BoolVarP(&checkArgs.Quiet, "quiet", "q", false, "suppress informational output; only show diagnostics")
	checkCmd.Flags().StringArrayVarP(&checkArgs.Defines, "define", "d", nil,
		"set a compile-time directive constant: NAME (bool true) or NAME=VALUE (bool/int/float)")
}

var checkCmd = &cobra.Command{
	Use:   "check",
	Short: "parse and type-check modules without producing any output files",
	Long: `check runs the full front-end pipeline (module discovery, parsing, name
resolution, type checking, and control-flow analysis) and reports any
diagnostics found. No object files, assembly, or other artefacts are written.

When --path is omitted, obx.mod in the nearest parent directory is read for
source roots.

Exit code is 0 when all modules are clean, 1 when errors are found.`,

	Run: func(cmd *cobra.Command, args []string) {
		entry := buildArgs.Entry
		roots := []string{checkArgs.Path}
		label := checkArgs.Path // used in progress output

		ctx, _ := newContext(checkArgs.MaxErrors)

		// Inject platform directives first so --define can still override.
		injectPlatformDirectives(ctx, checkArgs.Target)

		if err := applyDirectives(ctx, checkArgs.Defines); err != nil {
			fmt.Fprintf(os.Stderr, "check: %v\n", err)
			os.Exit(1)
		}

		// ── 0. Fall back to obx.mod when --path is not given ─────────────
		var manifest project.Manifest
		if checkArgs.Path == "" {
			dir, err := project.FindProjectRoot()
			if err != nil {
				log.Fatalf("check: no --path given and %s", err)
			}
			manifest, err = project.LoadManifest(dir)
			if err != nil {
				log.Fatalf("check: %v", err)
			}
			roots = manifest.Roots
			label = dir
			if entry == "" {
				entry = manifest.Entry
			}
		}

		// ── 0a. Prepend stdlib root (dual-root discovery) ─────────────────
		if stdlibRoot := project.ResolveStdlibRoot(manifest); stdlibRoot != "" {
			roots = append([]string{stdlibRoot}, roots...)
		}

		// ── 1. Discover and order modules ────────────────────────────────
		sorted, graph, err := resolveModules(ctx, roots...)
		if err != nil {
			fmt.Fprintf(os.Stderr, "check: %v\n", err)
			os.Exit(1)
		}

		sorted, err = project.ReachableFrom(sorted, graph, entry)
		if err != nil {
			fmt.Fprintf(os.Stderr, "check: %v\n", err)
			os.Exit(1)
		}

		if !checkArgs.Quiet {
			fmt.Printf("Checking %d module(s) in %s\n", len(sorted), label)
			for _, h := range sorted {
				fmt.Printf("  %-20s  %s\n", h.Key, h.File)
			}
			fmt.Println()
		}

		// ── 2. Parse ─────────────────────────────────────────────────────
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
		if !checkArgs.Quiet {
			fmt.Printf("ok\t%d module(s) checked, no errors\n", len(sorted))
		}
	},
}
