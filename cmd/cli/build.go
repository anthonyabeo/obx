package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/codegen"
	"github.com/anthonyabeo/obx/src/codegen/target/riscv"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var buildArgs struct {
	Roots  []string // --root (repeatable); falls back to roots in obx.mod
	Entry  string   // --entry; falls back to entry in obx.mod
	Output string

	TabWidth      int
	OptLevel      int
	EnablePasses  string
	DisablePasses string
	Verbose       bool
	Asm           bool
}

func init() {
	buildCmd.Flags().StringArrayVarP(&buildArgs.Roots, "root", "r", nil,
		"source root directory (repeatable; defaults to roots in obx.mod)")
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "",
		"entry module to build (defaults to entry in obx.mod; omit to build all)")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "name of the output file to produce")
	buildCmd.Flags().IntVarP(&buildArgs.TabWidth, "tabWidth", "t", 4, "how many spaces should represent a tab")
	buildCmd.Flags().IntVarP(&buildArgs.OptLevel, "optlevel", "O", 2, "optimisation level (0-3)")
	buildCmd.Flags().StringVarP(&buildArgs.EnablePasses, "passes", "P", "", "comma-separated optimisation passes to enable (overrides -O)")
	buildCmd.Flags().StringVarP(&buildArgs.DisablePasses, "disable-passes", "D", "", "comma-separated optimisation passes to disable")
	buildCmd.Flags().BoolVarP(&buildArgs.Verbose, "verbose", "V", false, "output detailed optimisation info")
	buildCmd.Flags().BoolVarP(&buildArgs.Asm, "asm", "S", false, "print assembly to stdout")
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module(s) and their dependencies into object files",
	Long: `build runs the full compilation pipeline (discovery, parsing, semantic
analysis, IR lowering, optimisation, and code generation).

When --root is omitted, obx.mod in the nearest parent directory is read for
source roots and the default entry module.  --entry and --root always take
precedence over obx.mod values.`,

	Run: func(cmd *cobra.Command, args []string) {
		roots := buildArgs.Roots
		entry := buildArgs.Entry
		projectDir := "."

		// ── 0. Fall back to obx.mod when no roots are given ──────────────
		if len(roots) == 0 {
			dir, err := project.FindProjectRoot()
			if err != nil {
				log.Fatalf("build: no --root given and %s", err)
			}
			projectDir = dir

			m, err := project.LoadManifest(dir)
			if err != nil {
				log.Fatalf("build: %v", err)
			}
			roots = m.Roots
			if entry == "" {
				entry = m.Entry
			}
		}

		// ── 1. Discover and order modules ────────────────────────────────
		sorted, graph, err := resolveModules(roots...)
		if err != nil {
			log.Fatal(err)
		}

		sorted = reachableFrom(sorted, graph, entry)

		fmt.Printf("Building %d module(s)", len(sorted))
		if entry != "" {
			fmt.Printf("  (entry: %s)", entry)
		}
		fmt.Println()
		for _, h := range sorted {
			fmt.Printf("  %-30s  %s\n", h.Key, h.File)
		}

		// ── 2. Parse ─────────────────────────────────────────────────────
		ctx, _ := newContext(buildArgs.TabWidth, 32)
		obx := ast.NewOberonX()

		if ok := parseModules(sorted, ctx, obx); !ok {
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		// ── 3. Semantic analysis ──────────────────────────────────────────
		s := sema.NewSema(ctx, obx)
		s.Validate()

		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		// ── 4. Lower: AST → HIR → MIR ────────────────────────────────────
		hirGen := desugar.NewGenerator(ctx, obx)
		hirProgram := hirGen.Generate()

		Builder := mir.NewIRBuilder(ctx)
		MIRProgram := Builder.Build(hirProgram)

		for _, module := range MIRProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// ── 5. Optimise ───────────────────────────────────────────────────
		pm := opt.NewPassManager()
		pm.ConfigurePasses(map[string]any{
			"verbose":       buildArgs.Verbose,
			"optlevel":      buildArgs.OptLevel,
			"enablePasses":  buildArgs.EnablePasses,
			"disablePasses": buildArgs.DisablePasses,
		})

		// ── 6. Emit assembly ──────────────────────────────────────────────
		outDir := filepath.Join(projectDir, "out")
		for _, module := range MIRProgram.Modules {
			asmPath := filepath.Join(outDir, module.Name+".s")
			asmFile, err := os.Create(asmPath)
			if err != nil {
				log.Printf("failed to create assembly file: %v", err)
				continue
			}
			defer asmFile.Close()

			targetDesc := filepath.Join(projectDir, "src", "codegen", "target", "desc")
			ss := codegen.Compile(module, riscv.NewRV64IMAFDTarget(), targetDesc)
			if buildArgs.Asm {
				fmt.Println(ss)
			}

			if _, err := asmFile.WriteString(ss + "\n\n"); err != nil {
				log.Printf("failed to write assembly: %v", err)
			}
		}
	},
}
