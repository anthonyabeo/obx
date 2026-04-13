package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/codegen"
	"github.com/anthonyabeo/obx/src/codegen/target"
	_ "github.com/anthonyabeo/obx/src/codegen/target/arm64"  // register arm64-apple-macos + aarch64-apple-darwin
	_ "github.com/anthonyabeo/obx/src/codegen/target/riscv"  // register rv64imafd
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var buildArgs struct {
	Roots  []string // --root (repeatable); falls back to roots in obx.mod
	Entry  string   // --entry; falls back to entry in obx.mod
	Output string
	Target string // --target; defaults to rv64imafd

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
	buildCmd.Flags().StringVarP(&buildArgs.Target, "target", "T", "rv64imafd",
		"target architecture (available: "+strings.Join(target.Available(), ", ")+")")
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

		// ── 0. Resolve target ─────────────────────────────────────────────
		mach, err := target.Lookup(buildArgs.Target)
		if err != nil {
			log.Fatalf("build: %v", err)
		}

		// ── 1. Fall back to obx.mod when no roots are given ───────────────
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

		// ── 2. Discover and order modules ─────────────────────────────────
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

		// ── 3. Parse ──────────────────────────────────────────────────────
		ctx, _ := newContext(32)

		obx := ast.NewOberonX()

		if ok := parseModules(sorted, ctx, obx); !ok {
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		// ── 4. Semantic analysis ──────────────────────────────────────────
		s := sema.NewSema(ctx, obx)
		s.Validate()

		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		// ── 5. Lower: AST → desugar → MIR ────────────────────────────────
		hirGen := desugar.NewGenerator(obx)
		hirProgram := hirGen.Generate()

		Builder := obxir.NewIRBuilder(ctx.Target.WordSize)
		MIRProgram := Builder.Build(hirProgram)

		for _, module := range MIRProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// ── 6. Optimise ───────────────────────────────────────────────────
		pm := opt.NewPassManager()
		pm.ConfigurePasses(map[string]any{
			"verbose":       buildArgs.Verbose,
			"optlevel":      buildArgs.OptLevel,
			"enablePasses":  buildArgs.EnablePasses,
			"disablePasses": buildArgs.DisablePasses,
		})

		// ── 7. Emit assembly ──────────────────────────────────────────────
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
			ss, err := codegen.Compile(module, mach, targetDesc, codegen.CompileOptions{Debug: buildArgs.Asm})
			if err != nil {
				log.Printf("compile failed: %v", err)
				continue
			}
			if buildArgs.Asm {
				fmt.Println(ss)
			}

			if _, err := asmFile.WriteString(ss + "\n\n"); err != nil {
				log.Printf("failed to write assembly: %v", err)
			}
		}
	},
}
