package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/codegen"
	"github.com/anthonyabeo/obx/src/codegen/target"
	_ "github.com/anthonyabeo/obx/src/codegen/target/arm64" // register arm64-apple-macos + aarch64-apple-darwin
	_ "github.com/anthonyabeo/obx/src/codegen/target/riscv" // register rv64imafd
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/ir/obxir"
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

	Defines       []string // --define (repeatable); NAME or NAME=VALUE
	OptLevel      int
	EnablePasses  string
	DisablePasses string
	Verbose       bool
	Asm           bool

	// minir text emission
	EmitMinir bool   // --emit-minir: write textual minir for every module
	MinirOut  string // --minir-out: directory to write .minir files (defaults to build/)
}

func init() {
	buildCmd.Flags().StringArrayVarP(&buildArgs.Roots, "root", "r", nil,
		"source root directory (repeatable; defaults to roots in obx.mod)")
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "",
		"entry module to build (defaults to entry in obx.mod; omit to build all)")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "name of the output file to produce")
	buildCmd.Flags().StringVarP(&buildArgs.Target, "target", "T", "rv64imafd",
		"target architecture (available: "+strings.Join(target.Available(), ", ")+")")
	buildCmd.Flags().StringArrayVarP(&buildArgs.Defines, "define", "d", nil,
		"set a compile-time directive constant: NAME (bool true) or NAME=VALUE (bool/int/float)")
	buildCmd.Flags().IntVarP(&buildArgs.OptLevel, "optlevel", "O", 2, "optimisation level (0-3)")
	buildCmd.Flags().StringVarP(&buildArgs.EnablePasses, "passes", "P", "", "comma-separated optimisation passes to enable (overrides -O)")
	buildCmd.Flags().StringVarP(&buildArgs.DisablePasses, "disable-passes", "D", "", "comma-separated optimisation passes to disable")
	buildCmd.Flags().BoolVarP(&buildArgs.Verbose, "verbose", "V", false, "output detailed optimisation info")
	buildCmd.Flags().BoolVarP(&buildArgs.Asm, "asm", "S", false, "print assembly to stdout")
	buildCmd.Flags().BoolVarP(&buildArgs.EmitMinir, "emit-minir", "I", false,
		"write textual minir for every lowered module to --minir-out (or build/)")
	buildCmd.Flags().StringVar(&buildArgs.MinirOut, "minir-out", "",
		"directory to write .minir files when --emit-minir is set (defaults to build/)")
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

		ctx, _ := newContext(32)

		// Inject platform directives first so --define can still override.
		injectPlatformDirectives(ctx, buildArgs.Target)

		if err := applyDirectives(ctx, buildArgs.Defines); err != nil {
			log.Fatalf("build: %v", err)
		}

		// ── 0. Resolve target ─────────────────────────────────────────────
		mach, err := target.Lookup(buildArgs.Target)
		if err != nil {
			log.Fatalf("build: %v", err)
		}

		// ── 1. Fall back to obx.mod when no roots are given ───────────────
		var manifest project.Manifest
		if len(roots) == 0 {
			dir, err := project.FindProjectRoot()
			if err != nil {
				log.Fatalf("build: no --root given and %s", err)
			}
			projectDir = dir

			manifest, err = project.LoadManifest(dir)
			if err != nil {
				log.Fatalf("build: %v", err)
			}
			roots = manifest.Roots
			if entry == "" {
				entry = manifest.Entry
			}
		}

		// ── 1a. Prepend the stdlib root (dual-root discovery) ─────────────
		if stdlibRoot := project.ResolveStdlibRoot(manifest); stdlibRoot != "" {
			roots = append([]string{stdlibRoot}, roots...)
		}

		// ── 2. Discover and order modules ─────────────────────────────────
		sorted, graph, err := resolveModules(ctx, roots...)
		if err != nil {
			log.Fatal(err)
		}

		sorted, err = project.ReachableFrom(sorted, graph, entry)
		if err != nil {
			log.Fatalf("invalid entry: %v", err)
		}

		fmt.Printf("Building %d module(s)", len(sorted))
		if entry != "" {
			fmt.Printf("  (entry: %s)", entry)
		}
		fmt.Println()
		for _, h := range sorted {
			fmt.Printf("  %-30s  %s\n", h.Key, h.File)
		}

		// ── 3. Parse ──────────────────────────────────────────────────────

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
		hirGen := desugar.NewGenerator(obx, ctx)
		hirProgram := hirGen.Generate()

		Builder := obxir.NewIRBuilder(ctx.Target.WordSize)
		MIRProgram := Builder.Build(hirProgram, ctx)

		for _, module := range MIRProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// ── 5a. Optionally emit textual minir ─────────────────────────────
		if buildArgs.EmitMinir {
			minirDir := buildArgs.MinirOut
			if minirDir == "" {
				// write textual minir output to the project's build/ directory by default
				minirDir = filepath.Join(projectDir, "build")
			}
			if err := os.MkdirAll(minirDir, 0755); err != nil {
				log.Printf("build: cannot create minir output dir %s: %v", minirDir, err)
			} else {
				lowered := minir.Lower(hirProgram)
				// Promote non-escaping scalar allocas, then forward store-to-load pairs.
				for _, mod := range lowered.Modules {
					for _, fn := range mod.Functions {
						miniropt.Mem2Reg(fn)
						miniropt.LoadForward(fn)
					}
				}
				for _, mod := range lowered.Modules {
					outPath := filepath.Join(minirDir, mod.Name+".minir")
					f, err := os.Create(outPath)
					if err != nil {
						log.Printf("build: create %s: %v", outPath, err)
						continue
					}
					if _, err := minir.NewEmitter(f).EmitModule(mod); err != nil {
						log.Printf("build: emit minir for %s: %v", mod.Name, err)
					}
					if err := f.Close(); err != nil {
						log.Printf("build: close %s: %v", outPath, err)
					}
					fmt.Printf("  minir: wrote %s\n", outPath)
				}
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
		// write generated artifacts (assembly, link flags) to the project's build/ directory
		buildDir := filepath.Join(projectDir, "build")
		if err := os.MkdirAll(buildDir, 0755); err != nil {
			log.Printf("failed to create build dir: %v", err)
		}
		seenDLL := make(map[string]bool)
		var linkLibs []string

		for _, module := range MIRProgram.Modules {
			asmPath := filepath.Join(buildDir, module.Name+".s")
			asmFile, err := os.Create(asmPath)
			if err != nil {
				log.Printf("failed to create assembly file: %v", err)
				continue
			}
			// explicitly close the asm file at the end of this iteration and log any error

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

			// Collect unique dll names for linker flags.
			for _, ext := range module.Externals {
				if ext.DLLName != "" && !seenDLL[ext.DLLName] {
					seenDLL[ext.DLLName] = true
					linkLibs = append(linkLibs, ext.DLLName)
												// close asm file for this module
												if err := asmFile.Close(); err != nil {
													log.Printf("failed to close assembly file %s: %v", asmPath, err)
												}
										}
			}
		}

		// Write out/link.flags so the downstream linker invocation can read it.
		if len(linkLibs) > 0 {
			var flags []string
			for _, lib := range linkLibs {
				flags = append(flags, "-l"+lib)
			}
			flagsPath := filepath.Join(buildDir, "link.flags")
			if err := os.MkdirAll(buildDir, 0755); err != nil {
				log.Printf("failed to create build dir: %v", err)
			}
			if err := os.WriteFile(flagsPath, []byte(strings.Join(flags, "\n")+"\n"), 0644); err != nil {
				log.Printf("failed to write link.flags: %v", err)
			} else {
				fmt.Printf("Linker flags written to %s\n", flagsPath)
			}
		}
	},
}
