package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/minir"
	zlog "github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/emit"
	_ "github.com/anthonyabeo/obx/src/backend/stages"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var buildArgs struct {
	Roots  []string // --root (repeatable); falls back to roots in obx.mod
	Entry  string   // --entry; falls back to entry in obx.mod
	Output string   // --output; final executable path/name (defaults to project name)
	Target string   // --target; defaults to backend target constants

	Defines       []string // --define (repeatable); NAME or NAME=VALUE
	OptLevel      int
	EnablePasses  string
	DisablePasses string
	Verbose       bool
	Asm           bool
	KeepAsm       bool
	KeepObj       bool

	// minir text emission
	EmitMinir bool   // --emit-minir: write textual minir for every module
	MinirOut  string // --minir-out: directory to write .minir files (defaults to build/)

	// verification
	StrictVerify bool // --strict-verify: treat minir verification errors as fatal
}

func init() {
	buildCmd.Flags().StringArrayVarP(&buildArgs.Roots, "root", "r", nil,
		"source root directory (repeatable; defaults to roots in obx.mod)")
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "",
		"entry module to build (defaults to entry in obx.mod; omit to build all)")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "final executable name/path (defaults to the project name under build/)")
	buildCmd.Flags().StringVarP(&buildArgs.Target, "target", "T", target.RV64IMAFDName,
		"target architecture (available: "+strings.Join(target.Available(), ", ")+")")
	buildCmd.Flags().StringArrayVarP(&buildArgs.Defines, "define", "d", nil,
		"set a compile-time directive constant: NAME (bool true) or NAME=VALUE (bool/int/float)")
	buildCmd.Flags().IntVarP(&buildArgs.OptLevel, "optlevel", "O", 2, "optimisation level (0-3)")
	buildCmd.Flags().StringVarP(&buildArgs.EnablePasses, "passes", "P", "", "comma-separated optimisation passes to enable (overrides -O)")
	buildCmd.Flags().StringVarP(&buildArgs.DisablePasses, "disable-passes", "D", "", "comma-separated optimisation passes to disable")
	buildCmd.Flags().BoolVarP(&buildArgs.Verbose, "verbose", "V", false, "output detailed optimisation info")
	buildCmd.Flags().BoolVarP(&buildArgs.Asm, "asm", "S", false, "print assembly to stdout")
	buildCmd.Flags().BoolVar(&buildArgs.KeepAsm, "keep-asm", false, "keep generated .s files after linking")
	buildCmd.Flags().BoolVar(&buildArgs.KeepObj, "keep-obj", false, "keep generated .o files after linking")
	buildCmd.Flags().BoolVarP(&buildArgs.EmitMinir, "emit-minir", "I", false,
		"write textual minir for every lowered module to --minir-out (or build/)")
	buildCmd.Flags().StringVar(&buildArgs.MinirOut, "minir-out", "",
		"directory to write .minir files when --emit-minir is set (defaults to build/)")
	buildCmd.Flags().BoolVar(&buildArgs.StrictVerify, "strict-verify", false,
		"treat minir IR verification errors as a fatal build error (default: warn only)")
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "lower module(s) into backend MIR",
	Long: `build runs the front-end pipeline (discovery, parsing, semantic
analysis, and minir lowering) and then lowers the result into backend MIR.

When --root is omitted, obx.mod in the nearest parent directory is read for
source roots and the default entry module.  --entry and --root always take
precedence over obx.mod values.`,

	Run: func(cmd *cobra.Command, args []string) {
		boot, err := bootstrapFrontEnd(bootstrapOptions{
			Command:   "build",
			Target:    buildArgs.Target,
			Defines:   buildArgs.Defines,
			MaxErrors: 32,
			Roots:     buildArgs.Roots,
			Entry:     buildArgs.Entry,
		})
		if err != nil {
			log.Fatalf("build: %v", err)
		}
		ctx := boot.Ctx
		mach := boot.Machine
		projectDir := boot.ProjectDir
		manifest := boot.Manifest
		roots := boot.Roots
		entry := boot.Entry

		toolchain, err := buildToolchainFor(mach)
		if err != nil {
			log.Fatalf("build: %v", err)
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
		// Attempt to load precompiled .obxi bundles for discovered modules.
		// This injects pre-built sema scopes and/or pre-lowered minir modules
		// for stdlib modules (if available) so we can skip reparsing/re-sema
		// while still providing symbols to the rest of the program.
		preBundles, loadedNames := loadPrecompiledBundles(ctx, sorted, boot.StdlibRoot)

		// Build a filtered list of headers that still need parsing (i.e. those
		// that did not have a precompiled .obxi bundle). This allows the
		// pipeline to completely skip parse+sema for precompiled modules.
		toParse, skipped := splitParsedHeaders(sorted, loadedNames)
		if len(skipped) > 0 {
			zlog.Info().Int("count", len(skipped)).Str("modules", strings.Join(skipped, ", ")).Msg("build: skipping parse/sema for precompiled modules")
		}

		if ok := parseModules(toParse, ctx, obx); !ok {
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		// Mark the designated entry module so the backend emits it as _main.
		// Only the entry module's __init_<Name> function should be renamed; all
		// imported modules keep their __init_<Name> symbols.
		if entry != "" {
			for _, unit := range obx.Units {
				if mod, ok := unit.(*ast.Module); ok && mod.BName == entry {
					mod.IsEntry = true
					break
				}
			}
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

		lowerer := minir.New(ctx)
		lowered := lowerer.Lower(hirProgram)
		mergePrecompiledMinirModules(lowered, preBundles)
		dedupMinirExternals(lowered)

		// ── 6. Run optimization passes ─────────────────────────────────────
		pm := miniropt.NewPassManager()

		// Configure passes based on CLI flags
		if buildArgs.EnablePasses != "" {
			// Explicit pass list overrides -O level
			if err := pm.ConfigureFromPassList(buildArgs.EnablePasses); err != nil {
				log.Fatalf("build: invalid pass list: %v", err)
			}
		} else {
			// Use passes for the selected optimization level
			pm.ConfigureFromLevel(buildArgs.OptLevel)
		}

		// Apply disable-passes filter
		if buildArgs.DisablePasses != "" {
			if err := pm.DisablePasses(buildArgs.DisablePasses); err != nil {
				log.Fatalf("build: invalid disable-passes list: %v", err)
			}
		}

		// Enable verbose output if requested
		if buildArgs.Verbose {
			pm.SetVerbose(true)
			pm.SetLogWriter(os.Stdout)
		}

		// Run passes on the entire program
		totalChanges := pm.RunOnProgram(lowered)
		if buildArgs.Verbose {
			stats := pm.Stats()
			fmt.Printf("Passes applied: %d total changes across %d pass invocations\n", totalChanges, len(stats))
		}

		if verifyErrs := minir.VerifyProgram(lowered); len(verifyErrs) > 0 {
			for _, verr := range verifyErrs {
				zlog.Error().Err(verr).Msg("build: minir verification failed")
			}
			if buildArgs.StrictVerify {
				log.Fatalf("build: minir verification failed with %d error(s)", len(verifyErrs))
			} else {
				zlog.Warn().Int("count", len(verifyErrs)).
					Msg("build: minir verification reported errors (use --strict-verify to treat as fatal)")
			}
		}

		if buildArgs.EmitMinir {
			minirDir := buildArgs.MinirOut
			if minirDir == "" {
				minirDir = filepath.Join(projectDir, "build")
			}
			if err := os.MkdirAll(minirDir, 0755); err != nil {
				zlog.Error().Err(err).Str("dir", minirDir).Msg("build: cannot create minir output dir")
			} else {
				for _, mod := range lowered.Modules {
					outPath := filepath.Join(minirDir, mod.Name+".minir")
					f, err := os.Create(outPath)
					if err != nil {
						zlog.Error().Err(err).Str("file", outPath).Msg("build: create minir output file")
						continue
					}
					if _, err := minir.NewEmitter(f).EmitModule(mod); err != nil {
						zlog.Error().Err(err).Str("module", mod.Name).Msg("build: emit minir failed")
					}
					if err := f.Close(); err != nil {
						zlog.Error().Err(err).Str("file", outPath).Msg("build: close minir output file")
					}
					fmt.Printf("  minir: wrote %s\n", outPath)
				}
			}
		}

		fmt.Println("Backend MIR lowering complete.")

		backendTarget, err := backendTargetFor(mach.Name())
		if err != nil {
			log.Fatalf("build: %v", err)
		}

		emitter := emit.New(emit.Config{
			ProjectDir: projectDir,
			Manifest:   manifest,
			Target:     backendTarget,
			Toolchain: emit.Toolchain{
				Assembler:     toolchain.assembler,
				AssemblerArgs: append([]string(nil), toolchain.assemblerArgs...),
				Linker:        toolchain.linker,
				LinkerArgs:    append([]string(nil), toolchain.linkerArgs...),
			},
			Output:   buildArgs.Output,
			PrintAsm: buildArgs.Asm,
			KeepAsm:  buildArgs.KeepAsm,
			KeepObj:  buildArgs.KeepObj,
		})
		pipeline := backend.NewPipelineDriver(backendTarget)
		pipeline.Assemble = emitter.Assemble
		pipeline.Link = emitter.Link

		bridge, err := pipeline.Run(lowered)
		if err != nil {
			log.Fatalf("build: backend pipeline failed: %v", err)
		}
		if buildArgs.Verbose {
			fmt.Printf("Backend pipeline lowered %d module(s) and prepared %d function plan(s)\n", len(bridge.MIR.Modules), len(bridge.Plans))
		}
	},
}
