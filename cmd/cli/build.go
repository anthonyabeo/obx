package cli

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/obx/src/codegen"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	zlog "github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/codegen/target"
	_ "github.com/anthonyabeo/obx/src/codegen/target/arm64" // register arm64-apple-macos + aarch64-apple-darwin
	_ "github.com/anthonyabeo/obx/src/codegen/target/riscv" // register rv64imafd
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var buildArgs struct {
	Roots  []string // --root (repeatable); falls back to roots in obx.mod
	Entry  string   // --entry; falls back to entry in obx.mod
	Output string   // --output; final executable path/name (defaults to project name)
	Target string   // --target; defaults to rv64imafd

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
}

func init() {
	buildCmd.Flags().StringArrayVarP(&buildArgs.Roots, "root", "r", nil,
		"source root directory (repeatable; defaults to roots in obx.mod)")
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "",
		"entry module to build (defaults to entry in obx.mod; omit to build all)")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "final executable name/path (defaults to the project name under build/)")
	buildCmd.Flags().StringVarP(&buildArgs.Target, "target", "T", "rv64imafd",
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
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module(s), link them, and produce a runnable executable",
	Long: `build runs the full compilation pipeline (discovery, parsing, semantic
analysis, IR lowering, optimisation, and code generation).

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
		toolchain, err := buildToolchainFor(mach)
		if err != nil {
			log.Fatalf("build: %v", err)
		}
		projectDir := boot.ProjectDir
		manifest := boot.Manifest
		roots := boot.Roots
		entry := boot.Entry

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
		preBundles, loadedNames := loadPrecompiledBundles(ctx, sorted)

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

		lowered := minir.LowerWithContext(hirProgram, ctx)
		mergePrecompiledMinirModules(lowered, preBundles)
		dedupMinirExternals(lowered)

		for _, mod := range lowered.Modules {
			for _, fn := range mod.Functions {
				miniropt.Mem2Reg(fn)
				miniropt.LoadForward(fn)
				miniropt.CleanCFG(fn)
			}
		}

		if verifyErrs := minir.VerifyProgram(lowered); len(verifyErrs) > 0 {
			for _, verr := range verifyErrs {
				zlog.Error().Err(verr).Msg("build: minir verification failed")
			}
			log.Fatalf("build: minir verification failed with %d error(s)", len(verifyErrs))
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

		// ── 6. Optimise ───────────────────────────────────────────────────
		pm := opt.NewPassManager()
		pm.ConfigurePasses(map[string]any{
			"verbose":       buildArgs.Verbose,
			"optlevel":      buildArgs.OptLevel,
			"enablePasses":  buildArgs.EnablePasses,
			"disablePasses": buildArgs.DisablePasses,
		})

		Builder := obxir.NewIRBuilder(ctx.Target.WordSize)
		MIRProgram := Builder.Build(hirProgram, ctx)

		for _, module := range MIRProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// ── 7. Emit assembly ──────────────────────────────────────────────
		buildDir := filepath.Join(projectDir, "build")
		if err := os.MkdirAll(buildDir, 0755); err != nil {
			zlog.Error().Err(err).Msg("failed to create build dir")
		}
		seenDLL := make(map[string]bool)
		var linkLibs []string
		var asmPaths []string
		var objPaths []string
		buildFailed := false

		for _, module := range MIRProgram.Modules {
			asmPath := filepath.Join(buildDir, module.Name+".s")
			targetDesc := filepath.Join(projectDir, "src", "codegen", "target", "desc")
			ss, err := codegen.Compile(module, mach, targetDesc, codegen.CompileOptions{Debug: buildArgs.Asm})
			if err != nil {
				zlog.Error().Err(err).Msg("compile failed")
				buildFailed = true
				continue
			}
			if buildArgs.Asm {
				fmt.Println(ss)
			}

			if err := os.WriteFile(asmPath, []byte(ss+"\n\n"), 0644); err != nil {
				zlog.Error().Err(err).Str("file", asmPath).Msg("failed to write assembly")
				buildFailed = true
				continue
			}
			asmPaths = append(asmPaths, asmPath)

			objPath := filepath.Join(buildDir, module.Name+".o")
			assembleArgs := append([]string{}, toolchain.assemblerArgs...)
			assembleArgs = append(assembleArgs, "-c", asmPath, "-o", objPath)
			assembleCmd := exec.Command(toolchain.assembler, assembleArgs...)
			assembleCmd.Stdout = os.Stdout
			assembleCmd.Stderr = os.Stderr
			if err := assembleCmd.Run(); err != nil {
				zlog.Error().Err(err).Str("file", asmPath).Msg("failed to assemble object file")
				buildFailed = true
				continue
			}
			objPaths = append(objPaths, objPath)

			// Collect unique dll names for linker flags.
			for _, ext := range module.Externals {
				if ext.DLLName != "" && !seenDLL[ext.DLLName] {
					seenDLL[ext.DLLName] = true
					linkLibs = append(linkLibs, ext.DLLName)
				}
			}
		}

		if buildFailed {
			log.Fatalf("build: failed to assemble one or more modules")
		}

		var flags []string
		for _, lib := range linkLibs {
			flags = append(flags, "-l"+lib)
		}
		flagsPath := filepath.Join(buildDir, "link.flags")
		if err := os.WriteFile(flagsPath, []byte(strings.Join(flags, "\n")+"\n"), 0644); err != nil {
			zlog.Error().Err(err).Msg("failed to write link.flags")
		} else {
			fmt.Printf("Linker flags written to %s\n", flagsPath)
		}

		outputPath := resolveOutputPath(projectDir, manifest, mach.Name(), buildArgs.Output)
		if err := os.MkdirAll(filepath.Dir(outputPath), 0755); err != nil {
			zlog.Error().Err(err).Str("dir", filepath.Dir(outputPath)).Msg("failed to create executable output dir")
			log.Fatalf("build: %v", err)
		}

		linkArgs := append([]string{}, toolchain.linkerArgs...)
		linkArgs = append(linkArgs, "-o", outputPath)
		linkArgs = append(linkArgs, objPaths...)
		linkArgs = append(linkArgs, flags...)
		linkCmd := exec.Command(toolchain.linker, linkArgs...)
		linkCmd.Stdout = os.Stdout
		linkCmd.Stderr = os.Stderr
		if err := linkCmd.Run(); err != nil {
			log.Fatalf("build: link failed: %v", err)
		}

		fmt.Printf("Executable written to %s\n", outputPath)

		if !buildArgs.KeepAsm {
			for _, p := range asmPaths {
				if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
					zlog.Warn().Err(err).Str("file", p).Msg("build: failed to remove assembly file")
				}
			}
		}
		if !buildArgs.KeepObj {
			for _, p := range objPaths {
				if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
					zlog.Warn().Err(err).Str("file", p).Msg("build: failed to remove object file")
				}
			}
		}
	},
}
