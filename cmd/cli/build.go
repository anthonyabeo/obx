package cli

import (
	"fmt"
	"log"
	"os"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/backend"
	"github.com/anthonyabeo/obx/src/backend/target/riscv"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/anthonyabeo/obx/src/modgraph"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
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

func init() {
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "", "the module that specify the entrypoint into the program")
	buildCmd.Flags().StringVarP(&buildArgs.Path, "path", "p", "", "the filesystem path to the root source directory. Defaults to the current directory")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "the name of the output file to produce")
	buildCmd.Flags().IntVarP(&buildArgs.TabWidth, "tabWidth", "t", 4, "how many space should represent a tab")
	buildCmd.Flags().IntVarP(&buildArgs.OptLevel, "optlevel", "O", 2, "Optimisation level (0-3)")
	buildCmd.Flags().StringVarP(&buildArgs.EnablePasses, "passes", "P", "", "Comma-separated list of optimisation passes to enable (overrides -O)")
	buildCmd.Flags().StringVarP(&buildArgs.DisablePasses, "disable-passes", "D", "", "Comma-separated list of optimisation passes to disable")
	buildCmd.Flags().BoolVarP(&buildArgs.Verbose, "verbose", "V", false, "Output detailed optimization output")
	buildCmd.Flags().BoolVarP(&buildArgs.Asm, "asm", "S", false, "Output the assembly code to a .s file")
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

		// ── 1. Discover and order modules ────────────────────────────────
		sorted, err := resolveModules(path)
		if err != nil {
			log.Fatal(err)
		}

		fmt.Println("Import order:")
		for _, h := range sorted {
			fmt.Printf("  %s from %s\n", h.Name, h.File)
		}

		// ── 2. Parse ─────────────────────────────────────────────────────
		ctx, _ := newContext(tabWidth, 32)
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
		hirGen := hir.NewGenerator(ctx, obx)
		hirProgram := hirGen.Generate()

		Builder := mir.NewIRBuilder(ctx)
		MIRProgram := Builder.Build(hirProgram)

		for _, module := range MIRProgram.Modules {
			for _, function := range module.Funcs {
				opt.BuildCFG(function)
			}
		}

		// ── 5. Optimise ───────────────────────────────────────────────────
		config := map[string]any{
			"verbose":       verbose,
			"optlevel":      optLevel,
			"enablePasses":  enablePasses,
			"disablePasses": disablePasses,
		}

		pm := opt.NewPassManager()
		pm.ConfigurePasses(config)

		// ── 6. Emit assembly ──────────────────────────────────────────────
		for _, module := range MIRProgram.Modules {
			root, err := modgraph.FindProjectRoot()
			if err != nil {
				log.Printf("failed to find project root: %v", err)
				root = "."
			}

			asmPath := root + "/out/" + module.Name + ".s"
			asmFile, err := os.Create(asmPath)
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
