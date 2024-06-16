package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate"
)

var buildArgs struct {
	Entry  string
	Path   string
	Output string
	Opt    []string
	EmitIR bool
}

var buildCmd = &cobra.Command{
	Use:   "build",
	Short: "compile module or definition (along with its dependencies) into an object file",
	Run: func(cmd *cobra.Command, args []string) {
		module, err := cmd.Flags().GetString("entry")
		path, err := cmd.Flags().GetString("path")
		emitIR, err := cmd.Flags().GetBool("emit-ir")
		opts, err := cmd.Flags().GetStringArray("opt")

		if len(path) == 0 {
			path, err = os.Getwd()
			if err != nil {
				panic("diagnostics finding the current working directory")
			}
		}

		obx := ast.NewOberon()
		scopes := map[string]scope.Scope{}
		for _, unit := range obx.Units() {
			scopes[unit.Name()] = nil
		}

		errReporter := diagnostics.NewStdErrReporter(10)
		_ = ParseModule(obx, module, path, errReporter)

		tsOrd := topologicalSort(obx)

		vst := sema.NewVisitor(scopes, errReporter)
		for _, name := range tsOrd {
			unit := obx.Units()[name]
			unit.Accept(vst)
		}

		tVst := translate.NewVisitor(scopes)
		for _, name := range tsOrd {
			unit := obx.Units()[name]
			unit.Accept(tVst)
		}

		if len(opts) > 0 {
			for _, op := range opts {
				switch op {
				case "mem2reg":
					for _, f := range tVst.Module.GetFunctionList() {
						cfg := f.CFG()
						opt.ComputePhiInsertLocations(cfg)

						stack := &opt.Stack{}
						vst := map[string]bool{}
						opt.RegisterPromotion(cfg, cfg.Entry, vst, stack)
					}
				case "dce":
				}
			}
		}

		if emitIR {
			for _, f := range tVst.Module.GetFunctionList() {
				fmt.Println(f)
			}
		}
	},
}

func GetModule(module, path string) (mod *Module, err error) {
	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fmt.Errorf("diagnostics reading directory '%s': %s", path, err)
	}

	for _, e := range entries {
		if !e.IsDir() && e.Name() == module+".obx" {
			filePath := filepath.Join(path, fmt.Sprintf("%s.obx", module))
			input, err := os.ReadFile(filePath)
			if err != nil {
				log.Fatalf("diagnostics opening file '%s': %s", e.Name(), err)
			}

			mod = &Module{file: filePath, input: input}
			break
		}
	}

	return
}

func ParseModule(obx *ast.Oberon, module, path string, errReporter diagnostics.ErrReporter) ast.Unit {
	mod, err := GetModule(module, path)
	if err != nil {
		log.Println(err)
		os.Exit(1)
	}

	file := token.NewFile(mod.file, len(mod.input))
	lex := lexer.NewLexer(file, mod.input)

	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx.AddUnit(unit.Name(), unit)
	obx.TopOrd = append(obx.TopOrd, mod.file)

	for _, imp := range unit.ListImport() {
		var importPath string

		if len(imp.ImportPath) > 0 {
			var p []string
			for _, id := range imp.ImportPath {
				p = append(p, id.Name)
			}
			importPath = fmt.Sprintf("%s/%s", path, strings.Join(p, "/"))
		} else {
			importPath = path
		}

		if _, processed := obx.Units()[imp.Name.Name]; !processed {
			u := ParseModule(obx, imp.Name.Name, importPath, errReporter)
			obx.AddEdge(u.Name(), unit.Name())
		} else {
			obx.TopOrd = append(obx.TopOrd, fmt.Sprintf("%s/%s.obx", importPath, imp.Name.Name))

			fmt.Println("import cycle detected")
			for _, p := range obx.TopOrd {
				fmt.Printf("\t%s\n", p)
			}
			os.Exit(1)
		}
	}

	return unit
}

type Module struct {
	file  string
	input []byte
}

func topologicalSort(obx *ast.Oberon) []string {
	// construct a hash mapping nodes to their indegrees
	var inDeg = make(map[string]int, 0)
	for name := range obx.Units() {
		inDeg[name] = 0
	}

	for _, unit := range obx.Units() {
		for _, adj := range obx.Neighbors(unit.Name()) {
			inDeg[adj] += 1
		}
	}

	// track nodes with no incoming edges
	var zeroInDegNodes = make([]string, 0)
	for name, inD := range inDeg {
		if inD == 0 {
			zeroInDegNodes = append(zeroInDegNodes, name)
		}
	}

	// initially, no nodes in our ordering
	var tsOrd = make([]string, 0)
	for len(zeroInDegNodes) > 0 {
		unit := zeroInDegNodes[0]
		tsOrd = append(tsOrd, unit)
		zeroInDegNodes = zeroInDegNodes[1:]

		// decrement the indegree of that node's neighbors
		for _, neighbor := range obx.Neighbors(unit) {
			inDeg[neighbor] -= 1
			if inDeg[neighbor] == 0 {
				zeroInDegNodes = append(zeroInDegNodes, neighbor)
			}
		}
	}

	if len(tsOrd) != len(obx.Units()) {
		// graph has a cycle
		log.Fatal("Import cycle of modules and definitions not allowed")

	}

	return tsOrd
}
