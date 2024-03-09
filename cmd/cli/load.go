package cli

import (
	"log"
	"os"
	"path/filepath"

	"github.com/urfave/cli/v2"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

var loadArgs struct {
	module string
	path   string
}

var loadCmd = &cli.Command{
	Name:        "load",
	Description: "Load a module (along with its dependencies) and execute it",
	Usage:       "load [flags]",
	Flags: []cli.Flag{
		&cli.StringFlag{
			Name:        "module",
			Aliases:     []string{"m"},
			Required:    true,
			Destination: &loadArgs.module,
			Usage:       "name of the module",
		},
		&cli.StringFlag{
			Name:        "path",
			Aliases:     []string{"p"},
			Required:    false,
			Destination: &loadArgs.path,
			Usage:       "path to the directory where this module is declared",
		},
	},
	Action: runLoad,
}

func runLoad(ctx *cli.Context) (err error) {
	//module := ctx.Value("module").(string)
	path := ctx.Value("path").(string)

	if len(path) == 0 {
		path, err = os.Getwd()
		if err != nil {
			panic("error finding the current working directory")
		}
	}

	var obx = ast.NewOberon()

	ParseDir(path, obx)

	// run a topological sort on obx
	tsSeq := topologicalSort(obx.Program())

	// run the semantics analyzer on the topological ordering
	for _, mod := range tsSeq {
		module, _ := obx.Module(mod)

		sym := sema.NewModule(module.Mod, module.BeginName.Name)
		sema.Global.Insert(sym)

		scp := sema.NewScope(sema.Global, mod)

		vst := sema.NewVisitor(scp)
		vst.VisitModule(module)
	}

	return err
}

func topologicalSort(program map[string]ast.Unit) []string {
	// construct a hash mapping nodes to their indegrees
	var inDeg = make(map[string]int, 0)
	for name := range program {
		inDeg[name] = 0
	}

	for _, unit := range program {
		for _, adj := range unit.Incident() {
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
		for _, neighbor := range program[unit].Incident() {
			inDeg[neighbor] -= 1
			if inDeg[neighbor] == 0 {
				zeroInDegNodes = append(zeroInDegNodes, neighbor)
			}
		}
	}

	if len(tsOrd) != len(program) {
		// graph has a cycle
		log.Fatal("Import cycle of modules and definitions not allowed")

	}

	return tsOrd
}

func ParseDir(path string, obx *ast.Oberon) {
	entries, err := os.ReadDir(path)
	if err != nil {
		log.Fatalf("error reading directory '%s': %s", path, err)
		return
	}

	for _, e := range entries {
		if !e.IsDir() {
			fileName := filepath.Join(path, e.Name())
			input, err := os.ReadFile(fileName)
			if err != nil {
				log.Fatalf("error opening file '%s': %s", e.Name(), err)
			}

			file := token.NewFile(path, len(input))
			lex := lexer.NewLexer(file, input)

			p := parser.NewParser(lex)
			p.Oberon(obx)
		} else {
			ParseDir(filepath.Join(path, e.Name()), obx)
		}
	}

	for name, unit := range obx.Program() {
		switch u := unit.(type) {
		case *ast.Module:
			for _, imp := range u.ImportList {
				m, _ := obx.Module(imp.Name.Name)
				m.AddAdjacent(name)
			}
		}
	}
}
