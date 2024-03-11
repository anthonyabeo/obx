package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/urfave/cli/v2"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

var buildArgs struct {
	entry  string
	path   string
	output string
}

var buildCmd = &cli.Command{
	Name:        "build",
	Description: "compile module or definition (along with its dependencies) into an object file",
	Usage:       "build [flags]",
	Flags: []cli.Flag{
		&cli.StringFlag{
			Name:        "entry",
			Aliases:     []string{"e"},
			Required:    true,
			Destination: &buildArgs.entry,
			Usage:       "the module that specify the entrypoint into the program",
		},
		&cli.StringFlag{
			Name:        "path",
			Aliases:     []string{"p"},
			Required:    false,
			Destination: &buildArgs.path,
			Usage:       "the filesystem path to the 'entry' module. Defaults to the current directory",
		},
		&cli.StringFlag{
			Name:        "output",
			Aliases:     []string{"o"},
			Required:    false,
			Destination: &buildArgs.output,
			Usage:       "the name of the output file to produce",
		},
	},
	Action: runBuild,
}

func runBuild(ctx *cli.Context) (err error) {
	module := ctx.Value("e").(string)
	path := ctx.Value("p").(string)

	if len(path) == 0 {
		path, err = os.Getwd()
		if err != nil {
			panic("error finding the current working directory")
		}
	}

	obx := ast.NewOberon()

	_ = ParseModule(obx, module, path)

	tsOrd := topologicalSort(obx)

	vst := sema.NewVisitor(obx)
	for _, name := range tsOrd {
		unit := obx.Units()[name]
		unit.Accept(vst)
	}

	return nil
}

func GetModule(module, path string) (mod *Module, err error) {
	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fmt.Errorf("error reading directory '%s': %s", path, err)
	}

	for _, e := range entries {
		if !e.IsDir() && e.Name() == module+".obx" {
			filePath := filepath.Join(path, fmt.Sprintf("%s.obx", module))
			input, err := os.ReadFile(filePath)
			if err != nil {
				log.Fatalf("error opening file '%s': %s", e.Name(), err)
			}

			mod = &Module{file: filePath, input: input}
			break
		}
	}

	return
}

func ParseModule(obx *ast.Oberon, module, path string) ast.Unit {
	mod, err := GetModule(module, path)
	if err != nil {
		log.Println(err)
		os.Exit(1)
	}

	file := token.NewFile(mod.file, len(mod.input))
	lex := lexer.NewLexer(file, mod.input)

	p := parser.NewParser(lex, scope.Global)
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
			u := ParseModule(obx, imp.Name.Name, importPath)
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
