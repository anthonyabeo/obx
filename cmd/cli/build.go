package cli

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/urfave/cli/v2"
	"log"
	"os"
	"path/filepath"
	"strings"
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

	// checking for circular imports

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

	p := parser.NewParser(lex)
	unit := p.Parse()

	obx.AddUnit(unit.Name(), unit)

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

		u := ParseModule(obx, imp.Name.Name, importPath)

		obx.AddEdge(u.Name(), unit.Name())
	}

	return unit
}

type Module struct {
	file  string
	input []byte
}
