package cli

import (
	"fmt"
	"github.com/urfave/cli/v2"
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
	fmt.Println(ctx.Value("module"))
	fmt.Println(ctx.Value("path"))
	return err
}
