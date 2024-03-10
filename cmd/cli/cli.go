package cli

import (
	"log"

	"github.com/urfave/cli/v2"
)

func Run(args []string) error {
	app := &cli.App{
		Name:  "obx",
		Usage: "Oberon+ Compiler",
		Commands: []*cli.Command{
			buildCmd,
		},
	}

	if err := app.Run(args); err != nil {
		log.Fatal(err)
	}

	return nil
}
