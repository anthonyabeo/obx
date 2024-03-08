package cli

import (
	"log"

	"github.com/urfave/cli/v2"
)

func Run(args []string) error {
	app := &cli.App{
		Name:  "obx",
		Usage: "obx <command> [arguments]",
		Commands: []*cli.Command{
			loadCmd,
		},
	}

	if err := app.Run(args); err != nil {
		log.Fatal(err)
	}

	return nil
}
