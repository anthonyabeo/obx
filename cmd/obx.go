package main

import (
	"fmt"
	"os"

	"github.com/anthonyabeo/obx/cmd/cli"
)

func main() {
	if err := cli.Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

}
