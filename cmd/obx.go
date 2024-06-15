package main

import (
	"fmt"
	"github.com/anthonyabeo/obx/cmd/cli"
	"os"
)

func main() {
	if err := cli.Run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

}
