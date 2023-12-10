package cli

import (
	"context"
	"flag"
	"strings"

	"github.com/peterbourgon/ff/v3/ffcli"
)

var buildCmd = &ffcli.Command{
	Name:       "build",
	ShortUsage: "build [-o output] [build flags] [packages]",
	ShortHelp:  "Compile and produce an executable",
	LongHelp: strings.TrimSpace(`

	`),
	Exec: runBuild,
	FlagSet: (func() *flag.FlagSet {
		return nil
	})(),
}

func runBuild(ctx context.Context, args []string) (err error) {
	return err
}
