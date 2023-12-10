package cli

import (
	"flag"
	"github.com/peterbourgon/ff/v3/ffcli"
	"strings"
)

var versionCmd = &ffcli.Command{
	Name:       "version",
	ShortUsage: "version",
	ShortHelp:  "Display the version of the compiler",
	LongHelp: strings.TrimSpace(`

	`),
	Exec: runBuild,
	FlagSet: (func() *flag.FlagSet {
		return nil
	})(),
}
