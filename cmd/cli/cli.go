package cli

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"strings"
	"text/tabwriter"

	"github.com/peterbourgon/ff/v3/ffcli"
)

func Run(args []string) error {
	rootCmd := &ffcli.Command{
		Name:       "obx",
		ShortUsage: "obx <command> [arguments]",
		ShortHelp:  "The Obreron+ Compiler.",
		LongHelp: strings.TrimSpace(`
use "obx <command> --help" for more information about the usage of a specific command
		`),
		Subcommands: []*ffcli.Command{
			versionCmd,
			buildCmd,
		},
		FlagSet:   nil,
		Exec:      func(context.Context, []string) error { return flag.ErrHelp },
		UsageFunc: usageFunc,
	}

	for _, c := range rootCmd.Subcommands {
		c.UsageFunc = usageFunc
	}

	if err := rootCmd.Parse(args); err != nil {
		if errors.Is(err, flag.ErrHelp) {
			return nil
		}
		return err
	}

	err := rootCmd.Run(context.Background())
	if errors.Is(err, flag.ErrHelp) {
		return nil
	}

	return err
}

func usageFunc(c *ffcli.Command) string {
	var b strings.Builder

	fmt.Fprintf(&b, "USAGE\n")
	if c.ShortUsage != "" {
		fmt.Fprintf(&b, "  %s\n", c.ShortUsage)
	} else {
		fmt.Fprintf(&b, "  %s\n", c.Name)
	}
	fmt.Fprintf(&b, "\n")

	if c.LongHelp != "" {
		fmt.Fprintf(&b, "%s\n\n", c.LongHelp)
	}

	if len(c.Subcommands) > 0 {
		fmt.Fprintf(&b, "SUBCOMMANDS\n")
		tw := tabwriter.NewWriter(&b, 0, 2, 2, ' ', 0)
		for _, subcommand := range c.Subcommands {
			fmt.Fprintf(tw, "  %s\t%s\n", subcommand.Name, subcommand.ShortHelp)
		}
		tw.Flush()
		fmt.Fprintf(&b, "\n")
	}

	if countFlags(c.FlagSet) > 0 {
		fmt.Fprintf(&b, "FLAGS\n")
		tw := tabwriter.NewWriter(&b, 0, 2, 2, ' ', 0)
		c.FlagSet.VisitAll(func(f *flag.Flag) {
			var s string
			name, usage := flag.UnquoteUsage(f)
			if isBoolFlag(f) {
				s = fmt.Sprintf("  --%s, --%s=false", f.Name, f.Name)
			} else {
				s = fmt.Sprintf("  --%s", f.Name) // Two spaces before --; see next two comments.
				if len(name) > 0 {
					s += " " + name
				}
			}
			// Four spaces before the tab triggers good alignment
			// for both 4- and 8-space tab stops.
			s += "\n    \t"
			s += strings.ReplaceAll(usage, "\n", "\n    \t")

			if f.DefValue != "" {
				s += fmt.Sprintf(" (default %s)", f.DefValue)
			}

			fmt.Fprintln(&b, s)
		})
		tw.Flush()
		fmt.Fprintf(&b, "\n")
	}

	return strings.TrimSpace(b.String())
}

func isBoolFlag(f *flag.Flag) bool {
	bf, ok := f.Value.(interface {
		IsBoolFlag() bool
	})
	return ok && bf.IsBoolFlag()
}

func countFlags(fs *flag.FlagSet) (n int) {
	fs.VisitAll(func(*flag.Flag) { n++ })
	return n
}
