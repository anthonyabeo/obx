package cli

import "github.com/spf13/cobra"

func Run() error {
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "", "the modgraph that specify the entrypoint into the program")
	buildCmd.Flags().StringVarP(&buildArgs.Path, "path", "p", "", "the filesystem path to the root source directory. Defaults to the current directory")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "the name of the output file to produce")
	buildCmd.Flags().IntVarP(&buildArgs.TabWidth, "tabWidth", "t", 4, "how many space should represent a tab")

	var rootCmd = &cobra.Command{
		Use:   "obx",
		Short: "obx is a tool for managing Oberon+ source code",
	}
	rootCmd.AddCommand(buildCmd)

	return rootCmd.Execute()
}
