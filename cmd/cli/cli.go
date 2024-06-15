package cli

import "github.com/spf13/cobra"

func Run() error {
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "", "the module that specify the entrypoint into the program")
	buildCmd.Flags().StringVarP(&buildArgs.Path, "path", "p", "", "the filesystem path to the 'entry' module. Defaults to the current directory")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "the name of the output file to produce")
	buildCmd.Flags().StringArrayVar(&buildArgs.Opt, "opt", []string{}, "list of optimizations to perform")

	var rootCmd = &cobra.Command{Use: "obx"}
	rootCmd.AddCommand(buildCmd)

	return rootCmd.Execute()
}
