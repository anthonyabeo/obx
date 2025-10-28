package cli

import "github.com/spf13/cobra"

func Run() error {
	buildCmd.Flags().StringVarP(&buildArgs.Entry, "entry", "e", "", "the module that specify the entrypoint into the program")
	buildCmd.Flags().StringVarP(&buildArgs.Path, "path", "p", "", "the filesystem path to the root source directory. Defaults to the current directory")
	buildCmd.Flags().StringVarP(&buildArgs.Output, "output", "o", "", "the name of the output file to produce")
	buildCmd.Flags().IntVarP(&buildArgs.TabWidth, "tabWidth", "t", 4, "how many space should represent a tab")

	buildCmd.Flags().IntVarP(&buildArgs.OptLevel, "optlevel", "O", 2, "Optimisation level (0-3)")
	buildCmd.Flags().StringVarP(&buildArgs.EnablePasses, "passes", "P", "", "Comma-separated list of optimisation passes to enable (overrides -O)")
	buildCmd.Flags().StringVarP(&buildArgs.DisablePasses, "disable-passes", "D", "", "Comma-separated list of optimisation passes to disable")
	buildCmd.Flags().BoolVarP(&buildArgs.Verbose, "verbose", "V", false, "Output detailed optimization output")
	buildCmd.Flags().BoolVarP(&buildArgs.Asm, "asm", "S", false, "Output the assembly code to a .s file")

	var rootCmd = &cobra.Command{
		Use:   "obx",
		Short: "obx is a tool for managing Oberon+ source code",
	}
	rootCmd.AddCommand(buildCmd)

	return rootCmd.Execute()
}
