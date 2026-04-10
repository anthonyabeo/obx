package cli

import "github.com/spf13/cobra"

func Run() error {
	var rootCmd = &cobra.Command{
		Use:   "obx",
		Short: "obx is a tool for managing Oberon+ source code",
	}
	rootCmd.AddCommand(buildCmd)
	rootCmd.AddCommand(checkCmd)
	rootCmd.AddCommand(newCmd)
	rootCmd.AddCommand(webCmd)

	return rootCmd.Execute()
}
