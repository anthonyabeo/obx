package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/project"
)

var newArgs struct {
	SrcDir string // --src
	Entry  string // --entry
}

func init() {
	newCmd.Flags().StringVarP(&newArgs.SrcDir, "src", "s", "src",
		"name of the source root directory to create inside the project")
	newCmd.Flags().StringVarP(&newArgs.Entry, "entry", "e", "Main",
		"name of the entry module to scaffold")
}

var newCmd = &cobra.Command{
	Use:   "new <project-name>",
	Short: "bootstrap a new Oberon+ project",
	Long: `new creates a new Oberon+ project directory with an obx.mod manifest,
a source directory, and a starter entry module.

  obx new calculator
  obx new calculator --src src --entry Main

The generated layout is:

  calculator/
  ├── obx.mod
  └── src/
      └── Main.obx`,

	Args: cobra.ExactArgs(1),

	Run: func(cmd *cobra.Command, args []string) {
		name := args[0]
		projectDir := filepath.Join(".", name)

		// ── 1. Guard against overwriting an existing directory ────────────
		if _, err := os.Stat(projectDir); err == nil {
			log.Fatalf("new: directory %q already exists", projectDir)
		}

		srcDir := filepath.Join(projectDir, newArgs.SrcDir)

		// ── 2. Create directory tree ──────────────────────────────────────
		if err := os.MkdirAll(srcDir, 0755); err != nil {
			log.Fatalf("new: create directories: %v", err)
		}

		// ── 3. Write obx.mod ──────────────────────────────────────────────
		m := project.Manifest{
			Name:  name,
			Roots: []string{newArgs.SrcDir},
			Entry: newArgs.Entry,
		}
		if err := project.WriteManifest(projectDir, m); err != nil {
			log.Fatalf("new: %v", err)
		}

		// ── 4. Write the starter entry module ─────────────────────────────
		entryFile := filepath.Join(srcDir, newArgs.Entry+".obx")
		if err := os.WriteFile(entryFile, starterModule(newArgs.Entry), 0644); err != nil {
			log.Fatalf("new: write entry module: %v", err)
		}

		// ── 5. Report ─────────────────────────────────────────────────────
		fmt.Printf("Created %q\n\n", name)
		fmt.Printf("  %-36s  project manifest\n",
			filepath.Join(name, project.ManifestFile))
		fmt.Printf("  %-36s  entry module\n",
			filepath.Join(name, newArgs.SrcDir, newArgs.Entry+".obx"))
		fmt.Printf("\nNext steps:\n")
		fmt.Printf("  cd %s\n", name)
		fmt.Printf("  obx check\n")
		fmt.Printf("  obx build\n")
	},
}

// starterModule returns the source text for a minimal entry module named name.
func starterModule(name string) []byte {
	var b strings.Builder
	fmt.Fprintf(&b, "module %s\n", name)
	fmt.Fprintf(&b, "\nbegin\n")
	fmt.Fprintf(&b, "  // TODO: add your program here\n")
	fmt.Fprintf(&b, "end %s\n", name)
	return []byte(b.String())
}

