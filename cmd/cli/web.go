package cli

import (
	"log"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/cmd/web"
)

// ── Command ──────────────────────────────────────────────────────────────────

var webArgs struct {
	Addr      string
	TabWidth  int
	MaxErrors int
}

func init() {
	webCmd.Flags().StringVarP(&webArgs.Addr, "addr", "a", ":8080", "host:port to listen on")
	webCmd.Flags().IntVarP(&webArgs.TabWidth, "tabWidth", "t", 4, "tab width for diagnostic display")
	webCmd.Flags().IntVar(&webArgs.MaxErrors, "max-errors", 50, "maximum number of errors before stopping")
}

var webCmd = &cobra.Command{
	Use:   "web",
	Short: "start a web server with a UI and JSON API for checking Oberon+ code",
	Long: `web starts an HTTP server that exposes:

  GET  /             browser-based Oberon+ editor and diagnostic viewer
  POST /api/check    JSON API: {"source":"…","filename":"…"} → diagnostics
  GET  /api/version  build / runtime info

CORS is enabled on all endpoints (Access-Control-Allow-Origin: *) so the API
can be called directly from external editors, scripts, or CI tooling.`,

	Run: func(cmd *cobra.Command, args []string) {
		cfg := web.Config{
			Addr:      webArgs.Addr,
			TabWidth:  webArgs.TabWidth,
			MaxErrors: webArgs.MaxErrors,
		}
		if err := web.Start(cfg); err != nil {
			log.Fatalf("web: %v", err)
		}
	},
}


