package cli

import (
	"log"
	"time"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/cmd/web"
)

// ── Command ──────────────────────────────────────────────────────────────────

var webArgs struct {
	Addr      string
	MaxErrors int
	ReadTimeout  time.Duration
	WriteTimeout time.Duration
	IdleTimeout  time.Duration
	RateLimit    float64
	RateBurst    int
}

func init() {
	webCmd.Flags().StringVarP(&webArgs.Addr, "addr", "a", ":8080", "host:port to listen on")
	webCmd.Flags().IntVar(&webArgs.MaxErrors, "max-errors", 50, "maximum number of errors before stopping")
	webCmd.Flags().DurationVar(&webArgs.ReadTimeout, "read-timeout", 10*time.Second, "HTTP server read timeout (e.g. 15s)")
	webCmd.Flags().DurationVar(&webArgs.WriteTimeout, "write-timeout", 30*time.Second, "HTTP server write timeout (e.g. 30s)")
	webCmd.Flags().DurationVar(&webArgs.IdleTimeout, "idle-timeout", 120*time.Second, "HTTP server idle timeout (e.g. 2m)")
	webCmd.Flags().Float64Var(&webArgs.RateLimit, "rate", 5.0, "per-IP request rate (requests/sec)")
	webCmd.Flags().IntVar(&webArgs.RateBurst, "burst", 10, "per-IP rate limit burst")
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
			Addr:           webArgs.Addr,
			MaxErrors:      webArgs.MaxErrors,
			ReadTimeout:    webArgs.ReadTimeout,
			WriteTimeout:   webArgs.WriteTimeout,
			IdleTimeout:    webArgs.IdleTimeout,
			RateLimit:      webArgs.RateLimit,
			RateLimitBurst: webArgs.RateBurst,
		}
		if err := web.Start(cfg); err != nil {
			log.Fatalf("web: %v", err)
		}
	},
}

