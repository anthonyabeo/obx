package cli

import (
	"log"
	"time"

	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/cmd/web"
)

// ── Command ──────────────────────────────────────────────────────────────────

var webArgs struct {
	Addr           string
	MaxErrors      int
	ReadTimeout    time.Duration
	WriteTimeout   time.Duration
	IdleTimeout    time.Duration
	RateLimit      float64
	RateBurst      int
	AdminAddr      string
	MaxBodyBytes   int
	MaxSourceBytes int
	MaxFilenameLen int
	AllowedOrigins  []string
	APIKey          string
}

func init() {
	webCmd.Flags().StringVarP(&webArgs.Addr, "addr", "a", ":8080", "host:port to listen on")
	webCmd.Flags().IntVar(&webArgs.MaxErrors, "max-errors", 50, "maximum number of errors before stopping")
	webCmd.Flags().DurationVar(&webArgs.ReadTimeout, "read-timeout", 10*time.Second, "HTTP server read timeout (e.g. 15s)")
	webCmd.Flags().DurationVar(&webArgs.WriteTimeout, "write-timeout", 30*time.Second, "HTTP server write timeout (e.g. 30s)")
	webCmd.Flags().DurationVar(&webArgs.IdleTimeout, "idle-timeout", 120*time.Second, "HTTP server idle timeout (e.g. 2m)")
	webCmd.Flags().Float64Var(&webArgs.RateLimit, "rate", 5.0, "per-IP request rate (requests/sec)")
	webCmd.Flags().IntVar(&webArgs.RateBurst, "burst", 10, "per-IP rate limit burst")
	webCmd.Flags().StringVar(&webArgs.AdminAddr, "admin-addr", "127.0.0.1:9090", "localhost-only admin listener for /metrics")
	webCmd.Flags().IntVar(&webArgs.MaxBodyBytes, "max-body-bytes", 256*1024, "maximum JSON request body size in bytes")
	webCmd.Flags().IntVar(&webArgs.MaxSourceBytes, "max-source-bytes", 200*1024, "maximum 'source' payload size in bytes")
	webCmd.Flags().IntVar(&webArgs.MaxFilenameLen, "max-filename-len", 128, "maximum filename length")
	webCmd.Flags().StringSliceVar(&webArgs.AllowedOrigins, "allowed-origin", []string{}, "trusted origin(s) allowed for CORS (repeatable)")
	webCmd.Flags().StringVar(&webArgs.APIKey, "api-key", "", "optional API key required via X-API-Key header for API access")
}

var webCmd = &cobra.Command{
	Use:   "web",
	Short: "start a web server with a UI and JSON API for checking Oberon+ code",
	Long: `web starts an HTTP server that exposes:

  GET  /             browser-based Oberon+ editor and diagnostic viewer
  POST /api/check    JSON API: {"source":"…","filename":"…"} → diagnostics
  GET  /api/version  build / runtime info

CORS is configurable: use --allowed-origin to list trusted origins (repeatable),
or supply --api-key to require an X-API-Key header for API access. When neither
option is provided cross-origin requests are denied. This allows safely
exposing the API to external editors, scripts, or CI tooling when configured.`,

	Run: func(cmd *cobra.Command, args []string) {
		cfg := web.Config{
			Addr:           webArgs.Addr,
			MaxErrors:      webArgs.MaxErrors,
			ReadTimeout:    webArgs.ReadTimeout,
			WriteTimeout:   webArgs.WriteTimeout,
			IdleTimeout:    webArgs.IdleTimeout,
			RateLimit:      webArgs.RateLimit,
			RateLimitBurst: webArgs.RateBurst,
			AdminAddr:      webArgs.AdminAddr,
			MaxBodyBytes:   webArgs.MaxBodyBytes,
			MaxSourceBytes: webArgs.MaxSourceBytes,
			MaxFilenameLen: webArgs.MaxFilenameLen,
			AllowedOrigins: webArgs.AllowedOrigins,
			APIKey:         webArgs.APIKey,
		}
		if err := web.Start(cfg); err != nil {
			log.Fatalf("web: %v", err)
		}
	},
}
