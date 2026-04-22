// Package web implements the obx HTTP server: a browser-based Oberon+ editor
// (GET /) backed by a JSON API (POST /api/check, GET /api/version).
//
// CORS is enabled on every route so the API can be consumed by external
// editors, scripts, and CI tooling without proxy configuration.
package web

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/rs/zerolog"
	zlog "github.com/rs/zerolog/log"
)

// package-level holder for the rate-limited counter so limiter can increment it
var rateLimitedMetric prometheus.Counter

func incrementRateLimitedMetric() {
	if rateLimitedMetric != nil {
		rateLimitedMetric.Inc()
	}
}

// Config holds all runtime parameters for the web server.
type Config struct {
	Addr      string // host:port to listen on, e.g. ":8080"
	MaxErrors int    // max errors before the pipeline stops
	// Timeouts (defaults are set in the CLI and Start)
	ReadTimeout  time.Duration
	WriteTimeout time.Duration
	IdleTimeout  time.Duration
	// Rate limiting (requests per second and burst)
	RateLimit      float64
	RateLimitBurst int
	// AdminAddr binds a localhost-only admin listener for metrics/diagnostics (e.g. "127.0.0.1:9090").
	AdminAddr string
}

// Server owns the mux and carries Config so every handler can read it.
type Server struct {
	cfg Config
}

// Start creates a Server from cfg, registers routes, and blocks in
// http.ListenAndServe.  It returns a non-nil error only when the listener
// fails to bind (e.g. port in use).
func Start(cfg Config) error {
	s := &Server{cfg: cfg}

	mux := http.NewServeMux()
	mux.HandleFunc("/", s.HandleUI)
	mux.HandleFunc("/healthz", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})
	mux.HandleFunc("/readyz", func(w http.ResponseWriter, r *http.Request) {
		// readiness: for now we always report ready; could add deeper checks
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		w.WriteHeader(http.StatusOK)
		_, _ = w.Write([]byte("ok"))
	})
	// serve embedded static assets under /static/
	// (oberon-monarch.js is now served via /static/js/oberon-monarch.js)
	// generic static handler for embedded assets under /static/
	mux.HandleFunc("/static/", s.HandleStatic)
	mux.HandleFunc("/api/check", s.HandleCheck)
	mux.HandleFunc("/api/cfg", s.HandleCFG)
	mux.HandleFunc("/api/run", s.HandleRun)
	mux.HandleFunc("/api/version", s.HandleVersion)

	// wrap with CORS and logging middlewares
	handler := corsMiddleware(mux)
	handler = requestIDMiddleware(handler)

	// create IP rate limiter from config
	limiter := newIPRateLimiter(cfg.RateLimit, cfg.RateLimitBurst)
	handler = limiter.Middleware(handler)
	// start janitor to purge idle clients (runs until Start returns)
	limiter.StartJanitor(1*time.Minute, 5*time.Minute)
	defer limiter.StopJanitor()

	// logging should be outermost so it records blocked requests too
	handler = loggingMiddleware(handler)

	// --- Prometheus metrics ---
	// request counter labeled by method and path
	reqCounter := promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "obx_http_requests_total",
		Help: "Total HTTP requests received",
	}, []string{"method", "path"})
	// request duration histogram
	reqDuration := promauto.NewHistogramVec(prometheus.HistogramOpts{
		Name:    "obx_http_request_duration_seconds",
		Help:    "Request duration in seconds",
		Buckets: prometheus.DefBuckets,
	}, []string{"method", "path"})
	// response status counter
	respCounter := promauto.NewCounterVec(prometheus.CounterOpts{
		Name: "obx_http_responses_total",
		Help: "HTTP responses by status",
	}, []string{"status"})
	// rate limited counter
	rateLimited := promauto.NewCounter(prometheus.CounterOpts{
		Name: "obx_http_rate_limited_total",
		Help: "Number of requests rejected by rate limiter",
	})
	// expose the limiter counter to the package helper so limiter can increment it
	rateLimitedMetric = rateLimited
	// gauge for number of tracked limiter clients (via GaugeFunc)
	clientsGauge := prometheus.NewGaugeFunc(prometheus.GaugeOpts{
		Name: "obx_rate_limiter_clients",
		Help: "Number of IPs currently tracked by the rate limiter",
	}, func() float64 { return float64(limiter.ClientCount()) })
	prometheus.MustRegister(clientsGauge)

	// metrics middleware should be outermost (wraps logging) so it records
	// blocked requests as well.
	handler = metricsMiddleware(handler, reqCounter, reqDuration, respCounter)

	// apply configured timeouts (fallback to reasonable defaults)
	rt := cfg.ReadTimeout
	if rt == 0 {
		rt = 10 * time.Second
	}
	wt := cfg.WriteTimeout
	if wt == 0 {
		wt = 30 * time.Second
	}
	it := cfg.IdleTimeout
	if it == 0 {
		it = 120 * time.Second
	}

	// create admin server (localhost-only) for /metrics
	if cfg.AdminAddr == "" {
		cfg.AdminAddr = "127.0.0.1:9090"
	}
	adminMux := http.NewServeMux()
	adminMux.Handle("/metrics", promhttp.Handler())
	adminSrv := &http.Server{
		Addr:         cfg.AdminAddr,
		Handler:      adminMux,
		ReadTimeout:  rt,
		WriteTimeout: wt,
		IdleTimeout:  it,
	}

	adminErrCh := make(chan error, 1)
	go func() {
		if err := adminSrv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			adminErrCh <- err
		}
		close(adminErrCh)
	}()

	srv := &http.Server{
		Addr:         cfg.Addr,
		Handler:      handler,
		ReadTimeout:  rt,
		WriteTimeout: wt,
		IdleTimeout:  it,
	}

	// configure zerolog time format and start server
	zerolog.TimeFieldFormat = time.RFC3339
	zlog.Info().Str("addr", cfg.Addr).Msg("starting obx web server")
	errCh := make(chan error, 1)
	go func() {
		if err := srv.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			errCh <- err
		}
		close(errCh)
	}()

	// graceful shutdown on SIGINT/SIGTERM
	stop := make(chan os.Signal, 1)
	signal.Notify(stop, os.Interrupt, syscall.SIGTERM)
	select {
	case <-stop:
		zlog.Info().Msg("shutdown signal received, shutting down servers")
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		// shutdown main server
		if err := srv.Shutdown(ctx); err != nil {
			zlog.Error().Err(err).Msg("main server shutdown error")
		}
		// shutdown admin server
		if err := adminSrv.Shutdown(ctx); err != nil {
			zlog.Error().Err(err).Msg("admin server shutdown error")
		}
		zlog.Info().Msg("servers shutdown complete")
		return nil
	case err := <-errCh:
		if err != nil {
			return err
		}
		return nil
	case err := <-adminErrCh:
		if err != nil {
			return err
		}
		return nil
	}

}

// requestIDMiddleware injects a X-Request-ID header into each request if not
// already present. The ID is also set on the response for tracing across logs.
func requestIDMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		id := r.Header.Get("X-Request-ID")
		if id == "" {
			// generate 16-byte random hex id
			b := make([]byte, 16)
			if _, err := rand.Read(b); err == nil {
				id = hex.EncodeToString(b)
			} else {
				id = fmt.Sprintf("%d", time.Now().UnixNano())
			}
		}
		w.Header().Set("X-Request-ID", id)
		// also make it available to handlers via context and header
		r.Header.Set("X-Request-ID", id)
		next.ServeHTTP(w, r)
	})
}

// loggingMiddleware logs basic request information and response status.
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		// wrap ResponseWriter to capture status
		sr := &statusRecorder{ResponseWriter: w, status: http.StatusOK}
		next.ServeHTTP(sr, r)
		dur := time.Since(start)
		reqID := r.Header.Get("X-Request-ID")
		zlog.Info().
			Str("remote", r.RemoteAddr).
			Str("method", r.Method).
			Str("path", r.URL.Path).
			Int("status", sr.status).
			Dur("duration", dur).
			Str("request_id", reqID).
			Msg("http_request")
	})
}

// metricsMiddleware records Prometheus metrics for requests.
func metricsMiddleware(next http.Handler, reqCounter *prometheus.CounterVec, reqDuration *prometheus.HistogramVec, respCounter *prometheus.CounterVec) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		sr := &statusRecorder{ResponseWriter: w, status: http.StatusOK}
		next.ServeHTTP(sr, r)
		dur := time.Since(start).Seconds()
		path := r.URL.Path
		method := r.Method
		reqCounter.WithLabelValues(method, path).Inc()
		reqDuration.WithLabelValues(method, path).Observe(dur)
		respCounter.WithLabelValues(fmt.Sprintf("%d", sr.status)).Inc()
	})
}
