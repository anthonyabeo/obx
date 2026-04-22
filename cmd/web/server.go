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
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync"
	"syscall"
	"time"
)

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

	// logging should be outermost so it records blocked requests too
	handler = loggingMiddleware(handler)

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

	srv := &http.Server{
		Addr:         cfg.Addr,
		Handler:      handler,
		ReadTimeout:  rt,
		WriteTimeout: wt,
		IdleTimeout:  it,
	}

	// start server
	fmt.Printf("obx web  →  http://%s\n", cfg.Addr)
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
		log.Printf("shutdown signal received, shutting down server")
		ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
		defer cancel()
		if err := srv.Shutdown(ctx); err != nil {
			return fmt.Errorf("server shutdown: %w", err)
		}
		return nil
	case err := <-errCh:
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
		log.Printf("%s %s %s %d %s request_id=%s", r.RemoteAddr, r.Method, r.URL.Path, sr.status, dur, reqID)
	})
}

// statusRecorder wraps http.ResponseWriter to capture the response status code.
type statusRecorder struct {
	http.ResponseWriter
	status int
}

func (r *statusRecorder) WriteHeader(code int) {
	r.status = code
	r.ResponseWriter.WriteHeader(code)
}

// ipRateLimiter implements a simple token-bucket per-client IP limiter.
type ipRateLimiter struct {
	mu      sync.Mutex
	clients map[string]*bucket
	rate    float64
	burst   int
}

type bucket struct {
	tokens float64
	last   time.Time
}

func newIPRateLimiter(rate float64, burst int) *ipRateLimiter {
	if rate <= 0 {
		rate = 5.0
	}
	if burst <= 0 {
		burst = 10
	}
	return &ipRateLimiter{clients: make(map[string]*bucket), rate: rate, burst: burst}
}

// Middleware returns an http.Handler wrapper enforcing per-IP rate limits.
func (l *ipRateLimiter) Middleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// determine client IP (respect X-Forwarded-For if present)
		ip := r.Header.Get("X-Forwarded-For")
		if ip != "" {
			// may contain comma-separated list
			parts := strings.Split(ip, ",")
			ip = strings.TrimSpace(parts[0])
		} else {
			host, _, err := net.SplitHostPort(r.RemoteAddr)
			if err != nil {
				ip = r.RemoteAddr
			} else {
				ip = host
			}
		}

		now := time.Now()
		l.mu.Lock()
		b, ok := l.clients[ip]
		if !ok {
			b = &bucket{tokens: float64(l.burst), last: now}
			l.clients[ip] = b
		}
		// refill tokens
		elapsed := now.Sub(b.last).Seconds()
		b.tokens += elapsed * l.rate
		if b.tokens > float64(l.burst) {
			b.tokens = float64(l.burst)
		}

		allowed := false
		if b.tokens >= 1.0 {
			b.tokens -= 1.0
			allowed = true
		}
		b.last = now
		l.mu.Unlock()

		if !allowed {
			// Rate limited
			w.Header().Set("Retry-After", "1")
			http.Error(w, "too many requests", http.StatusTooManyRequests)
			return
		}

		next.ServeHTTP(w, r)
	})
}

