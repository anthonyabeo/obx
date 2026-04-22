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

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promauto"
	"github.com/prometheus/client_golang/prometheus/promhttp"
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

	// expose /metrics endpoint
	// register promhttp handler on the mux so it's protected by the same
	// middlewares (optionally change if you want it open)
	// NOTE: register directly on mux to avoid being instrumented twice
	mux.Handle("/metrics", promhttp.Handler())

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

// ipRateLimiter implements a simple token-bucket per-client IP limiter.
type ipRateLimiter struct {
	mu      sync.Mutex
	clients map[string]*bucket
	rate    float64
	burst   int
	// janitor controls
	janitorQuit chan struct{}
	janitorWg   sync.WaitGroup
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
	return &ipRateLimiter{clients: make(map[string]*bucket), rate: rate, burst: burst, janitorQuit: make(chan struct{})}
}

// ClientCount returns the number of client entries currently tracked.
func (l *ipRateLimiter) ClientCount() int {
	l.mu.Lock()
	defer l.mu.Unlock()
	return len(l.clients)
}

// StartJanitor launches a background goroutine that periodically purges
// client entries that have been idle for longer than idleTTL. It returns
// immediately; call StopJanitor to stop the background worker.
func (l *ipRateLimiter) StartJanitor(interval time.Duration, idleTTL time.Duration) {
	if interval <= 0 {
		interval = time.Minute
	}
	if idleTTL <= 0 {
		idleTTL = 5 * time.Minute
	}
	l.janitorWg.Add(1)
	go func() {
		defer l.janitorWg.Done()
		t := time.NewTicker(interval)
		defer t.Stop()
		for {
			select {
			case <-t.C:
				now := time.Now()
				l.mu.Lock()
				for ip, b := range l.clients {
					if now.Sub(b.last) > idleTTL {
						delete(l.clients, ip)
					}
				}
				l.mu.Unlock()
			case <-l.janitorQuit:
				return
			}
		}
	}()
}

// StopJanitor signals the janitor to stop and waits for it to exit.
func (l *ipRateLimiter) StopJanitor() {
	close(l.janitorQuit)
	l.janitorWg.Wait()
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
			// increment global metric if available
			incrementRateLimitedMetric()
			return
		}

		next.ServeHTTP(w, r)
	})
}
