package web

import (
	"net"
	"net/http"
	"strings"
	"sync"
	"time"
)

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
			// For API endpoints return JSON consistent error; otherwise plain text
			if strings.HasPrefix(r.URL.Path, "/api/") {
				writeJSON(w, http.StatusTooManyRequests, map[string]any{"ok": false, "error": "too many requests"})
			} else {
				http.Error(w, "too many requests", http.StatusTooManyRequests)
			}
			// increment global metric if available
			incrementRateLimitedMetric()
			return
		}

		next.ServeHTTP(w, r)
	})
}
