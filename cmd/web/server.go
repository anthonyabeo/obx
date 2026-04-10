// Package web implements the obx HTTP server: a browser-based Oberon+ editor
// (GET /) backed by a JSON API (POST /api/check, GET /api/version).
//
// CORS is enabled on every route so the API can be consumed by external
// editors, scripts, and CI tooling without proxy configuration.
package web

import (
	"fmt"
	"net/http"
)

// Config holds all runtime parameters for the web server.
type Config struct {
	Addr      string // host:port to listen on, e.g. ":8080"
	TabWidth  int    // spaces per tab in rendered diagnostic snippets
	MaxErrors int    // max errors before the pipeline stops
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
	mux.HandleFunc("/api/check", s.HandleCheck)
	mux.HandleFunc("/api/version", s.HandleVersion)

	fmt.Printf("obx web  →  http://%s\n", cfg.Addr)
	return http.ListenAndServe(cfg.Addr, corsMiddleware(mux))
}
