package web

import (
	"crypto/rand"
	"embed"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"mime"
	"net/http"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"time"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

//go:embed static/*
var staticFS embed.FS

var filenameRE = regexp.MustCompile(`^[A-Za-z0-9._-]+$`)

// (monarch served via generic /static/ handler)

// HandleStatic serves embedded static files under the `static/` directory.
// It is a generic safe handler that cleans the requested path, prevents
// directory traversal, and sets an appropriate Content-Type based on the
// file extension (falling back to content-sniffing when necessary).
func (s *Server) HandleStatic(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet && r.Method != http.MethodHead {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Expect paths like /static/foo.js or /static/css/app.css
	// Trim the leading /static/ prefix and clean the path
	reqPath := r.URL.Path
	if strings.HasPrefix(reqPath, "/static/") {
		reqPath = strings.TrimPrefix(reqPath, "/static/")
	} else if strings.HasPrefix(reqPath, "/") {
		reqPath = strings.TrimPrefix(reqPath, "/")
	}

	// Clean and disallow traversal outside the static directory
	clean := path.Clean(reqPath)
	if clean == "." || clean == "" {
		http.NotFound(w, r)
		return
	}
	if strings.HasPrefix(clean, "../") || strings.Contains(clean, "../") {
		http.NotFound(w, r)
		return
	}

	embPath := path.Join("static", clean)
	data, err := staticFS.ReadFile(embPath)
	if err != nil {
		http.NotFound(w, r)
		return
	}

	// Determine MIME type: prefer extension-based detection, fall back to sniffing
	ext := filepath.Ext(clean)
	mimeType := mime.TypeByExtension(ext)
	if mimeType == "" {
		mimeType = http.DetectContentType(data)
	}
	// Ensure charset for text-like types where appropriate
	if strings.HasPrefix(mimeType, "text/") || strings.HasPrefix(mimeType, "application/javascript") || strings.HasPrefix(mimeType, "application/json") {
		if !strings.Contains(mimeType, "charset") {
			mimeType = mimeType + "; charset=utf-8"
		}
	}

	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.Header().Set("X-Frame-Options", "DENY")
	w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
	// Permissions-Policy (formerly Feature-Policy) - restrict powerful features
	w.Header().Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")

	// Content Security Policy: for index.html we generate a per-response nonce
	// and allow scripts from self, https:, and the generated nonce (no 'unsafe-inline').
	base := path.Base(clean)
	if base == "index.html" {
		// generate a per-response nonce and substitute into the HTML body below
		b := make([]byte, 16)
		if _, err := rand.Read(b); err != nil {
			b = []byte(fmt.Sprintf("%d", time.Now().UnixNano()))
		}
		nonce := base64.RawURLEncoding.EncodeToString(b)
		csp := fmt.Sprintf("default-src 'self'; script-src 'self' https: 'nonce-%s'; style-src 'self' 'unsafe-inline' https:; img-src 'self' data:; connect-src 'self' https:; font-src 'self' https: data:; frame-ancestors 'none';", nonce)
		w.Header().Set("Content-Security-Policy", csp)

		// Cache control for index
		w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")
		w.Header().Set("Pragma", "no-cache")
		w.Header().Set("Expires", "0")

		// Inject nonce into HTML placeholder and perform asset placeholder
		// substitution (replacing %VIZ_JS% etc with fingerprinted names).
		out := strings.ReplaceAll(string(data), "%CSP_NONCE%", nonce)
		out = replacePlaceholders(out)
		w.Header().Set("Content-Type", mimeType)
		w.Header().Set("Content-Length", fmt.Sprintf("%d", len(out)))
		if r.Method == http.MethodGet {
			_, _ = w.Write([]byte(out))
		}
		return
	}

	// Default CSP for other static assets (allow scripts from self and HTTPS)
	w.Header().Set("Content-Security-Policy", "default-src 'self'; script-src 'self' https:; style-src 'self' 'unsafe-inline' https:; img-src 'self' data:; connect-src 'self' https:; font-src 'self' https: data:; frame-ancestors 'none';")

	// Cache policy for non-index assets: relatively long-lived where appropriate
	w.Header().Set("Cache-Control", "public, max-age=31536000, immutable")
	w.Header().Set("Content-Type", mimeType)
	w.Header().Set("Content-Length", fmt.Sprintf("%d", len(data)))
	if r.Method == http.MethodGet {
		_, _ = w.Write(data)
	}
}

// ── GET / ────────────────────────────────────────────────────────────────────

func (s *Server) HandleUI(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	data, err := staticFS.ReadFile("static/index.html")
	if err != nil {
		http.Error(w, "could not read UI", http.StatusInternalServerError)
		return
	}
	// Generate per-response nonce and inject into HTML, then set strong CSP
	b := make([]byte, 16)
	if _, err := rand.Read(b); err != nil {
		b = []byte(fmt.Sprintf("%d", time.Now().UnixNano()))
	}
	nonce := base64.RawURLEncoding.EncodeToString(b)
	csp := fmt.Sprintf("default-src 'self'; script-src 'self' https: 'nonce-%s'; style-src 'self' 'unsafe-inline' https:; img-src 'self' data:; connect-src 'self' https:; font-src 'self' https: data:; frame-ancestors 'none';", nonce)
	w.Header().Set("Content-Security-Policy", csp)
	w.Header().Set("X-Content-Type-Options", "nosniff")
	w.Header().Set("X-Frame-Options", "DENY")
	w.Header().Set("Referrer-Policy", "strict-origin-when-cross-origin")
	w.Header().Set("Permissions-Policy", "geolocation=(), microphone=(), camera=()")
	w.Header().Set("Cache-Control", "no-cache, no-store, must-revalidate")

	out := strings.ReplaceAll(string(data), "%CSP_NONCE%", nonce)
	out = replacePlaceholders(out)
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	w.Header().Set("Content-Length", fmt.Sprintf("%d", len(out)))
	_, _ = w.Write([]byte(out))
}

// ── GET /api/version ─────────────────────────────────────────────────────────

func (s *Server) HandleVersion(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, http.StatusOK, map[string]string{
		"version": "dev",
		"go":      runtime.Version(),
		"os":      runtime.GOOS,
		"arch":    runtime.GOARCH,
	})
}

// ── POST /api/check ──────────────────────────────────────────────────────────

func (s *Server) HandleCheck(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		writeJSON(w, http.StatusMethodNotAllowed, map[string]any{"ok": false, "error": "method not allowed"})
		return
	}
	// limit request size early
	var req struct {
		Source   string `json:"source"`
		Filename string `json:"filename"`
		Entry    string `json:"entry"`
	}
	// determine effective limits (fallback to defaults if not set)
	mb := s.cfg.MaxBodyBytes
	if mb == 0 {
		mb = 256 * 1024
	}
	ms := s.cfg.MaxSourceBytes
	if ms == 0 {
		ms = 200 * 1024
	}
	mf := s.cfg.MaxFilenameLen
	if mf == 0 {
		mf = 128
	}

	dec := json.NewDecoder(http.MaxBytesReader(w, r.Body, int64(mb)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&req); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "bad request: " + err.Error()})
		return
	}
	if req.Filename == "" {
		req.Filename = "Main.obx"
	}

	// validate filename: must be a simple basename with allowed chars
	if len(req.Filename) == 0 || len(req.Filename) > mf || !filenameRE.MatchString(req.Filename) || filepath.Base(req.Filename) != req.Filename {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "invalid filename"})
		return
	}
	// validate source size
	if len(req.Source) == 0 {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "empty source"})
		return
	}
	if len(req.Source) > ms {
		writeJSON(w, http.StatusRequestEntityTooLarge, map[string]any{"ok": false, "error": "source too large"})
		return
	}

	// ── in-memory pipeline ────────────────────────────────────────────────
	srcMgr := source.NewSourceManager()
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)
	ctx := compiler.New(req.Filename, srcMgr, reporter, ast.NewEnv(), 0)

	injectHostPlatformDirectives(ctx)

	obx := ast.NewOberonX()

	// parse stdlib modules first (best-effort; skip if unavailable)
	entry := deriveEntryFromFilename(req.Filename)
	if err := prepareStdlibUnits(ctx, obx, entry, req.Filename, req.Source); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": err.Error()})
		return
	}

	// sema — only when parsing succeeded
	if reporter.ErrorCount() == 0 {
		sema.NewSema(ctx, obx).Validate()
	}

	// ── response ──────────────────────────────────────────────────────────
	type location struct {
		File      string `json:"file"`
		StartLine int    `json:"start_line"`
		StartCol  int    `json:"start_col"`
		EndLine   int    `json:"end_line"`
		EndCol    int    `json:"end_col"`
	}
	type diagItem struct {
		Severity string    `json:"severity"`
		Message  string    `json:"message"`
		Location *location `json:"location,omitempty"`
	}

	// Filter to only diagnostics from the user's file (not stdlib internals).
	raw := reporter.Diagnostics()
	items := make([]diagItem, 0, len(raw))
	userErrors := 0
	for _, d := range raw {
		// Skip diagnostics from stdlib files (not the user's submitted file).
		if d.Range != nil && d.Range.Start.File != req.Filename {
			continue
		}
		item := diagItem{Severity: d.Severity.String(), Message: d.Message}
		if d.Range != nil {
			item.Location = &location{
				File:      d.Range.Start.File,
				StartLine: d.Range.Start.Line,
				StartCol:  d.Range.Start.Column,
				EndLine:   d.Range.End.Line,
				EndCol:    d.Range.End.Column,
			}
		}
		items = append(items, item)
		if d.Severity == diag.Error {
			userErrors++
		}
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"ok":          userErrors == 0,
		"error_count": userErrors,
		"diagnostics": items,
	})
}

// ── POST /api/cfg ─────────────────────────────────────────────────────────────

// HandleCFG runs the full front-end pipeline (parse → sema → desugar → ObxIR)
// and returns a Graphviz DOT string for each function's CFG.
func (s *Server) HandleCFG(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		writeJSON(w, http.StatusMethodNotAllowed, map[string]any{"ok": false, "error": "method not allowed"})
		return
	}
	var req struct {
		Source   string `json:"source"`
		Filename string `json:"filename"`
		Entry    string `json:"entry"`
	}
	// determine effective limits (fallback to defaults if not set)
	mb := s.cfg.MaxBodyBytes
	if mb == 0 {
		mb = 256 * 1024
	}
	ms := s.cfg.MaxSourceBytes
	if ms == 0 {
		ms = 200 * 1024
	}
	mf := s.cfg.MaxFilenameLen
	if mf == 0 {
		mf = 128
	}

	dec := json.NewDecoder(http.MaxBytesReader(w, r.Body, int64(mb)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&req); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "bad request: " + err.Error()})
		return
	}
	if req.Filename == "" {
		req.Filename = "Main.obx"
	}

	// validate filename and source length similar to HandleCheck
	if len(req.Filename) == 0 || len(req.Filename) > mf || !filenameRE.MatchString(req.Filename) || filepath.Base(req.Filename) != req.Filename {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "invalid filename"})
		return
	}
	if len(req.Source) == 0 {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "empty source"})
		return
	}
	if len(req.Source) > ms {
		writeJSON(w, http.StatusRequestEntityTooLarge, map[string]any{"ok": false, "error": "source too large"})
		return
	}

	// ── in-memory pipeline ────────────────────────────────────────────────
	srcMgr := source.NewSourceManager()
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)
	ctx := compiler.New(req.Filename, srcMgr, reporter, ast.NewEnv(), 8)

	injectHostPlatformDirectives(ctx)

	obx := ast.NewOberonX()
	entry := deriveEntryFromFilename(req.Filename)
	if err := prepareStdlibUnits(ctx, obx, entry, req.Filename, req.Source); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": err.Error()})
		return
	}

	if reporter.ErrorCount() > 0 {
		writeJSON(w, http.StatusOK, map[string]any{
			"ok":    false,
			"error": "parse errors — fix diagnostics first",
		})
		return
	}

	// sema — run on all units (stdlib + user) so cross-module types resolve.
	sema.NewSema(ctx, obx).Validate()

	// Count only errors in the user's file, not stdlib internals.
	userSemaErrors := 0
	for _, d := range reporter.Diagnostics() {
		if d.Severity == diag.Error && (d.Range == nil || d.Range.Start.File == req.Filename) {
			userSemaErrors++
		}
	}
	if userSemaErrors > 0 {
		writeJSON(w, http.StatusOK, map[string]any{
			"ok":    false,
			"error": "type errors — fix diagnostics first",
		})
		return
	}

	// desugar → ObxIR → build CFG
	hirProgram := desugar.NewGenerator(obx, ctx).Generate()
	irProgram := obxir.NewIRBuilder(ctx.Target.WordSize).Build(hirProgram, ctx)

	type graphEntry struct {
		Module   string `json:"module"`
		Function string `json:"function"`
		Dot      string `json:"dot"`
	}

	var graphs []graphEntry
	for _, mod := range irProgram.Modules {
		for _, fn := range mod.Funcs {
			if fn.IsExternal {
				continue
			}
			opt.BuildCFG(fn)
			graphs = append(graphs, graphEntry{
				Module:   mod.Name,
				Function: fn.FnName,
				Dot:      fn.OutputDOT(),
			})
		}
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"ok":     true,
		"graphs": graphs,
	})
}

// HandleRun executes the user's program (MVP placeholder). Returns diagnostics
// and a textual output. Real execution/sandboxing is planned for later.
func (s *Server) HandleRun(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		writeJSON(w, http.StatusMethodNotAllowed, map[string]any{"ok": false, "error": "method not allowed"})
		return
	}
	var req struct {
		Source   string `json:"source"`
		Filename string `json:"filename"`
	}
	// determine effective limits (fallback to defaults if not set)
	mb := s.cfg.MaxBodyBytes
	if mb == 0 {
		mb = 256 * 1024
	}
	ms := s.cfg.MaxSourceBytes
	if ms == 0 {
		ms = 200 * 1024
	}
	mf := s.cfg.MaxFilenameLen
	if mf == 0 {
		mf = 128
	}

	dec := json.NewDecoder(http.MaxBytesReader(w, r.Body, int64(mb)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&req); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "bad request: " + err.Error()})
		return
	}
	if req.Filename == "" {
		req.Filename = "Main.obx"
	}

	if len(req.Filename) == 0 || len(req.Filename) > mf || !filenameRE.MatchString(req.Filename) || filepath.Base(req.Filename) != req.Filename {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "invalid filename"})
		return
	}
	if len(req.Source) == 0 {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": "empty source"})
		return
	}
	if len(req.Source) > ms {
		writeJSON(w, http.StatusRequestEntityTooLarge, map[string]any{"ok": false, "error": "source too large"})
		return
	}

	// build context and run the front-end to collect diagnostics
	srcMgr := source.NewSourceManager()
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)
	ctx := compiler.New(req.Filename, srcMgr, reporter, ast.NewEnv(), 0)

	injectHostPlatformDirectives(ctx)

	obx := ast.NewOberonX()
	entry := deriveEntryFromFilename(req.Filename)
	if err := prepareStdlibUnits(ctx, obx, entry, req.Filename, req.Source); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": err.Error()})
		return
	}

	if reporter.ErrorCount() == 0 {
		// run semantic checks
		sema.NewSema(ctx, obx).Validate()
	}

	// Collect diagnostics (only user's file)
	type location struct {
		File      string `json:"file"`
		StartLine int    `json:"start_line"`
		StartCol  int    `json:"start_col"`
		EndLine   int    `json:"end_line"`
		EndCol    int    `json:"end_col"`
	}
	type diagItem struct {
		Severity string    `json:"severity"`
		Message  string    `json:"message"`
		Location *location `json:"location,omitempty"`
	}

	raw := reporter.Diagnostics()
	items := make([]diagItem, 0, len(raw))
	userErrors := 0
	for _, d := range raw {
		if d.Range != nil && d.Range.Start.File != req.Filename {
			continue
		}
		item := diagItem{Severity: d.Severity.String(), Message: d.Message}
		if d.Range != nil {
			item.Location = &location{
				File:      d.Range.Start.File,
				StartLine: d.Range.Start.Line,
				StartCol:  d.Range.Start.Column,
				EndLine:   d.Range.End.Line,
				EndCol:    d.Range.End.Column,
			}
		}
		items = append(items, item)
		if d.Severity == diag.Error {
			userErrors++
		}
	}

	// Placeholder output — real execution will be implemented later.
	output := "[run] execution not implemented in playground yet\n"

	writeJSON(w, http.StatusOK, map[string]any{
		"ok":          userErrors == 0,
		"error_count": userErrors,
		"diagnostics": items,
		"output":      output,
	})
}
