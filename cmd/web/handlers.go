package web

import (
	"embed"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"runtime"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

//go:embed static/index.html
var staticFS embed.FS

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
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_, _ = w.Write(data)
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
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req struct {
		Source   string `json:"source"`
		Filename string `json:"filename"`
		Entry    string `json:"entry"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "bad request: "+err.Error(), http.StatusBadRequest)
		return
	}
	if req.Filename == "" {
		req.Filename = "input.obx"
	}

	// ── in-memory pipeline ────────────────────────────────────────────────
	srcMgr := source.NewSourceManager()
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)
	ctx := compiler.New(req.Filename, srcMgr, reporter, ast.NewEnv(), 0)

	injectHostPlatformDirectives(ctx)

	obx := ast.NewOberonX()

	// parse stdlib modules first (best-effort; skip if unavailable)
	if err := loadStdlibInto(ctx, obx, req.Entry, req.Filename, req.Source); err != nil {
		http.Error(w, "bad request: "+err.Error(), http.StatusBadRequest)
		return
	}

	// parse the user's source
	p := parser.NewParser(ctx, req.Filename, []byte(req.Source))
	unit := p.Parse()

	// sema — only when parsing succeeded
	if reporter.ErrorCount() == 0 {
		obx.AddUnit(unit)
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

// ── CORS middleware ───────────────────────────────────────────────────────────

// corsMiddleware adds permissive CORS headers to every response so the API
// can be consumed from external editors, scripts, and CI tooling without a
// proxy.  OPTIONS pre-flight requests are short-circuited with 204.
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Accept")
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// ── POST /api/cfg ─────────────────────────────────────────────────────────────

// HandleCFG runs the full front-end pipeline (parse → sema → desugar → ObxIR)
// and returns a Graphviz DOT string for each function's CFG.
func (s *Server) HandleCFG(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "method not allowed", http.StatusMethodNotAllowed)
		return
	}

	var req struct {
		Source   string `json:"source"`
		Filename string `json:"filename"`
		Entry    string `json:"entry"`
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "bad request: "+err.Error(), http.StatusBadRequest)
		return
	}
	if req.Filename == "" {
		req.Filename = "input.obx"
	}

	// ── in-memory pipeline ────────────────────────────────────────────────
	srcMgr := source.NewSourceManager()
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)
	ctx := compiler.New(req.Filename, srcMgr, reporter, ast.NewEnv(), 8)

	injectHostPlatformDirectives(ctx)

	obx := ast.NewOberonX()
	if err := loadStdlibInto(ctx, obx, req.Entry, req.Filename, req.Source); err != nil {
		writeJSON(w, http.StatusBadRequest, map[string]any{"ok": false, "error": err.Error()})
		return
	}

	// parse
	p := parser.NewParser(ctx, req.Filename, []byte(req.Source))
	unit := p.Parse()

	if reporter.ErrorCount() > 0 {
		writeJSON(w, http.StatusOK, map[string]any{
			"ok":    false,
			"error": "parse errors — fix diagnostics first",
		})
		return
	}

	// sema — run on all units (stdlib + user) so cross-module types resolve.
	obx.AddUnit(unit)
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
	hirProgram := desugar.NewGenerator(obx).Generate()
	irProgram := obxir.NewIRBuilder(8).Build(hirProgram)

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

// ── shared helpers ────────────────────────────────────────────────────────────

// injectHostPlatformDirectives sets POSIX/WINDOWS/DARWIN/LINUX based on the
// OS the server is currently running on.
func injectHostPlatformDirectives(ctx *compiler.Context) {
	switch runtime.GOOS {
	case "windows":
		ctx.SetDirective("WINDOWS", true)
		ctx.SetDirective("POSIX", false)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("DARWIN", false)
	case "darwin":
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("DARWIN", true)
		ctx.SetDirective("LINUX", false)
		ctx.SetDirective("WINDOWS", false)
	default:
		ctx.SetDirective("POSIX", true)
		ctx.SetDirective("LINUX", true)
		ctx.SetDirective("DARWIN", false)
		ctx.SetDirective("WINDOWS", false)
	}
}

// loadStdlibInto discovers stdlib modules (via OBX_STDLIB or the executable's
// adjacent stdlib/ directory) and parses them into obx in dependency order.
// Errors are silently ignored so the server keeps working without stdlib.
func loadStdlibInto(ctx *compiler.Context, obx *ast.OberonX, entry, userFilename, userSource string) error {
	stdlibRoot := project.ResolveStdlibRoot(project.Manifest{})
	if stdlibRoot == "" {
		// Try a path relative to the current working directory as a fallback
		// (useful when running `go run` or tests directly from the repo).
		if wd, err := os.Getwd(); err == nil {
			for dir := wd; ; dir = filepath.Dir(dir) {
				candidate := filepath.Join(dir, "stdlib")
				if info, err := os.Stat(candidate); err == nil && info.IsDir() {
					stdlibRoot = candidate
					break
				}
				parent := filepath.Dir(dir)
				if parent == dir {
					break
				}
			}
		}
	}

	var roots []string
	var tmpDir string
	if userSource != "" {
		td, err := os.MkdirTemp("", "obx-web-*")
		if err != nil {
			return fmt.Errorf("create temp dir: %w", err)
		}
		tmpDir = td
		// Ensure cleanup
		defer func() {
			if err := os.RemoveAll(tmpDir); err != nil {
				log.Printf("failed to remove temp dir %s: %v", tmpDir, err)
			}
		}()

		if filepath.Ext(userFilename) != ".obx" {
			userFilename = userFilename + ".obx"
		}
		target := filepath.Join(tmpDir, userFilename)
		if err := os.WriteFile(target, []byte(userSource), 0644); err != nil {
			return fmt.Errorf("write temp file: %w", err)
		}

		// Place the temp root before stdlib so uploaded files override stdlib.
		roots = append(roots, tmpDir)
	}

	if stdlibRoot != "" {
		roots = append(roots, stdlibRoot)
	}

	if len(roots) == 0 {
		// Nothing to discover.
		return nil
	}

	r := project.NewResolver(roots...)

	// Use the server's compiler context to supply platform directive flags so
	// header scanning selects the correct conditional branches.
	headers, err := r.DiscoverAllWithResolver(directive.ResolverFromContext(ctx))
	if err != nil {
		return fmt.Errorf("stdlib discover: %w", err)
	}
	graph, err := project.BuildImportGraph(headers)
	if err != nil {
		return fmt.Errorf("stdlib import graph: %w", err)
	}
	sorted, err := project.TopoSort(graph)
	if err != nil {
		return fmt.Errorf("stdlib topo sort: %w", err)
	}

	// Apply entry filtering (full module key) using the project-level helper.
	sorted, err = project.ReachableFrom(sorted, graph, entry)
	if err != nil {
		return err
	}

	// Print sorted stdlib file list for debugging/visibility.
	for i, hdr := range sorted {
		log.Printf("web: stdlib sorted[%d]: %s", i, filepath.Base(hdr.File))
	}

	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			continue
		}
		fileName := filepath.Base(header.File)
		content := data[header.StartPos:header.EndPos]
		p := parser.NewParser(ctx, fileName, content)
		// Snapshot diagnostics count so we can detect parse errors produced by
		// this file and avoid adding a partially-parsed unit into the global
		// OberonX used for later lowering.
		before := len(ctx.Reporter.Diagnostics())
		unit := p.Parse()
		after := len(ctx.Reporter.Diagnostics())
		if after > before {
			// Skip adding this unit due to parse errors in the stdlib file.
			continue
		}
		if unit != nil {
			obx.AddUnit(unit)
		}
	}

	// Discard any parse errors from stdlib so they don't pollute the
	// user's diagnostic output.
	if r, ok := ctx.Reporter.(*diag.BufferedReporter); ok {
		r.Reset()
	}
	return nil
}

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(v); err != nil {
		log.Printf("writeJSON: %v", err)
	}
}
