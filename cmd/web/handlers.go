package web

import (
	"embed"
	"encoding/json"
	"io"
	"log"
	"net/http"
	"runtime"

	"github.com/anthonyabeo/obx/src/support/adt"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
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
	}
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "bad request: "+err.Error(), http.StatusBadRequest)
		return
	}
	if req.Filename == "" {
		req.Filename = "input.obx"
	}

	// ── in-memory pipeline (no file discovery) ────────────────────────────
	srcMgr := source.NewSourceManager()

	// Diagnostics are buffered and read back directly; nothing needs to be
	// written by the emitter, so io.Discard is the right destination.
	emitter := diag.ToWriter(formatter.NewJSONFormatter(srcMgr), io.Discard)
	reporter := diag.NewBufferedReporter(srcMgr, s.cfg.MaxErrors, emitter)

	ctx := &diag.Context{
		FileName:              req.Filename,
		Content:               []byte(req.Source),
		Source:                srcMgr,
		Reporter:              reporter,
		Env:                   ast.NewEnv(),
		Names:                 adt.NewStack[string](),
		ExprLists:             adt.NewStack[[]ast.Expression](),
		TargetMachineWordSize: 8,
	}

	// parse
	p := parser.NewParser(ctx)
	unit := p.Parse()

	// sema — only when parsing succeeded
	if reporter.ErrorCount() == 0 {
		obx := ast.NewOberonX()
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

	raw := reporter.Diagnostics()
	items := make([]diagItem, 0, len(raw))
	for _, d := range raw {
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
	}

	writeJSON(w, http.StatusOK, map[string]any{
		"ok":          reporter.ErrorCount() == 0,
		"error_count": reporter.ErrorCount(),
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

// ── shared helper ─────────────────────────────────────────────────────────────

func writeJSON(w http.ResponseWriter, status int, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	if err := json.NewEncoder(w).Encode(v); err != nil {
		log.Printf("writeJSON: %v", err)
	}
}
