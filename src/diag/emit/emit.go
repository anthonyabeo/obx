// Package emit provides concrete DiagnosticSink implementations that write
// diagnostics to standard output, files, or a remote HTTP endpoint.
package emit

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"os"

	"github.com/anthonyabeo/obx/src/diag"
	"github.com/anthonyabeo/obx/src/diag/render"
	"github.com/anthonyabeo/obx/src/source"
)

// StdoutSink writes formatted diagnostics to an io.Writer (typically os.Stdout
// or os.Stderr).
type StdoutSink struct {
	Source   *source.Manager
	Writer   io.Writer // typically os.Stderr
	TabWidth int       // spaces per tab in source snippets; 0 → default 4
}

func (s StdoutSink) Emit(d diag.Diagnostic) {
	render.Diagnostic(s.Writer, s.Source, d, s.TabWidth)
}

// FileSink writes formatted diagnostics to an *os.File.
type FileSink struct {
	Source   *source.Manager
	File     *os.File
	TabWidth int // spaces per tab in source snippets; 0 → default 4
}

func (s *FileSink) Emit(d diag.Diagnostic) {
	render.Diagnostic(s.File, s.Source, d, s.TabWidth)
}

// JSONNetworkSink POSTs diagnostics as JSON to an HTTP endpoint.
type JSONNetworkSink struct {
	Endpoint string
	Client   *http.Client
}

func (s *JSONNetworkSink) Emit(d diag.Diagnostic) {
	payload, _ := json.Marshal(d)
	req, _ := http.NewRequest("POST", s.Endpoint, bytes.NewReader(payload))
	req.Header.Set("Content-Type", "application/json")
	s.Client.Do(req) //nolint:errcheck
}
