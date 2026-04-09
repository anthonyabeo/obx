package diag

// Package diag provides the core diagnostic types and the Emitter pipeline.
//
// Typical usage:
//
//	f   := emit.NewTextFormatter(srcMgr, 4)   // concrete Formatter
//	e   := diag.Stderr(f)                      // *Emitter bound to os.Stderr
//	rep := diag.NewBufferedReporter(srcMgr, 32, e)

import (
	"bytes"
	"fmt"
	"io"
	"net/http"
	"os"
)

// Formatter serialises a single Diagnostic into a byte slice.
// Implementations are immutable after construction and safe for concurrent use.
type Formatter interface {
	// Format renders d and returns the result as a UTF-8 byte slice,
	// including a trailing newline.
	Format(d Diagnostic) []byte

	// ContentType returns the MIME type produced by Format,
	// e.g. "text/plain; charset=utf-8" or "application/json".
	ContentType() string
}

// -----------------------------------------------------------------------
// Emitter
// -----------------------------------------------------------------------

// Emitter formats each Diagnostic with Formatter and writes the bytes to Writer.
// It is the single concrete output type; there is no DiagnosticSink interface.
type Emitter struct {
	Formatter Formatter
	Writer    io.Writer
}

// Emit formats d and writes it to the underlying Writer.
func (e *Emitter) Emit(d Diagnostic) {
	_, _ = e.Writer.Write(e.Formatter.Format(d))
}

// EmitAll emits every diagnostic in ds.  When limitReached is true it appends
// a final informational message reporting the cap.
func (e *Emitter) EmitAll(ds []Diagnostic, limitReached bool, maxErrors int) {
	for _, d := range ds {
		e.Emit(d)
	}
	if limitReached {
		e.Emit(Diagnostic{
			Severity: Info,
			Message:  fmt.Sprintf("stopped after %d errors", maxErrors),
		})
	}
}

// -----------------------------------------------------------------------
// Sink factory functions
// -----------------------------------------------------------------------

// ToWriter returns an Emitter that writes to any io.Writer.
func ToWriter(f Formatter, w io.Writer) *Emitter {
	return &Emitter{Formatter: f, Writer: w}
}

// Stdout returns an Emitter that writes to os.Stdout.
func Stdout(f Formatter) *Emitter {
	return &Emitter{Formatter: f, Writer: os.Stdout}
}

// Stderr returns an Emitter that writes to os.Stderr.
func Stderr(f Formatter) *Emitter {
	return &Emitter{Formatter: f, Writer: os.Stderr}
}

// ToFile returns an Emitter that appends to the named file (creating it if
// needed).  The caller owns the returned *os.File and must close it when done.
func ToFile(f Formatter, path string) (*Emitter, *os.File, error) {
	file, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o644)
	if err != nil {
		return nil, nil, err
	}
	return &Emitter{Formatter: f, Writer: file}, file, nil
}

// Network returns an Emitter that POSTs each formatted diagnostic to an HTTP
// endpoint.  Content-Type is taken from f.ContentType().
func Network(f Formatter, endpoint string, client *http.Client) *Emitter {
	return &Emitter{
		Formatter: f,
		Writer:    &httpWriter{endpoint: endpoint, client: client, ct: f.ContentType()},
	}
}

// httpWriter is an io.Writer that POSTs each Write call to an HTTP endpoint.
type httpWriter struct {
	endpoint string
	client   *http.Client
	ct       string
}

func (h *httpWriter) Write(p []byte) (int, error) {
	req, err := http.NewRequest(http.MethodPost, h.endpoint, bytes.NewReader(p))
	if err != nil {
		return 0, err
	}
	req.Header.Set("Content-Type", h.ct)
	h.client.Do(req) //nolint:errcheck
	return len(p), nil
}
