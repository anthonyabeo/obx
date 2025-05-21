package report

import (
	"fmt"
)

type Reporter interface {
	Report(Diagnostic)
	Flush()
	ErrorCount() int
	LimitReached() bool
}

type BufferedReporter struct {
	Source      *SourceManager
	MaxErrors   int
	diagnostics []Diagnostic
	errorCount  int
	sink        DiagnosticSink
}

func NewBufferedReporter(source *SourceManager, maxErrors int, sink DiagnosticSink) *BufferedReporter {
	return &BufferedReporter{
		Source:    source,
		MaxErrors: maxErrors,
		sink:      sink,
	}
}

func (r *BufferedReporter) Report(d Diagnostic) {
	if d.Severity == Error {
		if r.errorCount >= r.MaxErrors {
			return
		}
		r.errorCount++
	}
	r.diagnostics = append(r.diagnostics, d)
}

func (r *BufferedReporter) Flush() {
	for _, d := range r.diagnostics {
		r.sink.Emit(d)
	}

	if r.LimitReached() {
		r.sink.Emit(Diagnostic{
			Severity: Info,
			Message:  fmt.Sprintf("stopped after %d errors", r.MaxErrors),
		})
	}

	r.diagnostics = nil
	r.errorCount = 0
}

func (r *BufferedReporter) ErrorCount() int {
	return r.errorCount
}

func (r *BufferedReporter) LimitReached() bool {
	return r.MaxErrors > 0 && r.errorCount >= r.MaxErrors
}
