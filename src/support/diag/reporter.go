package diag

import "github.com/anthonyabeo/obx/src/support/source"

// Reporter is the interface through which the compiler records diagnostics.
type Reporter interface {
	Report(Diagnostic)
	Flush()
	ErrorCount() int
	LimitReached() bool
	Diagnostics() []Diagnostic
}

// BufferedReporter collects diagnostics and flushes them to an Emitter on demand.
type BufferedReporter struct {
	Source      *source.Manager
	MaxErrors   int
	diagnostics []Diagnostic
	errorCount  int
	emitter     *Emitter
}

// NewBufferedReporter creates a reporter that flushes to e.
func NewBufferedReporter(src *source.Manager, maxErrors int, e *Emitter) *BufferedReporter {
	return &BufferedReporter{
		Source:    src,
		MaxErrors: maxErrors,
		emitter:   e,
	}
}

func (r *BufferedReporter) Diagnostics() []Diagnostic { return r.diagnostics }

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
	r.emitter.EmitAll(r.diagnostics, r.LimitReached(), r.MaxErrors)
	r.diagnostics = nil
	r.errorCount = 0
}

func (r *BufferedReporter) ErrorCount() int    { return r.errorCount }
func (r *BufferedReporter) LimitReached() bool { return r.MaxErrors > 0 && r.errorCount >= r.MaxErrors }

// Reset clears all accumulated diagnostics and resets the error counter.
// Useful when preloading stdlib so its errors don't appear in user output.
func (r *BufferedReporter) Reset() {
	r.diagnostics = nil
	r.errorCount = 0
}
