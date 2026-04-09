package diag

// DiagnosticSink is the output target for emitted diagnostics.
// Concrete implementations live in the emit sub-package.
type DiagnosticSink interface {
	Emit(d Diagnostic)
}
