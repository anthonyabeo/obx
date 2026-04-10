package diag

import "github.com/anthonyabeo/obx/src/support/source"

type Severity int

const (
	Info Severity = iota
	Warning
	Error
	Fatal
	Debug
)

func (s Severity) String() string {
	switch s {
	case Info:
		return "info"
	case Warning:
		return "warning"
	case Error:
		return "error"
	case Fatal:
		return "fatal"
	case Debug:
		return "debug"
	default:
		return "unknown"
	}
}

type Diagnostic struct {
	Severity Severity
	Message  string
	Range    *source.Range // optional span
}
