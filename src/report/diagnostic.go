package report

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

type Position struct {
	File   string
	Offset int // byte offset in file
	Line   int // 1-based
	Column int
}

type Diagnostic struct {
	Severity Severity
	Message  string
	Pos      Position // still the primary point for caret (`^`)
	Range    *Range   // optional span
	LineText string   // optional override (for single-line messages)
}

type Range struct {
	Start Position
	End   Position
}
