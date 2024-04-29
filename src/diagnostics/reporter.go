package diagnostics

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ErrReporter interface {
	AddError(*token.Position, string)
	Errors() []*Error
	ClearErrors()
	ErrCount() int
	SetErrLimit(int)
	SetErrSeverityThreshold()
	OutputErrors()
	SetWarningPolicy()
}

type StdErrReporter struct {
	errors []*Error
	limit  int
}

// NewStdErrReporter creates and returns a new error reporter that outputs
// errors to standard error console. It can be customised by setting the
// maximum number of errors that can be collected.
func NewStdErrReporter(limit int) *StdErrReporter {
	if limit < 0 || limit > 128 {
		limit = 10
	}
	return &StdErrReporter{limit: limit}
}

// AddError creates and records a new compilation error.
func (s StdErrReporter) AddError(pos *token.Position, msg string) {
	if s.ErrCount() > s.limit {
		s.OutputErrors()
		panic("too many errors encountered. terminating the compilation process")
	}
	s.errors = append(s.errors, &Error{pos, msg})
}

// Errors returns the list of errors collected by the diagnostics reporter.
// It allows external components to access the errors for further
// processing or analysis.
func (s StdErrReporter) Errors() []*Error { return s.errors }

// ClearErrors clears the list of errors stored in the diagnostics reporter.
// It can be helpful when reusing the same diagnostics reporter instance for
// multiple compilations.
func (s StdErrReporter) ClearErrors() {
	//TODO implement me
	panic("implement me")
}

// ErrCount returns the total number of errors collected by the diagnostics
// reporter. It provides a convenient way to determine the severity of
// compilation issues.
func (s StdErrReporter) ErrCount() int { return len(s.errors) }

// SetErrLimit sets a limit on the number of errors reported by the diagnostics
// reporter. Once the limit is reached, the diagnostics reporter may stop collecting
// additional errors to prevent overwhelming the user with too many diagnostics messages.
func (s StdErrReporter) SetErrLimit(limit int) { s.limit = limit }

// SetErrSeverityThreshold allows users to specify a severity threshold for errors.
// Only errors with a severity level equal to or higher than the specified threshold
// will be reported. This can help users focus on critical issues while ignoring
// less severe warnings.
func (s StdErrReporter) SetErrSeverityThreshold() {
	//TODO implement me
	panic("implement me")
}

// OutputErrors logs errors to a file or external logging system in addition
// to (or instead of) printing them to the console. It allows for centralized
// diagnostics logging and analysis across multiple compilations.
func (s StdErrReporter) OutputErrors() {
	for _, err := range s.errors {
		fmt.Println(err.Error())
	}
}

// SetWarningPolicy allows users to customize the handling of warnings, such
// as whether to treat them as errors, ignore them, or log them separately.
func (s StdErrReporter) SetWarningPolicy() {
	//TODO implement me
	panic("implement me")
}
