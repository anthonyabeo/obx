package diagnostics

import "github.com/anthonyabeo/obx/src/syntax/token"

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
