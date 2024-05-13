package diagnostics

import (
	"os"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type FileLogReporter struct {
	file   *os.File
	limit  int
	errors []*Error
}

func NewFileLogReporter(file *os.File, limit int) *FileLogReporter {
	if limit < 0 || limit > 128 {
		limit = 10
	}
	return &FileLogReporter{file: file, limit: limit}
}

func (f *FileLogReporter) AddError(pos *token.Position, msg string) {
	if f.ErrCount() > f.limit {
		f.OutputErrors()
		panic("too many errors encountered. terminating the compilation process")
	}
	f.errors = append(f.errors, &Error{pos, msg})
}

func (f *FileLogReporter) Errors() []*Error { return f.errors }

func (f *FileLogReporter) ClearErrors() {}

func (f *FileLogReporter) ErrCount() int { return len(f.errors) }

func (f *FileLogReporter) SetErrLimit(limit int) { f.limit = limit }

func (f *FileLogReporter) SetErrSeverityThreshold() {
	//TODO implement me
	panic("implement me")
}

func (f *FileLogReporter) OutputErrors() {
	//TODO implement me
	panic("implement me")
}

func (f *FileLogReporter) SetWarningPolicy() {
	//TODO implement me
	panic("implement me")
}
