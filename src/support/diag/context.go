package diag

import "github.com/anthonyabeo/obx/src/support/source"

// Context carries the minimal diagnostic infrastructure that every compiler
// phase needs to record and locate diagnostics.  It deliberately contains no
// language-specific types (AST nodes, type system) so the diag package remains
// a low-level leaf with no upward imports.
type Context struct {
	FileName string
	Source   *source.Manager
	Reporter Reporter
}
