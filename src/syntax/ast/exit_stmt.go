package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type ExitStmt struct {
	Exit *token.Position
}

func (stmt *ExitStmt) stmt()                {}
func (stmt *ExitStmt) Pos() *token.Position { return stmt.Exit }
func (stmt *ExitStmt) End() *token.Position { panic("not implemented") }
func (stmt *ExitStmt) String() string       { panic("not implemented") }
func (stmt *ExitStmt) Accept(vst Visitor)   { vst.VisitExitStmt(stmt) }
