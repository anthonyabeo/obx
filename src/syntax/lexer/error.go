package lexer

import "github.com/anthonyabeo/obx/src/syntax/token"

type Error struct {
	Pos *token.Position
	Msg string
}

// Error implements the error interface.
func (e Error) Error() string {
	if e.Pos.Filename != "" {
		return e.Pos.String() + ": " + e.Msg
	}

	return e.Msg
}

type ErrorList []*Error

// Append creates and adds a new error to the ErrorList
func (p *ErrorList) Append(pos *token.Position, msg string) {
	*p = append(*p, &Error{pos, msg})
}
