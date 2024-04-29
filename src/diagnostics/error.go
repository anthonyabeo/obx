package diagnostics

import "github.com/anthonyabeo/obx/src/syntax/token"

type Error struct {
	Pos *token.Position
	Msg string
}

// Error implements the diagnostics interface.
func (e Error) Error() string {
	if e.Pos.Filename != "" {
		return e.Pos.String() + ": " + e.Msg
	}

	return e.Msg
}
