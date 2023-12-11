package parser

type Error struct {
	Msg string
}

// Error implements the error interface.
func (e Error) Error() string {
	return e.Msg
}

type ErrorList []*Error

// Append creates and adds a new error to the ErrorList
func (p *ErrorList) Append(msg string) {
	*p = append(*p, &Error{msg})
}
