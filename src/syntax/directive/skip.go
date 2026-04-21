package directive

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// DNext returns the next non-NEWLINE token from the supplier.
func DNext(s TokenSupplier) token.Token {
	for {
		t := s.NextToken()
		if t.Kind == token.NEWLINE {
			continue
		}
		return t
	}
}

// ConsumeDirectiveEnd consumes tokens until DIRECTIVE_END or EOF.
func ConsumeDirectiveEnd(s TokenSupplier) {
	for {
		t := s.NextToken()
		if t.Kind == token.DIRECTIVE_END || t.Kind == token.EOF {
			return
		}
	}
}

// SkipToNextBranch skips tokens until the next branch delimiter at depth 0.
// Returns the kind of the branch found and the token that represented it
// (e.g. the ELSIF token). On EOF reports a diagnostic and returns token.EOF.
func SkipToNextBranch(s TokenSupplier) (token.Kind, token.Token) {
	depth := 0
	for {
		t := s.NextToken()
		if t.Kind == token.EOF {
			return token.EOF, t
		}
		if t.Kind != token.DIRECTIVE_START {
			continue
		}
		kw := s.NextToken()
		switch kw.Kind {
		case token.IF:
			depth++
			ConsumeDirectiveEnd(s)
		case token.END:
			if depth == 0 {
				ConsumeDirectiveEnd(s)
				return token.END, kw
			}
			depth--
			ConsumeDirectiveEnd(s)
		case token.ELSIF:
			if depth == 0 {
				return token.ELSIF, kw
			}
			ConsumeDirectiveEnd(s)
		case token.ELSE:
			if depth == 0 {
				ConsumeDirectiveEnd(s)
				return token.ELSE, kw
			}
			ConsumeDirectiveEnd(s)
		default:
			ConsumeDirectiveEnd(s)
		}
	}
}

// SkipAllRemainingArms skips until matching <* END *> at depth 0, consuming it.
func SkipAllRemainingArms(s TokenSupplier) {
	depth := 0
	for {
		t := s.NextToken()
		if t.Kind == token.EOF {
			return
		}
		if t.Kind != token.DIRECTIVE_START {
			continue
		}
		kw := s.NextToken()
		switch kw.Kind {
		case token.IF:
			depth++
			ConsumeDirectiveEnd(s)
		case token.END:
			if depth == 0 {
				ConsumeDirectiveEnd(s)
				return
			}
			depth--
			ConsumeDirectiveEnd(s)
		default:
			ConsumeDirectiveEnd(s)
		}
	}
}
