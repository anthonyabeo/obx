package scan

import (
	"github.com/anthonyabeo/gocc/src/syntax/token"
)

type StateFn func(*Scanner) StateFn

func scanIdentifier(sc *Scanner) StateFn {
	lit := sc.identifier()
	tok := token.Lookup(lit)

	sc.emit(tok)
	return scanText
}

func scanNumber(sc *Scanner) StateFn {
	kind := sc.number()
	sc.emit(kind)

	return scanText
}

func scanText(s *Scanner) StateFn {
	for ch := s.next(); ch != eof; {
		switch ch {
		case '~':
			s.emit(token.Comp)
		case '-':
			c := s.peek()
			if c == '-' {
				s.next()
				s.emit(token.Dec)
			} else {
				s.emit(token.Sub)
			}
		case '+':
			c := s.peek()
			if c == '+' {
				s.next()
				s.emit(token.Inc)
			} else {
				s.emit(token.Add)
			}
		case '=':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.Eq)
			} else {
				s.emit(token.Assign)
			}
		case '!':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.Neq)
			} else {
				s.emit(token.LNot)
			}
		case '<':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.Leq)
			} else {
				s.emit(token.Lt)
			}
		case '>':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.Geq)
			} else {
				s.emit(token.Gt)
			}
		case '&':
			c := s.peek()
			if c == '&' {
				s.next()
				s.emit(token.And)
			} else {
				s.emit(token.LAnd)
			}
		case '|':
			c := s.peek()
			if c == '|' {
				s.next()
				s.emit(token.Or)
			} else {
				s.emit(token.LOr)
			}
		case '^':
			s.emit(token.Xor)
		case '?':
			s.emit(token.Quest)
		case ':':
			s.emit(token.Colon)
		case '/':
			s.emit(token.Div)
		case '*':
			s.emit(token.Mul)
		case '%':
			s.emit(token.Mod)
		case '{':
			s.emit(token.LBrace)
		case '}':
			s.emit(token.RBrace)
		case '[':
			s.emit(token.LBrack)
		case ']':
			s.emit(token.RBrack)
		case '(':
			s.emit(token.LParen)
		case ')':
			s.emit(token.RParen)
		case ',':
			s.emit(token.Comma)
		case ';':
			s.emit(token.SemiColon)
		case ' ', '\t', '\n':
			s.ignore()
		default:
			if s.isLetter(ch) {
				s.backup()
				return scanIdentifier
			} else if s.isDigit(ch) {
				s.backup()
				return scanNumber
			} else {
				s.errorf("invalid character %c", ch)
			}
		}

		ch = s.next()
	}

	s.emit(token.Eof)

	return nil
}
