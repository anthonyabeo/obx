package scan

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type StateFn func(*Scanner) StateFn

func scanIdentifier(sc *Scanner) StateFn {
	lit := sc.scanIdentifier()
	tok := token.Lookup(lit)

	sc.emit(tok)
	return scanText
}

func scanNumber(sc *Scanner) StateFn {
	kind := sc.scanNumber()
	sc.emit(kind)

	return scanText
}

func scanHexString(sc *Scanner) StateFn {
	kind := sc.scanHexString()
	sc.emit(kind)

	return scanText
}

func scanText(s *Scanner) StateFn {
	for ch := s.next(); ch != eof; {
		switch ch {
		case '.':
			c := s.peek()
			if c == '.' {
				s.emit(token.RANGE)
			} else {
				s.emit(token.PERIOD)
			}
		case '-':
			s.emit(token.MINUS)
		case '+':
			s.emit(token.PLUS)
		case '=':
			s.emit(token.EQUAL)
		case '<':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.LEQ)
			} else {
				s.emit(token.LESS)
			}
		case '>':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.GEQ)
			} else {
				s.emit(token.GREAT)
			}
		case '&':
			s.emit(token.AND)
		case '|':
			s.emit(token.BAR)
		case '#':
			s.emit(token.NEQ)
		case '^':
			s.emit(token.CARET)
		case ':':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.BECOMES)
			} else {
				s.emit(token.COLON)
			}
		case '/':
			c := s.peek()
			if c == '/' {
				s.next()
				s.emit(token.SL_COMMENT_START)
			} else {
				s.emit(token.QUOT)
			}
		case '*':
			c := s.peek()
			if c == ')' {
				s.next()
				s.emit(token.ML_COMMENT_END)
			} else {
				s.emit(token.STAR)
			}
		case '{':
			s.emit(token.LBRACE)
		case '}':
			s.emit(token.RBRACE)
		case '[':
			s.emit(token.LBRACK)
		case ']':
			s.emit(token.RBRACK)
		case '(':
			c := s.peek()
			if c == '*' {
				s.next()
				s.emit(token.ML_COMMENT_START)
			} else {
				s.emit(token.LPAREN)
			}
		case ')':
			s.emit(token.RPAREN)
		case ',':
			s.emit(token.COMMA)
		case '~':
			s.emit(token.NOT)
		case ';':
			s.emit(token.SEMICOLON)
		case ' ', '\t', '\n':
			s.ignore()
		case '$':
			s.backup()
			return scanHexString
		case '\'', '"':
			s.backup()
			return scanString
		default:
			if s.startsIdent(ch) {
				s.backup()
				return scanIdentifier
			} else if s.isDecDigit(ch) {
				s.backup()
				return scanNumber
			} else {
				s.errorf("invalid character %c", ch)
			}
		}

		ch = s.next()
	}

	s.emit(token.EOF)

	return nil
}

func scanString(s *Scanner) StateFn {
	kind := s.scanString()
	s.emit(kind)

	return scanText
}
