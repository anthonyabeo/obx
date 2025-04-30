package scan

import "unicode"

func (s *Scanner) isWhiteSpace(ch rune) bool {

	switch ch {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (s *Scanner) isDecDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func (s *Scanner) isHexDigit(ch rune) bool {
	return s.isDecDigit(ch) ||
		'a' <= ch && ch <= 'f' ||
		'A' <= ch && ch <= 'F'
}

func (s *Scanner) isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' ||
		'A' <= ch && ch <= 'Z'
}

func (s *Scanner) startsIdent(ch rune) bool {
	return s.isLetter(ch) || ch == '_'
}

func (s *Scanner) isDelim() bool {
	ch := s.peek()

	switch ch {
	case '(', ')', '[', ']', '{', '}', ',', ';':
		return true
	default:
		return false
	}
}

func (s *Scanner) isOperator() bool {
	ch := s.peek()

	switch ch {
	case '+', '-', '*', '/', '%', '<', '>', '&', '|', '^', '~', '!':
		return true
	default:
		return false
	}
}

func (s *Scanner) isPrintable(ch rune) bool {
	return unicode.IsPrint(ch)
}
