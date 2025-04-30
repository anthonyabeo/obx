package scan

import (
	"fmt"
	"math"
	"strconv"
	"unicode/utf8"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Scanner struct {
	name  string
	input string // the string being tokenized
	start int    // start position of this item
	pos   int    // current position of the input
	width int    // width of the last rune read

	state StateFn
	items chan token.Token // channel of scanned items
}

func (s *Scanner) NextItem() token.Token {
	for {
		select {
		case item := <-s.items:
			return item
		default:
			s.state = s.state(s)
		}
	}
}

func (s *Scanner) run() {
	for state := scanText; state != nil; {
		state = state(s)
	}

	close(s.items)
}

func (s *Scanner) emit(kind token.Kind) {
	s.items <- token.Token{Kind: kind, Val: s.input[s.start:s.pos]}
	s.start = s.pos
}

func (s *Scanner) next() (r rune) {
	if s.pos >= len(s.input) {
		s.width = 0
		return eof
	}

	r, s.width = utf8.DecodeRuneInString(s.input[s.pos:])
	s.pos += s.width

	return r
}

func (s *Scanner) scanIdentifier() string {
	ch := s.next()
	for s.isLetter(ch) || s.isDecDigit(ch) || ch == '_' {
		ch = s.next()
	}

	return s.input[s.start:s.pos]
}

func (s *Scanner) scanNumber() token.Kind {
	var kind token.Kind

	ch := s.peek()

	if s.isDecDigit(ch) {
		kind = token.INT_LIT

		for s.isDecDigit(ch) {
			ch = s.next()
		}

		if ch == '.' {
			ch = s.next()
			if !s.isDecDigit(ch) {
				s.errorf("%c is not a decimal digit. At least one decimal digit expected", ch)
				return token.ILLEGAL
			}

			for s.isDecDigit(ch) {
				ch = s.next()
			}

			kind = token.REAL_LIT

			if ch == 'E' || ch == 'e' || ch == 'D' || ch == 'd' || ch == 'S' || ch == 's' {
				exp := ch
				ch = s.next()

				pos := s.pos - 1
				if ch == '+' || ch == '-' {
					ch = s.next()
				}

				if !s.isDecDigit(ch) {
					s.errorf("'%c' is not a decimal digit. Decimal number expected for exponent", ch)
					return token.ILLEGAL
				}

				for s.isDecDigit(ch) {
					ch = s.next()
				}

				switch exp {
				case 'D', 'd':
					kind = token.LONGREAL_LIT
				case 'S', 's':
					kind = token.REAL_LIT
				case 'E', 'e':
					val, err := strconv.ParseInt(s.input[pos:s.pos], 10, 64)
					if err == nil && (val >= math.MinInt32 && val <= math.MaxInt32) {
						kind = token.REAL_LIT
					} else {
						kind = token.LONGREAL_LIT
					}
				}
			}

			// check for invalid real boundary character
			if !s.isDelim() && !s.isOperator() && ch != eof {
				s.errorf("%c is not a valid real boundary character", ch)
				return token.ILLEGAL
			}

			return kind
		}
	}

	if s.isHexDigit(ch) || ch == 'H' || ch == 'h' || ch == 'X' || ch == 'x' {
		err := false
		for s.isHexDigit(ch) {
			ch = s.next()
		}

		if ch == 'X' || ch == 'x' {
			ch = s.next()

			if !s.isDelim() && !s.isOperator() && ch != eof {
				s.errorf("%c is not a valid integer boundary character", ch)
				return token.ILLEGAL
			}

			return token.CHAR_LIT
		}

		// check for hex marker
		if ch != 'H' && ch != 'h' {
			s.errorf("%c is not a hex marker", ch)
			err = true
		}
		ch = s.next()

		if !err {
			kind = token.INT_LIT
		} else {
			return token.ILLEGAL
		}
	}

	// check for integer size specifier
	if ch == 'L' || ch == 'l' {
		ch = s.next()
		kind = token.INT64_LIT
	}

	if ch == 'I' || ch == 'i' {
		ch = s.next()
		kind = token.INT32_LIT
	}

	// check for invalid integer boundary character
	if !s.isDelim() && !s.isOperator() && ch != eof {
		s.errorf("%c is not a valid integer boundary character", ch)
		kind = token.ILLEGAL
	}

	return kind
}

func (s *Scanner) scanHexString() token.Kind {
	delim := s.next()
	ch := s.next()

	for s.isHexDigit(ch) || s.isWhiteSpace(ch) {
		ch = s.next()
	}

	if ch != delim {
		s.errorf("'%c' is not a valid a hex string character", ch)
		return token.ILLEGAL
	}

	return token.HEX_STR_LIT
}

func (s *Scanner) scanString() token.Kind {
	delim := s.next()
	ch := s.next()

	for ch != delim {
		if s.isPrintable(ch) {
			ch = s.next()
		} else {
			s.errorf("'%c' is not a valid string character", ch)
			return token.ILLEGAL
		}
	}

	return token.STR_LIT
}

func (s *Scanner) backup() {
	s.pos -= s.width
}

func (s *Scanner) ignore() {
	s.start = s.pos
}

func (s *Scanner) peek() rune {
	r := s.next()
	s.backup()
	return r
}

func (s *Scanner) errorf(format string, args ...interface{}) StateFn {
	s.items <- token.Token{Kind: token.ILLEGAL, Val: fmt.Sprintf(format, args...)}
	return nil
}

func Scan(name, input string) *Scanner {
	scan := &Scanner{
		name:  name,
		input: input,
		state: scanText,
		items: make(chan token.Token, 512),
	}

	return scan
}

const eof = -1
