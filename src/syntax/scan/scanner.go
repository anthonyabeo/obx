package scan

import (
	"fmt"
	"unicode/utf8"

	"github.com/anthonyabeo/gocc/src/syntax/token"
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
	panic("not reached")
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

func (s *Scanner) isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' ||
		'A' <= ch && ch <= 'Z'
}

func (s *Scanner) identifier() string {
peek:
	ch := s.peek()
	if s.isLetter(ch) || s.isDigit(ch) || ch == '_' {
		s.next()
		goto peek
	}

	return s.input[s.start:s.pos]
}

func (s *Scanner) number() token.Kind {
peek:
	ch := s.peek()
	if s.isDigit(ch) {
		s.next()
		goto peek
	}

	// if lex.ch is not a word boundary, return an error
	if !s.isDelim() && !s.isOperator() && !s.isWhiteSpace() {
		s.errorf("%c is an invalid number digit", ch)
	}

	return token.IntLiteral
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

func (s *Scanner) isWhiteSpace() bool {
	ch := s.peek()

	switch ch {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (s *Scanner) isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
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
	s.items <- token.Token{Kind: token.Illegal, Val: fmt.Sprintf(format, args...)}
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
