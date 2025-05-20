package scan

import (
	"fmt"
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

func (s *Scanner) emitWithValue(t token.Kind, value string) {
	s.items <- token.Token{Kind: t, Val: value /*, add Position if needed */}
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

func (s *Scanner) scanIdentifier() token.Kind {
	start := s.pos

	// The first character must be a letter or '_'
	r := s.peek()
	if !s.isLetter(r) && !s.isDecDigit(r) && r != '_' {
		s.errorf("invalid identifier: must start with letter or '_'")
		return token.ILLEGAL
	}

	// Consume the rest: letters, digits, or underscores
	for {
		r = s.peek()
		if s.isLetter(r) || s.isDecDigit(r) || r == '_' {
			s.next()
		} else {
			break
		}
	}

	return token.Lookup(s.input[start:s.pos])
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
	return scanText
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
