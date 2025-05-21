package scan

import (
	"fmt"
	"unicode/utf8"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Scanner struct {
	src *report.SourceFile
	mgr *report.SourceManager

	start  int // start position of this item
	pos    int // current position of the input
	width  int // width of the last rune read
	line   int // current line number
	column int // current column number

	state StateFn
	items chan token.Token // channel of scanned items
}

func (s *Scanner) NextToken() token.Token {
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

func (s *Scanner) emit(kind token.Kind, rng *report.Range) {
	s.emitWithValue(kind, string(s.src.Content[s.start:s.pos]), rng)
}

func (s *Scanner) emitWithValue(t token.Kind, value string, rng *report.Range) {
	s.items <- token.Token{Kind: t, Lexeme: value, Range: rng}
	s.start = s.pos
}

func (s *Scanner) next() (r rune) {
	if s.pos >= len(s.src.Content) {
		s.width = 0
		return eof
	}

	r, s.width = utf8.DecodeRune(s.src.Content[s.pos:])
	s.pos += s.width

	if r == '\n' {
		s.line++
		s.column = 1
	} else {
		s.column++
	}

	return r
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

func (s *Scanner) errorf(format string, rng *report.Range, args ...interface{}) StateFn {
	s.items <- token.Token{Kind: token.ILLEGAL, Lexeme: fmt.Sprintf(format, args...), Range: rng}
	return scanText
}

func Scan(src *report.SourceFile, mgr *report.SourceManager) *Scanner {
	scan := &Scanner{
		src:    src,
		mgr:    mgr,
		state:  scanText,
		items:  make(chan token.Token, 512),
		line:   1,
		column: 1,
	}

	return scan
}

const eof = -1
