package scan

import (
	"fmt"
	"unicode/utf8"

	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Scanner struct {
	content []byte

	start  int // start position of this item
	pos    int // current position of the input
	width  int // width of the last rune read
	line   int // current line number
	column int // current column number
	items  chan token.Token // channel of scanned items
}

func (s *Scanner) NextToken() token.Token {
	tok, ok := <-s.items
	if !ok {
		return token.Token{Kind: token.EOF}
	}
	return tok
}

func (s *Scanner) run() {
	for state := scanText; state != nil; {
		state = state(s)
	}

	close(s.items)
}

func (s *Scanner) emit(kind token.Kind, pos, end int) {
	s.emitWithValue(kind, string(s.content[s.start:s.pos]), pos, end)
}

func (s *Scanner) emitWithValue(t token.Kind, value string, pos, end int) {
	s.items <- token.Token{Kind: t, Lexeme: value, Pos: pos, End: end}
	s.start = s.pos
}

func (s *Scanner) next() (r rune) {
	if s.pos >= len(s.content) {
		s.width = 0
		return eof
	}

	r, s.width = utf8.DecodeRune(s.content[s.pos:])
	s.pos += s.width

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

func (s *Scanner) errorf(format string /*, rng *diag.Range*/, args ...interface{}) StateFn {
	s.items <- token.Token{Kind: token.ILLEGAL, Lexeme: fmt.Sprintf(format, args...) /*, Range: rng*/, Pos: s.start, End: s.pos}
	s.start = s.pos // reset so the next token starts cleanly after the error
	return scanText
}

// Scan creates a Scanner for the given source file.  The content bytes are
// registered with srcMgr under fileName so that source spans can be resolved
// later during diagnostic formatting.
func Scan(fileName string, content []byte, srcMgr *source.Manager) *Scanner {
	srcMgr.Load(fileName, content)
	scan := &Scanner{
		content: content,
		items:   make(chan token.Token, 512),
		line:    1,
		column:  1,
	}

	go scan.run()

	return scan
}

const eof = -1
