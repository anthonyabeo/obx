package parser

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
)

const eof = -1

type Lexer struct {
	src []byte

	ch       rune // current character
	offset   int  // current character offset
	rdOffset int  // next character offset
}

func (lex *Lexer) InitLexer(src []byte) {
	lex.src = src

	lex.ch = ' '
	lex.offset = 0
	lex.rdOffset = 0

	lex.next()
}

func (lex *Lexer) Lex() (tok token.Token, lit string) {
	lex.skipWhitespace()

	switch ch := lex.ch; {
	case lex.startsIdent(ch):
		lit = lex.identifier()
		tok = token.Lookup(lit)
	case lex.isDigit(ch):
		tok, lit = lex.number()
	default:
		lex.next()
		switch ch {
		case eof:
			tok = token.EOF
		case ',':
			tok = token.COMMA
			lit = ","
		case ';':
			tok = token.SEMICOLON
			lit = ";"
		case '(':
			tok = token.LPAREN
			lit = "("
		case ')':
			tok = token.RPAREN
			lit = ")"
		case ':':
			if lex.ch == '=' {
				lex.next()
				return token.ASSIGN, ":="
			}

			tok = token.COLON
			lit = ":"
		case '=':
			tok = token.EQUAL
			lit = "="
		case '-':
			tok = token.MINUS
			lit = "-"
		case '+':
			tok = token.PLUS
			lit = "+"
		default:
			tok = token.ILLEGAL
			lit = string(ch)
		}
	}

	return
}

func (lex *Lexer) skipWhitespace() {
	for lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == '\r' {
		lex.next()
	}
}

func (lex *Lexer) identifier() string {
	pos := lex.offset
	for lex.isLetter(lex.ch) || lex.isDigit(lex.ch) || lex.ch == '_' {
		lex.next()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) number() (token.Token, string) {
	pos := lex.offset
	for lex.isDigit(lex.ch) {
		lex.next()
	}

	return token.INT, string(lex.src[pos:lex.offset])
}

func (lex *Lexer) isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func (lex *Lexer) isHexDigit(ch rune) bool {
	return lex.isDigit(ch) || 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

func (lex *Lexer) startsIdent(ch rune) bool {
	return lex.isLetter(ch) || '_' == ch
}

func (lex *Lexer) isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' ||
		'A' <= ch && ch <= 'Z'
}

func (lex *Lexer) next() {
	if lex.rdOffset >= len(lex.src) {
		lex.offset = len(lex.src)
		lex.ch = eof
	} else {
		lex.offset = lex.rdOffset

		r, w := rune(lex.src[lex.rdOffset]), 1

		lex.rdOffset += w
		lex.ch = r
	}
}

func (lex *Lexer) peek() byte {
	if lex.rdOffset < len(lex.src) {
		return lex.src[lex.rdOffset]
	}

	return 0
}
