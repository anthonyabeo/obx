package lexer

import (
	"bytes"
	"strconv"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

const eof = -1

type Lexer struct {
	file *token.File

	src []byte

	ch       rune // current character
	offset   int  // current character offset
	rdOffset int  // next character offset
}

func (lex *Lexer) InitLexer(file *token.File, src []byte) {
	lex.file = file

	lex.src = src

	lex.ch = ' '
	lex.offset = 0
	lex.rdOffset = 0

	lex.next()
}

func (lex *Lexer) Lex() (tok token.Token, lit string, pos *token.Position) {
	lex.skipWhitespace()

	colNo := (lex.offset - lex.file.LastLineOffset()) + 1
	pos = &token.Position{Line: lex.file.CurLineNo(), Column: colNo, Filename: lex.file.Name()}

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
		case '.':
			if lex.ch == '.' {
				lex.next()
				return token.RANGE, "..", pos
			}

			tok = token.PERIOD
			lit = "."
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
				return token.BECOMES, ":=", pos
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
		case '>':
			if lex.ch == '=' {
				lex.next()
				return token.GEQ, ">=", pos
			}

			tok = token.GREAT
			lit = ">"
		case '<':
			if lex.ch == '=' {
				lex.next()
				return token.LEQ, "<=", pos
			}

			tok = token.LESS
			lit = "<"
		case '#':
			tok = token.NEQ
			lit = "#"
		case '[':
			tok = token.LBRACK
			lit = "["
		case ']':
			tok = token.RBRACK
			lit = "]"
		case '|':
			tok = token.BAR
			lit = "|"
		case '{':
			tok = token.LBRACE
			lit = "{"
		case '}':
			tok = token.RBRACE
			lit = "}"
		case '\'', '"':
			lit = lex.readString(ch)
			tok = token.STRING
			lex.next()
		case '$':
			lit = lex.readHexString()
			tok = token.HEXSTRING
			lex.next()
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

func (lex *Lexer) number() (tok token.Token, lit string) {
	pos := lex.offset
	for lex.isDigit(lex.ch) || lex.isHexDigit(lex.ch) {
		lex.next()
	}

	// CHAR
	if lex.ch == 'X' || lex.ch == 'x' {
		lex.next()
		return token.CHAR, string(lex.src[pos : lex.offset-1])
	}

	// INT64
	if lex.ch == 'L' || lex.ch == 'l' {
		lex.next()
		return token.INT64, string(lex.src[pos : lex.offset-1])
	}

	// INT32
	if lex.ch == 'I' || lex.ch == 'i' {
		lex.next()
		return token.INT32, string(lex.src[pos : lex.offset-1])
	}

	// REAL
	if lex.ch == '.' {
		lex.next()
		for lex.isDigit(lex.ch) {
			lex.next()
		}

		var typ = token.REAL
		if lex.ch == 'E' || lex.ch == 'e' || lex.ch == 'D' || lex.ch == 'd' {
			typ = token.LONGREAL
			lex.next()
		}

		if lex.ch == 'S' || lex.ch == 's' {
			typ = token.REAL
			lex.next()
		}

		if lex.ch == '+' || lex.ch == '-' {
			lex.next()
		}

		for lex.isDigit(lex.ch) {
			lex.next()
		}

		return typ, string(lex.src[pos:lex.offset])
	}

	var (
		//i    int64
		err  error
		base = 10
	)

	// DECIMAL & HEX
	if lex.ch == 'H' || lex.ch == 'h' {
		base = 16
	}

	lit = string(lex.src[pos:lex.offset])
	_, err = strconv.ParseInt(lit, base, 64)
	if err != nil {
		return token.ILLEGAL, lit
	}

	if base == 16 {
		lex.next()
		lit = string(lex.src[pos:lex.offset])
	}

	//switch {
	//case 0 <= i && i <= 255:
	//	tok = token.BYTE
	//case -128 <= i && i <= 127:
	//	tok = token.INT8
	//case -32768 <= i && i <= 32767:
	//	tok = token.INT16
	//case -2147483648 <= i && i <= 2147483647:
	//	tok = token.INT32
	//case -9223372036854775808 <= i && i < 9223372036854775807:
	//	tok = token.INT64
	//}
	tok = token.INT

	return tok, lit
}

func (lex *Lexer) readString(open rune) string {
	pos := lex.offset
	for lex.ch != open {
		lex.next()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) readHexString() string {
	var b bytes.Buffer
	for lex.ch != '$' {
		if lex.isHexDigit(lex.ch) {
			b.WriteRune(lex.ch)
		}

		lex.next()
	}

	return b.String()
}

func (lex *Lexer) isDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func (lex *Lexer) isHexDigit(ch rune) bool {
	return lex.isDigit(ch) || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F'
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
		if lex.ch == '\n' {
			lex.file.UpdateLineNo()
			lex.file.AddLine(lex.offset)
		}
		lex.ch = eof
	} else {
		lex.offset = lex.rdOffset
		if lex.ch == '\n' {
			lex.file.UpdateLineNo()
			lex.file.AddLine(lex.offset)
		}

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
