package selector

import (
	"strconv"
	"unicode"
)

type TokenType int

const (
	TokEOF TokenType = iota
	TokIdent
	TokString
	TokNumber
	TokLBrace
	TokRBrace
	TokLParen
	TokRParen
	TokLBracket
	TokRBracket
	TokColon
	TokSemi
	TokComma
	TokEqual
	TokBang
	TokArrow // "->"
)

type Token struct {
	Kind TokenType
	Lit  string
	Pos  int
}

func (t Token) String() string {
	switch t.Kind {
	case TokEOF:
		return "<eof>"
	case TokIdent, TokString, TokNumber:
		return t.Lit
	default:
		return strconv.QuoteRuneToASCII(rune(t.Kind))
	}
}

type Lexer struct {
	runes []rune
	pos   int
}

func NewLexer(src string) *Lexer { return &Lexer{runes: []rune(src)} }

func (l *Lexer) NextToken() Token {
	l.skipSpaceAndComments()
	if l.pos >= len(l.runes) {
		return Token{Kind: TokEOF, Pos: l.pos}
	}

	start := l.pos
	ch := l.next()
	switch ch {
	case '{':
		return Token{Kind: TokLBrace, Lit: "{", Pos: start}
	case '}':
		return Token{Kind: TokRBrace, Lit: "}", Pos: start}
	case '(':
		return Token{Kind: TokLParen, Lit: "(", Pos: start}
	case ')':
		return Token{Kind: TokRParen, Lit: ")", Pos: start}
	case '[':
		return Token{Kind: TokLBracket, Lit: "[", Pos: start}
	case ']':
		return Token{Kind: TokRBracket, Lit: "]", Pos: start}
	case ':':
		return Token{Kind: TokColon, Lit: ":", Pos: start}
	case ';':
		return Token{Kind: TokSemi, Lit: ";", Pos: start}
	case ',':
		return Token{Kind: TokComma, Lit: ",", Pos: start}
	case '=':
		return Token{Kind: TokEqual, Lit: "=", Pos: start}
	case '!':
		return Token{Kind: TokBang, Lit: "!", Pos: start}
	case '"':
		lit := l.scanString('"', start)
		return Token{Kind: TokString, Lit: lit, Pos: start}
	case '`':
		lit := l.scanString('`', start)
		return Token{Kind: TokString, Lit: lit, Pos: start}
	case '-':
		if l.peek() == '>' {
			l.pos++ // consume '>'
			return Token{Kind: TokArrow, Lit: "->", Pos: start}
		}
		if l.peekDigit() {
			return l.scanNumber(start, true)
		}
		return Token{Kind: TokIdent, Lit: "-", Pos: start}
	}

	if unicode.IsDigit(ch) {
		return l.scanNumber(start, false)
	}
	if isIdentStart(ch) {
		return l.scanIdent(start)
	}

	return Token{Kind: TokIdent, Lit: string(ch), Pos: start}
}

func (l *Lexer) skipSpaceAndComments() {
	for l.pos < len(l.runes) {
		switch l.runes[l.pos] {
		case ' ', '\t', '\n', '\r', '\f':
			l.pos++
		case '/':
			if l.pos+1 >= len(l.runes) {
				return
			}
			switch l.runes[l.pos+1] {
			case '/':
				l.pos += 2
				for l.pos < len(l.runes) && l.runes[l.pos] != '\n' {
					l.pos++
				}
			case '*':
				l.pos += 2
				for l.pos+1 < len(l.runes) && !(l.runes[l.pos] == '*' && l.runes[l.pos+1] == '/') {
					l.pos++
				}
				if l.pos+1 < len(l.runes) {
					l.pos += 2
				}
			default:
				return
			}
		default:
			return
		}
	}
}

func (l *Lexer) next() rune {
	if l.pos >= len(l.runes) {
		return 0
	}
	ch := l.runes[l.pos]
	l.pos++
	return ch
}

func (l *Lexer) peek() rune {
	if l.pos >= len(l.runes) {
		return 0
	}
	return l.runes[l.pos]
}

func (l *Lexer) peekDigit() bool {
	return l.pos < len(l.runes) && unicode.IsDigit(l.runes[l.pos])
}

func (l *Lexer) scanIdent(start int) Token {
	for l.pos < len(l.runes) && isIdentPart(l.runes[l.pos]) {
		l.pos++
	}
	return Token{Kind: TokIdent, Lit: string(l.runes[start:l.pos]), Pos: start}
}

func (l *Lexer) scanNumber(start int, signed bool) Token {
	if signed {
		if l.peek() == 0 || !unicode.IsDigit(l.peek()) {
			return Token{Kind: TokIdent, Lit: "-", Pos: start}
		}
	}
	seenDot := false
	for l.pos < len(l.runes) {
		ch := l.runes[l.pos]
		switch {
		case unicode.IsDigit(ch):
			l.pos++
		case ch == '.' && !seenDot:
			seenDot = true
			l.pos++
		default:
			return Token{Kind: TokNumber, Lit: string(l.runes[start:l.pos]), Pos: start}
		}
	}
	return Token{Kind: TokNumber, Lit: string(l.runes[start:l.pos]), Pos: start}
}

func (l *Lexer) scanString(quote rune, start int) string {
	var out []rune
	for l.pos < len(l.runes) {
		ch := l.next()
		if ch == quote {
			return string(out)
		}
		if quote == '"' && ch == '\\' && l.pos < len(l.runes) {
			esc := l.next()
			switch esc {
			case 'n':
				out = append(out, '\n')
			case 'r':
				out = append(out, '\r')
			case 't':
				out = append(out, '\t')
			case '\\', '"', '`':
				out = append(out, esc)
			default:
				out = append(out, esc)
			}
			continue
		}
		out = append(out, ch)
	}
	return string(out)
}

func isIdentStart(r rune) bool {
	return unicode.IsLetter(r) || r == '_' || r == '$'
}

func isIdentPart(r rune) bool {
	return isIdentStart(r) || unicode.IsDigit(r) || r == '-' || r == '.'
}

