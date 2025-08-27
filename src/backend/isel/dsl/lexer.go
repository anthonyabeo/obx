package dsl

import (
	"fmt"
	"unicode"
)

type TokenType int

const (
	TokEOF TokenType = iota
	TokIdent
	TokCost
	TokIn
	TokOut
	TokTemps
	TokRule
	TokAsm
	TokCond
	TokInt
	TokString
	TokPattern
	TokLParen
	TokRParen
	TokLBrace
	TokRBrace
	TokComma
	TokColon
	TokArrow
	TokSemi
	TokBang
	TokDollar
	TokHeader
)

type Token struct {
	Kind  TokenType
	Value string
}

type Lexer struct {
	input []rune
	pos   int
}

func NewLexer(s string) *Lexer {
	return &Lexer{input: []rune(s)}
}

func (l *Lexer) peek() rune {
	if l.pos >= len(l.input) {
		return 0
	}
	return l.input[l.pos]
}

func (l *Lexer) next() rune {
	if l.pos >= len(l.input) {
		return 0
	}
	ch := l.input[l.pos]
	l.pos++
	return ch
}

func (l *Lexer) skipSpaces() {
	for unicode.IsSpace(l.peek()) {
		l.next()
	}
}

func (l *Lexer) NextToken() Token {
	l.skipSpaces()
	ch := l.peek()
	if ch == 0 {
		return Token{Kind: TokEOF}
	}

	// punctuation
	switch ch {
	case '$':
		l.next()
		return Token{Kind: TokDollar, Value: "$"}
	case '(':
		l.next()
		return Token{Kind: TokLParen, Value: "("}
	case ')':
		l.next()
		return Token{Kind: TokRParen, Value: ")"}
	case '{':
		l.next()
		return Token{Kind: TokLBrace, Value: "{"}
	case '}':
		l.next()
		return Token{Kind: TokRBrace, Value: "}"}
	case ',':
		l.next()
		return Token{Kind: TokComma, Value: ","}
	case ':':
		l.next()
		return Token{Kind: TokColon, Value: ":"}
	case ';':
		l.next()
		return Token{Kind: TokSemi, Value: ";"}
	case '!':
		l.next()
		return Token{Kind: TokBang, Value: "!"}
	case '=':
		if l.pos+1 < len(l.input) && l.input[l.pos+1] == '>' {
			l.pos += 2
			return Token{Kind: TokArrow, Value: "=>"}
		}
	case '"':
		l.next()
		start := l.pos
		for l.peek() != '"' && l.peek() != 0 {
			l.next()
		}
		val := string(l.input[start:l.pos])
		if l.peek() == '"' {
			l.next()
		}
		return Token{Kind: TokString, Value: val}
	}

	// number
	if unicode.IsDigit(ch) {
		start := l.pos
		for unicode.IsDigit(l.peek()) {
			l.next()
		}
		return Token{Kind: TokInt, Value: string(l.input[start:l.pos])}
	}

	// identifier
	if unicode.IsLetter(ch) {
		start := l.pos
		for unicode.IsLetter(l.peek()) || unicode.IsDigit(l.peek()) || l.peek() == '_' {
			l.next()
		}
		ident := string(l.input[start:l.pos])
		switch ident {
		case "rule":
			return Token{Kind: TokRule, Value: ident}
		case "out":
			return Token{Kind: TokOut, Value: ident}
		case "in":
			return Token{Kind: TokIn, Value: ident}
		case "temps":
			return Token{Kind: TokTemps, Value: ident}
		case "pattern":
			return Token{Kind: TokPattern, Value: ident}
		case "asm":
			return Token{Kind: TokAsm, Value: ident}
		case "cost":
			return Token{Kind: TokCost, Value: ident}
		case "cond":
			return Token{Kind: TokCond, Value: ident}
		}
		return Token{Kind: TokIdent, Value: ident}
	}

	panic(fmt.Sprintf("unexpected char: %q", ch))
}
