package parser

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestLexingOfIdentifiers(t *testing.T) {
	input := `
x
Scan
Oberon_2
_y
firstLetter
`
	lexer := Lexer{}
	lexer.InitLexer([]byte(input))

	tests := []struct {
		tokenKind token.Token
		tokenLit  string
	}{
		{token.IDENT, "x"},
		{token.IDENT, "Scan"},
		{token.IDENT, "Oberon_2"},
		{token.IDENT, "_y"},
		{token.IDENT, "firstLetter"},
	}

	for _, tt := range tests {
		tok, lit := lexer.Lex()
		if tok != tt.tokenKind {
			t.Errorf(fmt.Sprintf("lex.Lex(). Expected token kind = %v, Got %v", tt.tokenKind, tok))
		}

		if lit != tt.tokenLit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.tokenLit, lit)
		}
	}
}

func TestLexingOfMinimalObxProgram(t *testing.T) {
	input := `
module Main
	proc fib(n : integer): integer
		var a, b: integer 

  	begin
		if (n = 0) or (n = 1) then
			return n
		else
		  a := fib(n - 1)
		  b := fib(n - 2)
		  return a + b
		end
  	end fib

begin
	res := fib(21)
  	assert(res = 10946)
end Main
`

	lexer := Lexer{}
	lexer.InitLexer([]byte(input))

	tests := []struct {
		tokenKind token.Token
		tokenLit  string
	}{
		{token.MODULE, "module"},
		{token.IDENT, "Main"},

		{token.PROC, "proc"},
		{token.IDENT, "fib"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},
		{token.RPAREN, ")"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},

		{token.VAR, "var"},
		{token.IDENT, "a"},
		{token.COMMA, ","},
		{token.IDENT, "b"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},

		{token.BEGIN, "begin"},

		{token.IF, "if"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.EQUAL, "="},
		{token.INT, "0"},
		{token.RPAREN, ")"},
		{token.OR, "or"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.EQUAL, "="},
		{token.INT, "1"},
		{token.RPAREN, ")"},
		{token.THEN, "then"},

		{token.RETURN, "return"},
		{token.IDENT, "n"},

		{token.ELSE, "else"},

		{token.IDENT, "a"},
		{token.ASSIGN, ":="},
		{token.IDENT, "fib"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.MINUS, "-"},
		{token.INT, "1"},
		{token.RPAREN, ")"},

		{token.IDENT, "b"},
		{token.ASSIGN, ":="},
		{token.IDENT, "fib"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.MINUS, "-"},
		{token.INT, "2"},
		{token.RPAREN, ")"},

		{token.RETURN, "return"},
		{token.IDENT, "a"},
		{token.PLUS, "+"},
		{token.IDENT, "b"},

		{token.END, "end"},
		{token.END, "end"},
		{token.IDENT, "fib"},

		{token.BEGIN, "begin"},
		{token.IDENT, "res"},
		{token.ASSIGN, ":="},
		{token.IDENT, "fib"},
		{token.LPAREN, "("},
		{token.INT, "21"},
		{token.RPAREN, ")"},

		{token.IDENT, "assert"},
		{token.LPAREN, "("},
		{token.IDENT, "res"},
		{token.EQUAL, "="},
		{token.INT, "10946"},
		{token.RPAREN, ")"},

		{token.END, "end"},
		{token.IDENT, "Main"},
	}

	for _, tt := range tests {
		tok, lit := lexer.Lex()
		if tok != tt.tokenKind {
			t.Errorf(fmt.Sprintf("lex.Lex(). Expected token kind = %v, Got %v", tt.tokenKind, tok))
		}

		if lit != tt.tokenLit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.tokenLit, lit)
		}
	}
}
