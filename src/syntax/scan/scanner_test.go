package scan

import (
	"testing"

	"github.com/anthonyabeo/gocc/src/syntax/token"
)

func TestMinimalProgram(t *testing.T) {
	input := `
int main(void) {
	return 2;
}
`
	tests := []struct {
		kind token.Kind
		lit  string
	}{
		{token.Int, "int"},
		{token.Identifier, "main"},
		{token.LParen, "("},
		{token.Void, "void"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Return, "return"},
		{token.IntLiteral, "2"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
	}

	scan := Scan("", input)

	for _, tt := range tests {
		tok := scan.NextItem()
		if tok.Kind != tt.kind {
			t.Errorf("scan.nextItem(). Expected token kind = %v, Got %v", tt.kind, tok.Kind)
		}

		if tok.Val != tt.lit {
			t.Errorf("scan.nextItem(). Expected token literal = %v, Got %v", tt.lit, tok.Val)
		}
	}
}

func TestMinimalProgramWithUnaryOperators(t *testing.T) {
	input := `
int main(void) {
	return ~(--2);
}
`
	tests := []struct {
		kind token.Kind
		lit  string
	}{
		{token.Int, "int"},
		{token.Identifier, "main"},
		{token.LParen, "("},
		{token.Void, "void"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Return, "return"},
		{token.Comp, "~"},
		{token.LParen, "("},
		{token.Dec, "--"},
		{token.IntLiteral, "2"},
		{token.RParen, ")"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
	}

	scan := Scan("", input)

	for _, tt := range tests {
		tok := scan.NextItem()
		if tok.Kind != tt.kind {
			t.Errorf("scan.nextItem(). Expected token kind = %v, Got %v", tt.kind, tok.Kind)
		}

		if tok.Val != tt.lit {
			t.Errorf("scan.nextItem(). Expected token literal = %v, Got %v", tt.lit, tok.Val)
		}
	}
}

func TestMinimalProgramWithExtraJunk(t *testing.T) {
	input := `int main(void) { return 2; }
foo`
	tests := []struct {
		kind token.Kind
		lit  string
	}{
		{token.Int, "int"},
		{token.Identifier, "main"},
		{token.LParen, "("},
		{token.Void, "void"},
		{token.RParen, ")"},
		{token.LBrace, "{"},
		{token.Return, "return"},
		{token.IntLiteral, "2"},
		{token.SemiColon, ";"},
		{token.RBrace, "}"},
		{token.Identifier, "foo"},
	}

	scan := Scan("", input)

	for _, tt := range tests {
		tok := scan.NextItem()
		if tok.Kind != tt.kind {
			t.Errorf("scan.nextItem(). Expected token kind = %v, Got %v", tt.kind, tok.Kind)
		}

		if tok.Val != tt.lit {
			t.Errorf("scan.nextItem(). Expected token literal = %v, Got %v", tt.lit, tok.Val)
		}
	}
}
