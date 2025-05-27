package scan

import (
	"bytes"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type ExpectedToken struct {
	Kind        token.Kind
	Lexeme      string
	StartLine   int
	StartColumn int
	EndLine     int
	EndColumn   int
}

func expectTokens(t *testing.T, src []byte, filename string, expected []ExpectedToken) {
	ctx := &report.Context{
		FileName: filename,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	sc := Scan(src, ctx)

	var tokens []token.Token
	for {
		tok := sc.NextToken()

		if tok.Kind != token.EOF {
			tokens = append(tokens, tok)
		} else {
			break
		}
	}

	if len(tokens) != len(expected) {
		t.Fatalf("expected %d tokens, got %d", len(expected), len(tokens))
	}

	for i, exp := range expected {
		tok := tokens[i]

		if tok.Kind != exp.Kind {
			t.Errorf("token %d: expected kind %v, got %v", i, exp.Kind, tok.Kind)
		}

		if tok.Lexeme != exp.Lexeme {
			t.Errorf("token %d: expected lexeme %q, got %q", i, exp.Lexeme, tok.Lexeme)
		}

		r := ctx.Source.Span(filename, tok.Pos, tok.End)
		start := r.Start
		end := r.End

		if start.Line != exp.StartLine || start.Column != exp.StartColumn {
			t.Errorf("token %d (%q): expected start %d:%d, got %d:%d",
				i, tok.Lexeme, exp.StartLine, exp.StartColumn, start.Line, start.Column)
		}

		if end.Line != exp.EndLine || end.Column != exp.EndColumn {
			t.Errorf("token %d (%q): expected end %d:%d, got %d:%d",
				i, tok.Lexeme, exp.EndLine, exp.EndColumn, end.Line, end.Column)
		}
	}
}

func TestLexerTokenRangesHelper(t *testing.T) {
	src := []byte(`
MODULE Hello;
VAR x, y: INTEGER;
BEGIN
  x := 42;
  y := x + 1
END Hello.
`)

	expectTokens(t, src, "example.ob", []ExpectedToken{
		{token.MODULE, "MODULE", 2, 1, 2, 7},
		{token.IDENTIFIER, "Hello", 2, 8, 2, 13},
		{token.SEMICOLON, ";", 2, 13, 2, 14},
		{token.VAR, "VAR", 3, 1, 3, 4},
		{token.IDENTIFIER, "x", 3, 5, 3, 6},
		{token.COMMA, ",", 3, 6, 3, 7},
		{token.IDENTIFIER, "y", 3, 8, 3, 9},
		{token.COLON, ":", 3, 9, 3, 10},
		{token.INTEGER, "INTEGER", 3, 11, 3, 18},
		{token.SEMICOLON, ";", 3, 18, 3, 19},
		{token.BEGIN, "BEGIN", 4, 1, 4, 6},
		{token.IDENTIFIER, "x", 5, 3, 5, 4},
		{token.BECOMES, ":=", 5, 5, 5, 7},
		{token.INT_LIT, "42", 5, 8, 5, 10},
		{token.SEMICOLON, ";", 5, 10, 5, 11},
		{token.IDENTIFIER, "y", 6, 3, 6, 4},
		{token.BECOMES, ":=", 6, 5, 6, 7},
		{token.IDENTIFIER, "x", 6, 8, 6, 9},
		{token.PLUS, "+", 6, 10, 6, 11},
		{token.INT_LIT, "1", 6, 12, 6, 13},
		{token.END, "END", 7, 1, 7, 4},
		{token.IDENTIFIER, "Hello", 7, 5, 7, 10},
		{token.PERIOD, ".", 7, 10, 7, 11},
	})
}

func TestLexerLargeProgramControlFlow(t *testing.T) {
	src := []byte(`
MODULE Math;

VAR a, b, result: INTEGER;

BEGIN
  a := 10;
  b := 20;

  IF a < b THEN
    result := a * b
  END;

  WHILE result > 0 DO
    result := result - 1
  END

END Math.
`)

	expectTokens(t, src, "math.ob", []ExpectedToken{
		{token.MODULE, "MODULE", 2, 1, 2, 7},
		{token.IDENTIFIER, "Math", 2, 8, 2, 12},
		{token.SEMICOLON, ";", 2, 12, 2, 13},

		{token.VAR, "VAR", 4, 1, 4, 4},
		{token.IDENTIFIER, "a", 4, 5, 4, 6},
		{token.COMMA, ",", 4, 6, 4, 7},
		{token.IDENTIFIER, "b", 4, 8, 4, 9},
		{token.COMMA, ",", 4, 9, 4, 10},
		{token.IDENTIFIER, "result", 4, 11, 4, 17},
		{token.COLON, ":", 4, 17, 4, 18},
		{token.INTEGER, "INTEGER", 4, 19, 4, 26},
		{token.SEMICOLON, ";", 4, 26, 4, 27},

		{token.BEGIN, "BEGIN", 6, 1, 6, 6},

		{token.IDENTIFIER, "a", 7, 3, 7, 4},
		{token.BECOMES, ":=", 7, 5, 7, 7},
		{token.INT_LIT, "10", 7, 8, 7, 10},
		{token.SEMICOLON, ";", 7, 10, 7, 11},

		{token.IDENTIFIER, "b", 8, 3, 8, 4},
		{token.BECOMES, ":=", 8, 5, 8, 7},
		{token.INT_LIT, "20", 8, 8, 8, 10},
		{token.SEMICOLON, ";", 8, 10, 8, 11},

		// Comment is ignored by token

		{token.IF, "IF", 10, 3, 10, 5},
		{token.IDENTIFIER, "a", 10, 6, 10, 7},
		{token.LESS, "<", 10, 8, 10, 9},
		{token.IDENTIFIER, "b", 10, 10, 10, 11},
		{token.THEN, "THEN", 10, 12, 10, 16},

		{token.IDENTIFIER, "result", 11, 5, 11, 11},
		{token.BECOMES, ":=", 11, 12, 11, 14},
		{token.IDENTIFIER, "a", 11, 15, 11, 16},
		{token.STAR, "*", 11, 17, 11, 18},
		{token.IDENTIFIER, "b", 11, 19, 11, 20},

		{token.END, "END", 12, 3, 12, 6},
		{token.SEMICOLON, ";", 12, 6, 12, 7},

		{token.WHILE, "WHILE", 14, 3, 14, 8},
		{token.IDENTIFIER, "result", 14, 9, 14, 15},
		{token.GREAT, ">", 14, 16, 14, 17},
		{token.INT_LIT, "0", 14, 18, 14, 19},
		{token.DO, "DO", 14, 20, 14, 22},

		{token.IDENTIFIER, "result", 15, 5, 15, 11},
		{token.BECOMES, ":=", 15, 12, 15, 14},
		{token.IDENTIFIER, "result", 15, 15, 15, 21},
		{token.MINUS, "-", 15, 22, 15, 23},
		{token.INT_LIT, "1", 15, 24, 15, 25},

		{token.END, "END", 16, 3, 16, 6},

		{token.END, "END", 18, 1, 18, 4},
		{token.IDENTIFIER, "Math", 18, 5, 18, 9},
		{token.PERIOD, ".", 18, 9, 18, 10},
	})

}

func TestLexerTokenRanges(t *testing.T) {
	src := []byte(`VAR x := 42
    y := x + 1
`)
	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	lx := Scan(src, ctx)

	var tokens []token.Token
	for {
		tok := lx.NextToken()
		tokens = append(tokens, tok)
		if tok.Kind == token.EOF {
			break
		}
	}

	tests := []struct {
		Index       int
		Lexeme      string
		StartLine   int
		StartColumn int
		EndLine     int
		EndColumn   int
	}{
		{0, "VAR", 1, 1, 1, 4},
		{1, "x", 1, 5, 1, 6},
		{2, ":=", 1, 7, 1, 9},
		{3, "42", 1, 10, 1, 12},
		{4, "y", 2, 5, 2, 6},
		{5, ":=", 2, 7, 2, 9},
		{6, "x", 2, 10, 2, 11},
		{7, "+", 2, 12, 2, 13},
		{8, "1", 2, 14, 2, 15},
	}

	for _, tt := range tests {
		tok := tokens[tt.Index]
		if tok.Lexeme != tt.Lexeme {
			t.Errorf("token %d: expected lexeme %q, got %q", tt.Index, tt.Lexeme, tok.Lexeme)
		}

		r := ctx.Source.Span(file, tok.Pos, tok.End)

		start := r.Start
		end := r.End

		if start.Line != tt.StartLine || start.Column != tt.StartColumn {
			t.Errorf("token %d (%q): expected start %d:%d, got %d:%d",
				tt.Index, tt.Lexeme, tt.StartLine, tt.StartColumn, start.Line, start.Column)
		}

		if end.Line != tt.EndLine || end.Column != tt.EndColumn {
			t.Errorf("token %d (%q): expected end %d:%d, got %d:%d",
				tt.Index, tt.Lexeme, tt.EndLine, tt.EndColumn, end.Line, end.Column)
		}
	}
}

func TestOffsetToLineCol_WithTabs(t *testing.T) {
	src := []byte("a\tb\tc\n1234\t56    ")
	tests := []struct {
		Index       int
		Lexeme      string
		StartLine   int
		StartColumn int
		EndLine     int
		EndColumn   int
	}{
		{0, "a", 1, 1, 1, 2},
		{1, "b", 1, 6, 1, 7},
		{2, "c", 1, 11, 1, 12},
		{3, "1234", 2, 1, 2, 5},
		{4, "56", 2, 9, 2, 11},
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	sc := Scan(src, ctx)

	var tokens []token.Token
	for {
		tok := sc.NextToken()
		tokens = append(tokens, tok)
		if tok.Kind == token.EOF {
			break
		}
	}

	for _, tt := range tests {
		tok := tokens[tt.Index]
		if tok.Lexeme != tt.Lexeme {
			t.Errorf("token %d: expected lexeme %q, got %q", tt.Index, tt.Lexeme, tok.Lexeme)
		}

		r := ctx.Source.Span(file, tok.Pos, tok.End)

		start := r.Start
		end := r.End

		if start.Line != tt.StartLine || start.Column != tt.StartColumn {
			t.Errorf("token %d (%q): expected start %d:%d, got %d:%d",
				tt.Index, tt.Lexeme, tt.StartLine, tt.StartColumn, start.Line, start.Column)
		}

		if end.Line != tt.EndLine || end.Column != tt.EndColumn {
			t.Errorf("token %d (%q): expected end %d:%d, got %d:%d",
				tt.Index, tt.Lexeme, tt.EndLine, tt.EndColumn, end.Line, end.Column)
		}
	}
}

func TestMultiLineRangeError(t *testing.T) {
	src := []byte(`
MODULE Test;
VAR x: STRING;
BEGIN
  x := "This string literal
continues without closing quote
and causes an error";
END Test.
`)

	file := "test.obx"
	tabWidth := 4

	// Initialize diagnostic tools
	sm := report.NewSourceManager()
	sm.Load(file, []byte(src), tabWidth)

	reporter := report.NewBufferedReporter(sm, 25, report.StdoutSink{
		Source: sm,
		Writer: os.Stdout,
	})

	// Define the range (manually computed for demo purposes)
	startOffset := bytes.Index([]byte(src), []byte(`"`))                   // Start at quote
	endOffset := bytes.Index([]byte(src), []byte(`error"`)) + len("error") // End at "error"

	reporter.Report(report.Diagnostic{
		Severity: report.Error,
		Message:  "unterminated string literal",
		Range:    sm.Span(file, startOffset, endOffset),
	})

	// Print diagnostic
	reporter.Flush()

}
