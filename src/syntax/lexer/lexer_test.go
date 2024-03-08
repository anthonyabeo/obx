package lexer

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestLexingHexStrings(t *testing.T) {
	input := `
const arrow = $0F0F 0060 0070 0038 001C 000E 0007 8003
			   C101 E300 7700 3F00 1F00 3F00 7F00 FF00$
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

	tests := []struct {
		kind token.Token
		lit  string
	}{
		{token.CONST, "const"},
		{token.IDENT, "arrow"},
		{token.EQUAL, "="},
		{token.HEXSTRING, "0F0F006000700038001C000E00078003C101E30077003F001F003F007F00FF00"},
	}

	for _, tt := range tests {
		tok, lit, _ := lexer.Lex()
		if tok != tt.kind {
			t.Errorf("lex.Lex(). Expected token kind = %v, Got %v", tt.kind, tok)
		}

		if lit != tt.lit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.lit, lit)
		}
	}
}

func TestLexingNumbers(t *testing.T) {
	input := `1234
0dh
0DH
12.3
4.567e8 
4.567E8
0.57712566d-6    
0.57712566D-6
1234I
1234i
8393L
492000003l
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

	tests := []struct {
		kind token.Token
		lit  string
	}{
		{token.INT, "1234"},
		{token.INT, "0dh"},
		{token.INT, "0DH"},
		{token.REAL, "12.3"},
		{token.LONGREAL, "4.567e8"},
		{token.LONGREAL, "4.567E8"},
		{token.LONGREAL, "0.57712566d-6"},
		{token.LONGREAL, "0.57712566D-6"},
		{token.INT32, "1234"},
		{token.INT32, "1234"},
		{token.INT64, "8393"},
		{token.INT64, "492000003"},
	}

	for _, tt := range tests {
		tok, lit, _ := lexer.Lex()
		if tok != tt.kind {
			t.Errorf("lex.Lex(). Expected token kind = %v, Got %v", tt.kind, tok)
		}

		if lit != tt.lit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.lit, lit)
		}
	}
}

func TestLexingWithTokenPositions(t *testing.T) {
	input := `module Main
begin

end Main
`
	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

	tests := []struct {
		kind   token.Token
		lit    string
		lineNo int
		colNo  int
	}{
		{token.MODULE, "module", 1, 1},
		{token.IDENT, "Main", 1, 8},
		{token.BEGIN, "begin", 2, 1},
		{token.END, "end", 4, 1},
		{token.IDENT, "Main", 4, 5},
	}

	for _, tt := range tests {
		tok, lit, pos := lexer.Lex()
		if tok != tt.kind {
			t.Errorf("lex.Lex(). Expected token kind = %v, Got %v", tt.kind, tok)
		}

		if lit != tt.lit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.lit, lit)
		}

		if pos.Column != tt.colNo {
			t.Errorf("lex.Lex(): Expected colNo %v, Got %v instead", tt.colNo, pos.Column)
		}

		if pos.Line != tt.lineNo {
			t.Errorf("lex.Lex(): Expected line number %v, Got %v instead", tt.lineNo, pos.Line)
		}

	}

}

func TestLexingOfIdentifiers(t *testing.T) {
	input := `
x
Scan
Oberon_2
_y
firstLetter
`
	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

	tests := []struct {
		kind   token.Token
		lit    string
		lineNo int
		colNo  int
	}{
		{token.IDENT, "x", 2, 1},
		{token.IDENT, "Scan", 3, 1},
		{token.IDENT, "Oberon_2", 4, 1},
		{token.IDENT, "_y", 5, 1},
		{token.IDENT, "firstLetter", 6, 1},
	}

	for _, tt := range tests {
		tok, lit, pos := lexer.Lex()
		if tok != tt.kind {
			t.Errorf("lex.Lex(). Expected token kind = %v, Got %v", tt.kind, tok)
		}

		if lit != tt.lit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.lit, lit)
		}

		if pos.Column != tt.colNo {
			t.Errorf("lex.Lex(): Expected colNo %v, Got %v instead", tt.colNo, pos.Column)
		}

		if pos.Line != tt.lineNo {
			t.Errorf("lex.Lex(): Expected line number %v, Got %v instead", tt.lineNo, pos.Line)
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

	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

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
		{token.BECOMES, ":="},
		{token.IDENT, "fib"},
		{token.LPAREN, "("},
		{token.IDENT, "n"},
		{token.MINUS, "-"},
		{token.INT, "1"},
		{token.RPAREN, ")"},

		{token.IDENT, "b"},
		{token.BECOMES, ":="},
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
		{token.BECOMES, ":="},
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
		tok, lit, _ := lexer.Lex()
		if tok != tt.tokenKind {
			t.Errorf(fmt.Sprintf("lex.Lex(). Expected token kind = %v, Got %v", tt.tokenKind, tok))
		}

		if lit != tt.tokenLit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.tokenLit, lit)
		}
	}
}

func TestExampleOOPProgram(t *testing.T) {
	input := `module Drawing
  import F := Fibonacci
         C := Collections(Figure)

  type Figure* = pointer to record
                   position: record
                     x,y: integer 
                   end 
				 end
  proc (this: Figure) draw*() end

  type
     Circle* = pointer to record (Figure)
                  diameter: integer 
			   end
     Square* = pointer to record (Figure)
                          width: integer end
  proc (this: Circle) draw*() end
  proc (this: Square) draw*() end

  var figures: C.Deque
       circle: Circle
       square: Square

  proc drawAll()
    type I = record(C.Iterator) count: integer end
    proc (var this: I) apply( in figure: Figure )
    begin
      figure.draw(); inc(this.count)
    end apply
    var i: I
  begin
    figures.forEach(i)
    assert(i.count = 2)
  end drawAll
begin
  figures := C.createDeque()
  new(circle)
  circle.position.x := F.calc(3)
  circle.position.y := F.calc(4)
  circle.diameter := 3
  figures.append(circle)
  new(square)
  square.position.x := F.calc(5)
  square.position.y := F.calc(6)
  square.width := 4
  figures.append(square)
  drawAll()
end Drawing
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lexer := NewLexer(file, []byte(input))

	tests := []struct {
		tokenKind token.Token
		tokenLit  string
	}{
		{token.MODULE, "module"},
		{token.IDENT, "Drawing"},
		{token.IMPORT, "import"},
		{token.IDENT, "F"},
		{token.BECOMES, ":="},
		{token.IDENT, "Fibonacci"},
		{token.IDENT, "C"},
		{token.BECOMES, ":="},
		{token.IDENT, "Collections"},
		{token.LPAREN, "("},
		{token.IDENT, "Figure"},
		{token.RPAREN, ")"},
		{token.TYPE, "type"},
		{token.IDENT, "Figure"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.IDENT, "position"},
		{token.COLON, ":"},
		{token.RECORD, "record"},
		{token.IDENT, "x"},
		{token.COMMA, ","},
		{token.IDENT, "y"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},
		{token.END, "end"},
		{token.END, "end"},
		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENT, "this"},
		{token.COLON, ":"},
		{token.IDENT, "Figure"},
		{token.RPAREN, ")"},
		{token.IDENT, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.TYPE, "type"},
		{token.IDENT, "Circle"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENT, "Figure"},
		{token.RPAREN, ")"},
		{token.IDENT, "diameter"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},
		{token.END, "end"},
		{token.IDENT, "Square"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENT, "Figure"},
		{token.RPAREN, ")"},
		{token.IDENT, "width"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},
		{token.END, "end"},
		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENT, "this"},
		{token.COLON, ":"},
		{token.IDENT, "Circle"},
		{token.RPAREN, ")"},
		{token.IDENT, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENT, "this"},
		{token.COLON, ":"},
		{token.IDENT, "Square"},
		{token.RPAREN, ")"},
		{token.IDENT, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.VAR, "var"},
		{token.IDENT, "figures"},
		{token.COLON, ":"},
		{token.IDENT, "C"},
		{token.PERIOD, "."},
		{token.IDENT, "Deque"},
		{token.IDENT, "circle"},
		{token.COLON, ":"},
		{token.IDENT, "Circle"},
		{token.IDENT, "square"},
		{token.COLON, ":"},
		{token.IDENT, "Square"},
		{token.PROC, "proc"},
		{token.IDENT, "drawAll"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.TYPE, "type"},
		{token.IDENT, "I"},
		{token.EQUAL, "="},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENT, "C"},
		{token.PERIOD, "."},
		{token.IDENT, "Iterator"},
		{token.RPAREN, ")"},
		{token.IDENT, "count"},
		{token.COLON, ":"},
		{token.IDENT, "integer"},
		{token.END, "end"},
		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.VAR, "var"},
		{token.IDENT, "this"},
		{token.COLON, ":"},
		{token.IDENT, "I"},
		{token.RPAREN, ")"},
		{token.IDENT, "apply"},
		{token.LPAREN, "("},
		{token.IN, "in"},
		{token.IDENT, "figure"},
		{token.COLON, ":"},
		{token.IDENT, "Figure"},
		{token.RPAREN, ")"},
		{token.BEGIN, "begin"},
		{token.IDENT, "figure"},
		{token.PERIOD, "."},
		{token.IDENT, "draw"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.SEMICOLON, ";"},
		{token.IDENT, "inc"},
		{token.LPAREN, "("},
		{token.IDENT, "this"},
		{token.PERIOD, "."},
		{token.IDENT, "count"},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.IDENT, "apply"},
		{token.VAR, "var"},
		{token.IDENT, "i"},
		{token.COLON, ":"},
		{token.IDENT, "I"},
		{token.BEGIN, "begin"},
		{token.IDENT, "figures"},
		{token.PERIOD, "."},
		{token.IDENT, "forEach"},
		{token.LPAREN, "("},
		{token.IDENT, "i"},
		{token.RPAREN, ")"},
		{token.IDENT, "assert"},
		{token.LPAREN, "("},
		{token.IDENT, "i"},
		{token.PERIOD, "."},
		{token.IDENT, "count"},
		{token.EQUAL, "="},
		{token.INT, "2"},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.IDENT, "drawAll"},
		{token.BEGIN, "begin"},
		{token.IDENT, "figures"},
		{token.BECOMES, ":="},
		{token.IDENT, "C"},
		{token.PERIOD, "."},
		{token.IDENT, "createDeque"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.IDENT, "new"},
		{token.LPAREN, "("},
		{token.IDENT, "circle"},
		{token.RPAREN, ")"},
		{token.IDENT, "circle"},
		{token.PERIOD, "."},
		{token.IDENT, "position"},
		{token.PERIOD, "."},
		{token.IDENT, "x"},
		{token.BECOMES, ":="},
		{token.IDENT, "F"},
		{token.PERIOD, "."},
		{token.IDENT, "calc"},
		{token.LPAREN, "("},
		{token.INT, "3"},
		{token.RPAREN, ")"},
		{token.IDENT, "circle"},
		{token.PERIOD, "."},
		{token.IDENT, "position"},
		{token.PERIOD, "."},
		{token.IDENT, "y"},
		{token.BECOMES, ":="},
		{token.IDENT, "F"},
		{token.PERIOD, "."},
		{token.IDENT, "calc"},
		{token.LPAREN, "("},
		{token.INT, "4"},
		{token.RPAREN, ")"},
		{token.IDENT, "circle"},
		{token.PERIOD, "."},
		{token.IDENT, "diameter"},
		{token.BECOMES, ":="},
		{token.INT, "3"},
		{token.IDENT, "figures"},
		{token.PERIOD, "."},
		{token.IDENT, "append"},
		{token.LPAREN, "("},
		{token.IDENT, "circle"},
		{token.RPAREN, ")"},
		{token.IDENT, "new"},
		{token.LPAREN, "("},
		{token.IDENT, "square"},
		{token.RPAREN, ")"},
		{token.IDENT, "square"},
		{token.PERIOD, "."},
		{token.IDENT, "position"},
		{token.PERIOD, "."},
		{token.IDENT, "x"},
		{token.BECOMES, ":="},
		{token.IDENT, "F"},
		{token.PERIOD, "."},
		{token.IDENT, "calc"},
		{token.LPAREN, "("},
		{token.INT, "5"},
		{token.RPAREN, ")"},
		{token.IDENT, "square"},
		{token.PERIOD, "."},
		{token.IDENT, "position"},
		{token.PERIOD, "."},
		{token.IDENT, "y"},
		{token.BECOMES, ":="},
		{token.IDENT, "F"},
		{token.PERIOD, "."},
		{token.IDENT, "calc"},
		{token.LPAREN, "("},
		{token.INT, "6"},
		{token.RPAREN, ")"},
		{token.IDENT, "square"},
		{token.PERIOD, "."},
		{token.IDENT, "width"},
		{token.BECOMES, ":="},
		{token.INT, "4"},
		{token.IDENT, "figures"},
		{token.PERIOD, "."},
		{token.IDENT, "append"},
		{token.LPAREN, "("},
		{token.IDENT, "square"},
		{token.RPAREN, ")"},
		{token.IDENT, "drawAll"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.IDENT, "Drawing"},
	}

	for _, tt := range tests {
		tok, lit, _ := lexer.Lex()
		if tok != tt.tokenKind {
			t.Errorf(fmt.Sprintf("lex.Lex(). Expected token kind = %v, Got %v", tt.tokenKind, tok))
		}

		if lit != tt.tokenLit {
			t.Errorf("lex.Lex(). Expected token literal = %v, Got %v", tt.tokenLit, lit)
		}
	}
}
