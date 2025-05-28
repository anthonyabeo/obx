package scan

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestScanNumber(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		// Valid cases
		{"123", token.INT_LIT, false},
		{"97", token.INT_LIT, false},
		{"97L", token.INT64_LIT, false},
		{"97938l", token.INT64_LIT, false},
		{"0123", token.INT_LIT, false},
		{"123L", token.INT64_LIT, false},
		{"123I", token.INT32_LIT, false},
		{"123l", token.INT64_LIT, false},
		{"123i", token.INT32_LIT, false},
		{"1A2BH", token.INT_LIT, false},
		{"1A2BHL", token.INT64_LIT, false},
		{"1A2BHI", token.INT32_LIT, false},
		{"0", token.INT_LIT, false},
		{"0L", token.INT64_LIT, false},
		{"0I", token.INT32_LIT, false},
		{"0l", token.INT64_LIT, false},
		{"0i", token.INT32_LIT, false},
		{"0FFH", token.INT_LIT, false},
		{"0FFHL", token.INT64_LIT, false},
		{"0FFHI", token.INT32_LIT, false},
		{"0FFhL", token.INT64_LIT, false},
		{"0FFhI", token.INT32_LIT, false},
		{"0FFhl", token.INT64_LIT, false},
		{"0FFhi", token.INT32_LIT, false},
		{"9", token.INT_LIT, false},
		{"9L", token.INT64_LIT, false},
		{"9I", token.INT32_LIT, false},
		{"9l", token.INT64_LIT, false},
		{"9i", token.INT32_LIT, false},
		{"0FFFFFFFFH", token.INT_LIT, false},
		{"123.456", token.REAL_LIT, false},
		{"123.456E2", token.REAL_LIT, false},
		{"123.456e-2", token.REAL_LIT, false},
		{"123.456D2", token.LONGREAL_LIT, false},
		{"123.456S-2", token.REAL_LIT, false},
		{"0.0", token.REAL_LIT, false},       // zero as a real number
		{"0.0E0", token.REAL_LIT, false},     // zero with exponent
		{"0.0D0", token.LONGREAL_LIT, false}, // zero with long real exponent
		{"123.0", token.REAL_LIT, false},     // integer with decimal point
		{"0H", token.INT_LIT, false},         // minimal valid hex
		{"0h", token.INT_LIT, false},         // minimal valid hex
		{"123.456D2", token.LONGREAL_LIT, false},
		{"123.456S-2", token.REAL_LIT, false},
		{"123.456E2147483648", token.LONGREAL_LIT, false},  // exponent larger than 32 bits
		{"123.456e-2147483649", token.LONGREAL_LIT, false}, // negative exponent larger than 32 bits

		// Error cases
		{"ABCDH", token.IDENTIFIER, false},
		{"0H0", token.ILLEGAL, true},         // hex with trailing zero
		{"1A2B", token.ILLEGAL, true},        // missing hex marker
		{"123G", token.ILLEGAL, true},        // invalid size specifier
		{"0FFG", token.ILLEGAL, true},        // invalid hex marker
		{"XYZ", token.IDENTIFIER, false},     // invalid number format
		{"1A2BHK", token.ILLEGAL, true},      // invalid size specifier
		{"1A2BHG", token.ILLEGAL, true},      // invalid char after hex
		{"", token.EOF, false},               // empty input
		{"123.", token.ILLEGAL, true},        // missing decimal digits
		{"123.456E", token.ILLEGAL, true},    // missing exponent digits
		{"123.456E+", token.ILLEGAL, true},   // missing exponent digits after sign
		{"123..456", token.ILLEGAL, true},    // multiple decimal points
		{"123.456.789", token.ILLEGAL, true}, // multiple decimal points
		{"0H123", token.ILLEGAL, true},       // invalid characters after hex marker
		{"123.456E-+2", token.ILLEGAL, true}, // invalid exponent sign sequence
		{"123.456E2.3", token.ILLEGAL, true}, // invalid characters after exponent
		{"123.456D", token.ILLEGAL, true},    // missing digits after long real marker
		{"123.456S", token.ILLEGAL, true},    // missing digits after short real marker
		{"123.456E-", token.ILLEGAL, true},   // missing digits after negative exponent
		{"0H.", token.ILLEGAL, true},         // invalid character after hex marker
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanIdentifiers(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"main", token.IDENTIFIER, false},                           // simple identifier
		{"_main", token.IDENTIFIER, false},                          // starts with underscore
		{"main123", token.IDENTIFIER, false},                        // contains digits
		{"_123main", token.IDENTIFIER, false},                       // starts with underscore, contains digits
		{"a_b_c", token.IDENTIFIER, false},                          // contains underscores
		{"a123_b456", token.IDENTIFIER, false},                      // mixed letters, digits, and underscores
		{"_a_b_c_", token.IDENTIFIER, false},                        // starts and ends with underscores
		{"ABCDEFGHIJKLMNOPQRSTUVWXYZ", token.IDENTIFIER, false},     // all uppercase letters
		{"abcdefghijklmnopqrstuvwxyz", token.IDENTIFIER, false},     // all lowercase letters
		{"_ABCDEFGHIJKLMNOPQRSTUVWXYZ123", token.IDENTIFIER, false}, // mixed case with digits
		{"123main", token.ILLEGAL, true},                            // starts with a digit
		{"123", token.INT_LIT, false},                               // only digits
		{"!main", token.ILLEGAL, true},                              // starts with invalid character
		{"", token.EOF, false},                                      // empty input
		{"_", token.IDENTIFIER, false},                              // single underscore
		{"_123", token.IDENTIFIER, false},                           // underscore followed by digits
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()
		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanDelimitersAndOperators(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"(", token.LPAREN, false},
		{")", token.RPAREN, false},
		{"[", token.LBRACK, false},
		{"]", token.RBRACK, false},
		{"{", token.LBRACE, false},
		{"}", token.RBRACE, false},
		{",", token.COMMA, false},
		{";", token.SEMICOLON, false},
		{"+", token.PLUS, false},
		{"-", token.MINUS, false},
		{"*", token.STAR, false},
		{"/", token.QUOT, false},
		{"=", token.EQUAL, false},
		{"#", token.NEQ, false},
		{"<", token.LESS, false},
		{"<=", token.LEQ, false},
		{">", token.GREAT, false},
		{">=", token.GEQ, false},
		{"~", token.NOT, false},
		{"&", token.AND, false},
		{"|", token.BAR, false},
		{"^", token.CARET, false},
		{"//", token.SL_COMMENT_START, false},
		{"(*", token.ML_COMMENT_START, false},
		{"*)", token.ML_COMMENT_END, false},
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()
		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanHexStrings(t *testing.T) {
	tests := []struct {
		input         string
		expected      token.Kind
		expectedValue string
		wantErr       bool
	}{
		{"$1A2B$", token.HEX_STR_LIT, "1A2B", false},                            // valid hex string
		{"$1234$", token.HEX_STR_LIT, "1234", false},                            // valid hex string with digits only
		{"$ABCD$", token.HEX_STR_LIT, "ABCD", false},                            // valid hex string with letters only
		{"$1A2B3C$", token.HEX_STR_LIT, "1A2B3C", false},                        // valid hex string with mixed letters and digits
		{"$1A 2B 3C$", token.HEX_STR_LIT, "1A2B3C", false},                      // valid hex string with spaces
		{"$1A5D 2BFF 23CC 3C56$", token.HEX_STR_LIT, "1A5D2BFF23CC3C56", false}, // valid hex string with spaces
		{"$ 1A 2B 3C $", token.HEX_STR_LIT, "1A2B3C", false},                    // valid hex string with leading and trailing spaces
		{"$1A2B3C", token.ILLEGAL, "", true},                                    // missing closing '$'
		{"1A2B3C$", token.ILLEGAL, "", true},                                    // missing opening '$'
		{"$1A2G$", token.ILLEGAL, "", true},                                     // invalid character in hex string
		{"$1A 2G$", token.ILLEGAL, "", true},                                    // invalid character with spaces in hex string
		{"$$", token.HEX_STR_LIT, "", false},                                    // empty hex string
		{"$1A2B3C4D$", token.HEX_STR_LIT, "1A2B3C4D", false},                    // Valid hex string with 4 bytes
		{"$A1B2C3D4$", token.HEX_STR_LIT, "A1B2C3D4", false},                    // Another valid hex string with 4 bytes
		{"$a1b2c3d4$", token.HEX_STR_LIT, "a1b2c3d4", false},                    // Lowercase hex string
		{"$1F3D$", token.HEX_STR_LIT, "1F3D", false},                            // Odd-length hex string (should be an error)
		{"$123 456$", token.HEX_STR_LIT, "123456", false},                       // Whitespace should be ignored
		{"$123G$", token.ILLEGAL, "", true},                                     // Invalid character 'G'
		{"$123$", token.ILLEGAL, "", true},                                      // Odd number of hex digits
		{"$123", token.ILLEGAL, "", true},                                       // Unterminated hex string
		{"$A B C$", token.ILLEGAL, "", true},                                    // Valid hex string with space ignored
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanCharacterLiterals(t *testing.T) {
	tests := []struct {
		input         string
		expected      token.Kind
		expectedValue string
		wantErr       bool
	}{
		// Valid character literals
		{"1Fx", token.CHAR_LIT, "A", false},   // 8-bit character 'A'
		{"0041x", token.CHAR_LIT, "A", false}, // 16-bit Unicode 'A'
		{"03A9x", token.CHAR_LIT, "Ω", false}, // Unicode 'Ω'
		{"7Fx", token.CHAR_LIT, "", false},   // Extended ASCII ''
		{"4F60x", token.CHAR_LIT, "你", false}, // Unicode '你' (Chinese character)

		// Invalid character literals
		{"1x", token.ILLEGAL, "", true},      // Too few hex digits
		{"G2x", token.IDENTIFIER, "", false}, // Invalid hex digit 'G'
		{"1F", token.ILLEGAL, "", true},      // Missing 'x'
		{"12345x", token.ILLEGAL, "", true},  // Too many hex digits
		{"1Fz", token.ILLEGAL, "", true},     // Invalid suffix 'z'
		{"x", token.IDENTIFIER, "", false},   // Empty literal
		{"1234", token.INT_LIT, "", false},   // Missing 'x'
		{"1F x", token.ILLEGAL, "", true},    // Whitespace between hex and 'x'
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()
		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanStringLiterals(t *testing.T) {
	tests := []struct {
		input       string
		wantToken   token.Kind
		wantLiteral string
		wantError   bool
	}{
		{"\"Hello, World!\"", token.STR_LIT, "Hello, World!", false},       // valid string literal
		{"'Hello, World!'", token.STR_LIT, "Hello, World!", false},         // valid single-quoted string
		{"\"\"", token.STR_LIT, "", false},                                 // empty double-quoted string
		{"''", token.STR_LIT, "", false},                                   // empty single-quoted string
		{"\"Hello, 'World!'\"", token.STR_LIT, "Hello, 'World!'", false},   // double-quoted string with single quotes inside
		{"'Hello, \"World!\"'", token.STR_LIT, "Hello, \"World!\"", false}, // single-quoted string with double quotes inside
		{"\"Hello\nWorld\"", token.ILLEGAL, "", true},                      // invalid string with newline
		{"'Hello\nWorld'", token.ILLEGAL, "", true},                        // invalid single-quoted string with newline
		{"\"Hello, World!", token.ILLEGAL, "", true},                       // missing closing double quote
		{"'Hello, World!", token.ILLEGAL, "", true},                        // missing closing single quote
		{`\"Hello\\\"World\"`, token.ILLEGAL, "", true},                    // escaped double quote inside string
		{"'Hello\\'World'", token.ILLEGAL, "", true},                       // escaped single quote inside string
		{"'hello'", token.STR_LIT, "hello", false},
		{`"world"`, token.STR_LIT, "world", false},
		{"''", token.STR_LIT, "", false},
		{`"123456"`, token.STR_LIT, "123456", false},
		{`'a!@#$%^&*()_'`, token.STR_LIT, "a!@#$%^&*()_", false},
		{`'"'`, token.STR_LIT, `"`, false},   // double quote inside single-quoted string
		{`'hello"`, token.ILLEGAL, "", true}, // mismatched quote
		{`"hello'`, token.ILLEGAL, "", true}, // mismatched quote
		{`'hello`, token.ILLEGAL, "", true},  // unterminated
		{`"hello`, token.ILLEGAL, "", true},
		{`'line
next'`, token.ILLEGAL, "", true}, // newline
		{`"'`, token.ILLEGAL, "", true}, // invalid single char
		{`'`, token.ILLEGAL, "", true},
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, tt := range tests {
		ctx.Content = []byte(tt.input)
		sc := Scan(ctx)

		tok := sc.NextToken()

		if tt.wantError {
			if tok.Kind != token.ILLEGAL {
				t.Errorf("input %q: expected an error, got %v", tt.input, tok.Kind)
			}
			continue
		}

		if tok.Kind != tt.wantToken {
			t.Errorf("input %q: expected token %v, got %v", tt.input, tt.wantToken, tok.Kind)
		}
		if tok.Lexeme != tt.wantLiteral {
			t.Errorf("input %q: expected literal %q, got %q", tt.input, tt.wantLiteral, tok.Lexeme)
		}
	}
}

func TestScanComments(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"// This is a single-line comment", token.SL_COMMENT_START, false},
		{"(* This is a multi-line comment *)", token.ML_COMMENT_START, false},
		{"(* This is a multi-line comment\n spanning multiple lines *)", token.ML_COMMENT_START, false},
		{"// Another single-line comment", token.SL_COMMENT_START, false},
		{"(* Another multi-line comment *)", token.ML_COMMENT_START, false},
	}

	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}

	for _, test := range tests {
		ctx.Content = []byte(test.input)
		sc := Scan(ctx)

		got := sc.NextToken()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextToken(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextToken(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestLexingOfMinimalObxProgram(t *testing.T) {
	input := []byte(`
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
`)
	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Content:  input,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}
	sc := Scan(ctx)

	tests := []struct {
		tokenKind token.Kind
		tokenLit  string
	}{
		{token.NEWLINE, "\n"},

		{token.MODULE, "module"},
		{token.IDENTIFIER, "Main"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.IDENTIFIER, "fib"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "n"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.RPAREN, ")"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.NEWLINE, "\n"},

		{token.VAR, "var"},
		{token.IDENTIFIER, "a"},
		{token.COMMA, ","},
		{token.IDENTIFIER, "b"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.BEGIN, "begin"},
		{token.NEWLINE, "\n"},

		{token.IF, "if"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "n"},
		{token.EQUAL, "="},
		{token.INT_LIT, "0"},
		{token.RPAREN, ")"},
		{token.OR, "or"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "n"},
		{token.EQUAL, "="},
		{token.INT_LIT, "1"},
		{token.RPAREN, ")"},
		{token.THEN, "then"},
		{token.NEWLINE, "\n"},

		{token.RETURN, "return"},
		{token.IDENTIFIER, "n"},
		{token.NEWLINE, "\n"},

		{token.ELSE, "else"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "a"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "fib"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "n"},
		{token.MINUS, "-"},
		{token.INT_LIT, "1"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "b"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "fib"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "n"},
		{token.MINUS, "-"},
		{token.INT_LIT, "2"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.RETURN, "return"},
		{token.IDENTIFIER, "a"},
		{token.PLUS, "+"},
		{token.IDENTIFIER, "b"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.IDENTIFIER, "fib"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.BEGIN, "begin"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "res"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "fib"},
		{token.LPAREN, "("},
		{token.INT_LIT, "21"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "assert"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "res"},
		{token.EQUAL, "="},
		{token.INT_LIT, "10946"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.IDENTIFIER, "Main"},
		{token.NEWLINE, "\n"},
	}

	for _, tt := range tests {
		got := sc.NextToken()
		if got.Kind != tt.tokenKind {
			t.Errorf(fmt.Sprintf("sc.NextToken(). Expected token kind = %v, Got %v", tt.tokenKind, got.Kind))
		}

		if got.Lexeme != tt.tokenLit {
			t.Errorf("sc.NextToken(). Expected token literal = %v, Got %v", tt.tokenLit, got.Lexeme)
		}
	}
}

func TestExampleOOPProgram(t *testing.T) {
	input := []byte(`module Drawing
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
`)
	file := "test.ob"
	ctx := &report.Context{
		FileName: file,
		Content:  input,
		Source:   report.NewSourceManager(),
		Reporter: nil,
		TabWidth: 4,
	}
	sc := Scan(ctx)

	tests := []struct {
		tokenKind token.Kind
		tokenLit  string
	}{
		{token.MODULE, "module"},
		{token.IDENTIFIER, "Drawing"},
		{token.NEWLINE, "\n"},
		{token.IMPORT, "import"},
		{token.IDENTIFIER, "F"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "Fibonacci"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "C"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "Collections"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "Figure"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.TYPE, "type"},
		{token.IDENTIFIER, "Figure"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "position"},
		{token.COLON, ":"},
		{token.RECORD, "record"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "x"},
		{token.COMMA, ","},
		{token.IDENTIFIER, "y"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "this"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Figure"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.TYPE, "type"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "Circle"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "Figure"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "diameter"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "Square"},
		{token.STAR, "*"},
		{token.EQUAL, "="},
		{token.POINTER, "pointer"},
		{token.TO, "to"},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "Figure"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "width"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "this"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Circle"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "this"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Square"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "draw"},
		{token.STAR, "*"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.END, "end"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.VAR, "var"},
		{token.IDENTIFIER, "figures"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "C"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "Deque"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "circle"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Circle"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "square"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Square"},
		{token.NEWLINE, "\n"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.IDENTIFIER, "drawAll"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.TYPE, "type"},
		{token.IDENTIFIER, "I"},
		{token.EQUAL, "="},
		{token.RECORD, "record"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "C"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "Iterator"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "count"},
		{token.COLON, ":"},
		{token.INTEGER, "integer"},
		{token.END, "end"},
		{token.NEWLINE, "\n"},

		{token.PROC, "proc"},
		{token.LPAREN, "("},
		{token.VAR, "var"},
		{token.IDENTIFIER, "this"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "I"},
		{token.RPAREN, ")"},
		{token.IDENTIFIER, "apply"},
		{token.LPAREN, "("},
		{token.IN, "in"},
		{token.IDENTIFIER, "figure"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "Figure"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.BEGIN, "begin"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "figure"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "draw"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.SEMICOLON, ";"},
		{token.IDENTIFIER, "inc"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "this"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "count"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.IDENTIFIER, "apply"},
		{token.NEWLINE, "\n"},

		{token.VAR, "var"},
		{token.IDENTIFIER, "i"},
		{token.COLON, ":"},
		{token.IDENTIFIER, "I"},
		{token.NEWLINE, "\n"},

		{token.BEGIN, "begin"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "figures"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "forEach"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "i"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "assert"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "i"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "count"},
		{token.EQUAL, "="},
		{token.INT_LIT, "2"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.IDENTIFIER, "drawAll"},
		{token.NEWLINE, "\n"},

		{token.BEGIN, "begin"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "figures"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "C"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "createDeque"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "new"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "circle"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "circle"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "position"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "x"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "F"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "calc"},
		{token.LPAREN, "("},
		{token.INT_LIT, "3"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "circle"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "position"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "y"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "F"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "calc"},
		{token.LPAREN, "("},
		{token.INT_LIT, "4"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "circle"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "diameter"},
		{token.BECOMES, ":="},
		{token.INT_LIT, "3"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "figures"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "append"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "circle"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "new"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "square"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "square"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "position"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "x"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "F"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "calc"},
		{token.LPAREN, "("},
		{token.INT_LIT, "5"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "square"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "position"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "y"},
		{token.BECOMES, ":="},
		{token.IDENTIFIER, "F"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "calc"},
		{token.LPAREN, "("},
		{token.INT_LIT, "6"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "square"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "width"},
		{token.BECOMES, ":="},
		{token.INT_LIT, "4"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "figures"},
		{token.PERIOD, "."},
		{token.IDENTIFIER, "append"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "square"},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.IDENTIFIER, "drawAll"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.NEWLINE, "\n"},

		{token.END, "end"},
		{token.IDENTIFIER, "Drawing"},
		{token.NEWLINE, "\n"},
	}

	for _, tt := range tests {
		got := sc.NextToken()
		if got.Kind != tt.tokenKind {
			t.Errorf(fmt.Sprintf("sc.NextToken(). Expected token kind = %v, Got %v", tt.tokenKind, got.Kind))
		}

		if got.Lexeme != tt.tokenLit {
			t.Errorf("sc.NextToken(). Expected token literal = %v, Got %v", tt.tokenLit, got.Lexeme)
		}
	}
}
