package scan

import (
	"testing"

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

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
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

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
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

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanHexStrings(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"$1A2B$", token.HEX_STR_LIT, false},                // valid hex string
		{"$1234$", token.HEX_STR_LIT, false},                // valid hex string with digits only
		{"$ABCD$", token.HEX_STR_LIT, false},                // valid hex string with letters only
		{"$1A2B3C$", token.HEX_STR_LIT, false},              // valid hex string with mixed letters and digits
		{"$1A 2B 3C$", token.HEX_STR_LIT, false},            // valid hex string with spaces
		{"$1A5D 2BFF 23CC 3C56$", token.HEX_STR_LIT, false}, // valid hex string with spaces
		{"$ 1A 2B 3C $", token.HEX_STR_LIT, false},          // valid hex string with leading and trailing spaces
		{"$1A2B3C", token.ILLEGAL, true},                    // missing closing '$'
		{"1A2B3C$", token.ILLEGAL, true},                    // missing opening '$'
		{"$1A2G$", token.ILLEGAL, true},                     // invalid character in hex string
		{"$1A 2G$", token.ILLEGAL, true},                    // invalid character with spaces in hex string
		{"$$", token.HEX_STR_LIT, false},                    // empty hex string
	}

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanCharacterLiterals(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"1X", token.CHAR_LIT, false},    // valid character literal with digit and 'X'
		{"1x", token.CHAR_LIT, false},    // valid character literal with digit and 'x'
		{"123X", token.CHAR_LIT, false},  // valid character literal with digits and 'X'
		{"123x", token.CHAR_LIT, false},  // valid character literal with digits and 'x'
		{"1A2BX", token.CHAR_LIT, false}, // valid character literal with hex digits and 'X'
		{"1A2Bx", token.CHAR_LIT, false}, // valid character literal with hex digits and 'x'
		{"1Gx", token.ILLEGAL, true},     // invalid character literal with non-hex digit
		{"123", token.INT_LIT, false},    // missing 'X' or 'x'
		{"1A2B", token.ILLEGAL, true},    // missing 'X' or 'x' after hex digits
		{"", token.EOF, false},           // empty input
		{"X", token.IDENTIFIER, false},   // single 'X' without preceding digits
		{"x", token.IDENTIFIER, false},   // single 'x' without preceding digits
		{"1X2", token.ILLEGAL, true},     // invalid character after 'X'
		{"1x2", token.ILLEGAL, true},     // invalid character after 'x'
		{"1Xx", token.ILLEGAL, true},     // mixed 'X' and 'x'
		{"1xX", token.ILLEGAL, true},     // mixed 'x' and 'X'
		{"1X ", token.ILLEGAL, true},     // trailing space after 'X'
		{"1x ", token.ILLEGAL, true},     // trailing space after 'x'
		{"1 X", token.ILLEGAL, true},     // space between digit and 'X'
		{"1 x", token.ILLEGAL, true},     // space between digit and 'x'
		{"1X\n", token.ILLEGAL, true},    // newline after 'X'
		{"1x\n", token.ILLEGAL, true},    // newline after 'x'    // missing 'X' or 'x' after hex digits
	}

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}

func TestScanStringLiterals(t *testing.T) {
	tests := []struct {
		input    string
		expected token.Kind
		wantErr  bool
	}{
		{"\"Hello, World!\"", token.STR_LIT, false},   // valid string literal
		{"'Hello, World!'", token.STR_LIT, false},     // valid single-quoted string
		{"\"\"", token.STR_LIT, false},                // empty double-quoted string
		{"''", token.STR_LIT, false},                  // empty single-quoted string
		{"\"Hello, 'World!'\"", token.STR_LIT, false}, // double-quoted string with single quotes inside
		{"'Hello, \"World!\"'", token.STR_LIT, false}, // single-quoted string with double quotes inside
		{"\"Hello\nWorld\"", token.ILLEGAL, true},     // invalid string with newline
		{"'Hello\nWorld'", token.ILLEGAL, true},       // invalid single-quoted string with newline
		{"\"Hello, World!", token.ILLEGAL, true},      // missing closing double quote
		{"'Hello, World!", token.ILLEGAL, true},       // missing closing single quote
		{"\"Hello\\\"World\"", token.STR_LIT, false},  // escaped double quote inside string
		{"'Hello\\'World'", token.STR_LIT, false},     // escaped single quote inside string
	}

	for _, test := range tests {
		sc := Scan("", test.input)
		got := sc.NextItem()

		if test.wantErr {
			if got.Kind != token.ILLEGAL {
				t.Errorf("NextItem(%q) = %v; want error", test.input, got.Kind)
			}
		} else if got.Kind != test.expected {
			t.Errorf("NextItem(%q) = %v; want %v", test.input, got.Kind, test.expected)
		}
	}
}
