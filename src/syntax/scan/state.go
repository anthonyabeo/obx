package scan

import (
	"strconv"
	"strings"
	"unicode"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

type StateFn func(*Scanner) StateFn

func scanIdentifier(s *Scanner) StateFn {
	// The first character must be a letter or '_'
	r := s.peek()
	if !s.isLetter(r) && !s.isDecDigit(r) && r != '_' {
		return s.errorf("invalid identifier: must start with letter or '_'")
	}

	// Consume the rest: letters, digits, or underscores
	for {
		r = s.peek()
		if s.isLetter(r) || s.isDecDigit(r) || r == '_' {
			s.next()
		} else {
			break
		}
	}

	lexeme := strings.ToLower(string(s.content[s.start:s.pos]))
	kind := token.Lookup(lexeme)

	s.emit(kind, s.start, s.pos)

	return scanText
}

func scanNumber(s *Scanner) StateFn {
	isReal := false
	scaleChar := rune(0) // 'E', 'D', or 'S'

	// Require at least one digit
	if !s.acceptDigits("0123456789") {
		return s.errorf("malformed number literal: expected digit")
	}

	midPos := s.pos
	s.acceptRun("0123456789AaBbCcDdEeFf") // possible hex digits
	if s.accept("Hh") {
		if s.accept("Ll") {
			s.emit(token.INT64_LIT, s.start, s.pos)
			return scanText
		}

		if s.accept("Ii") {
			s.emit(token.INT32_LIT, s.start, s.pos)
			return scanText
		}

		// Ensure all characters before 'H' were valid hex digits
		if !isValidHex(string(s.content[s.start:midPos])) {
			return s.errorf("malformed number: invalid hex digits before 'H'")
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after hex literal", s.peek())
		}

		s.emit(token.INT_LIT, s.start, s.pos)
		return scanText
	}

	if s.accept("Xx") {
		// extract the character before 'X'
		hexValue := string(s.content[s.start : s.pos-1])

		// Ensure all characters before 'X' were valid hex digits
		if !isValidHex(hexValue) {
			return s.errorf("malformed number: invalid hex digits before 'X'")
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after char literal", s.peek())
		}

		var value int
		if len(hexValue) == 2 {
			// 8-bit value (ISO/IEC 8859-1 Latin-1)
			parsed, err := strconv.ParseInt(hexValue, 16, 8)
			if err != nil {
				return s.errorf("malformed number: invalid 8-bit character value: %s", hexValue)
			}
			value = int(parsed)
		} else if len(hexValue) == 4 {
			// 16-bit value (Unicode BMP)
			parsed, err := strconv.ParseInt(hexValue, 16, 16)
			if err != nil {
				return s.errorf("malformed number: invalid 16-bit character value: %s", hexValue)
			}
			value = int(parsed)
		} else {
			return s.errorf("malformed number: invalid character literal, hex value must be 2 or 4 digits")
		}

		// Convert the value to a rune
		character := rune(value)

		// Emit the character token with its Unicode code point
		s.emitWithValue(token.CHAR_LIT, string(character), s.start, s.pos)

		return scanText
	}
	s.pos = midPos // backtrack if not a hex

	// Check for decimal point (required for real)
	if s.accept(".") {
		isReal = true
		if !s.acceptDigits("0123456789") {
			return s.errorf("malformed number: invalid real number: no digits after decimal point")
		}
	}

	// Check for exponent
	if s.accept("EeDdSs") {
		isReal = true
		scaleChar = rune(s.content[s.pos-1])
		s.accept("+-")
		if !s.acceptDigits("0123456789") {
			return s.errorf("malformed number: invalid exponent: expected digit after exponent")
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after real literal", s.peek())
		}
	}

	// Optional suffix for integer
	if !isReal {
		if s.accept("Ll") {
			s.emit(token.INT64_LIT, s.start, s.pos)
			return scanText
		}

		if s.accept("Ii") {
			s.emit(token.INT32_LIT, s.start, s.pos)
			return scanText
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number literal. '%c' after integer literal", s.peek())
		}

		s.emit(token.INT_LIT, s.start, s.pos)
		return scanText
	}

	// Real number type decision
	switch scaleChar {
	case 'D', 'd':
		s.emit(token.LONGREAL_LIT, s.start, s.pos)
	case 'S', 's':
		s.emit(token.REAL_LIT, s.start, s.pos)
	case 'E', 'e':
		fullNum := string(s.content[s.start:s.pos])
		if canFitInFloat32FromString(fullNum, string(scaleChar)) {
			s.emit(token.REAL_LIT, s.start, s.pos)
		} else {
			s.emit(token.LONGREAL_LIT, s.start, s.pos)
		}
	default:
		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number literal. '%c' after real literal", s.peek())
		}

		s.emit(token.REAL_LIT, s.start, s.pos)
	}

	return scanText
}

func scanHexString(s *Scanner) StateFn {
	// First, check for the opening dollar sign
	if s.next() != '$' {
		return s.errorf("expected '$' to start hex string")
	}

	var hexDigits []rune // to store the hex digits

	// Loop through the characters in the string
	for {
		r := s.next()

		// If we reach EOF, the string is unterminated
		if r == eof {
			return s.errorf("unterminated hex string")
		}

		if unicode.IsSpace(r) {
			// Ignore whitespace characters
			continue
		}

		// If we encounter a closing dollar sign, finish the scan
		if r == '$' {
			// Check if we have an even number of hex digits
			if len(hexDigits)%2 != 0 {
				return s.errorf("hex string must have an even number of hex digits")
			}

			// Emit the hex string as a token
			s.emitWithValue(token.HEX_STR_LIT, string(hexDigits), s.start, s.pos)
			return scanText
		}

		// Check if the character is a valid hex digit
		if !s.isHexDigit(r) {
			return s.errorf("invalid hex digit: %q", r)
		}

		// Add the valid hex digit to the hexDigits slice
		hexDigits = append(hexDigits, r)
	}
}

func scanString(s *Scanner) StateFn {
	quote := s.next() // get the opening quote (either ' or ")
	if quote != '"' && quote != '\'' {
		return s.errorf("invalid string start: expected ' or \"")
	}

	var value []rune // to store the string content

	for {
		r := s.next()

		switch r {
		case eof:
			return s.errorf("unterminated string")
		case '\n':
			return s.errorf("string must not span multiple lines")
		case '\\': // handle escape sequences
			// Peek the next character to determine what to escape
			next := s.peek()
			switch next {
			case 'n':
				value = append(value, '\n')
				s.next() // consume 'n'
			case 'r':
				value = append(value, '\r')
				s.next() // consume 'r'
			case 't':
				value = append(value, '\t')
				s.next() // consume 't'
			case '\\':
				value = append(value, '\\')
				s.next() // consume '\\'
			case '\'', '"':
				// Escaped quote
				value = append(value, r)
				if next == quote {
					return s.errorf("quote cannot be the same as the string delimiter")
				}

				s.next() // consume '\'
			//case '"':
			//	// Escaped quote for double-quoted string
			//	value = append(value, '"')
			//	s.next() // consume '\'
			default:
				// If it's an unsupported escape, just add the backslash as it is
				value = append(value, '\\', next)
				s.next() // consume the character
			}
		case quote: // closing quote

			// Emit the string content (without quotes)
			s.emitWithValue(token.STR_LIT, string(value), s.start, s.pos)
			return scanText
		default:
			// Just a regular character inside the string
			value = append(value, r)
		}
	}
}

func scanText(s *Scanner) StateFn {
	for ch := s.next(); ch != eof; {
		switch ch {
		case '.':
			c := s.peek()
			if c == '.' {
				s.next()
				s.emit(token.RANGE, s.start, s.pos)
			} else {
				s.emit(token.PERIOD, s.start, s.pos)
			}
		case '-':
			s.emit(token.MINUS, s.start, s.pos)
		case '+':
			s.emit(token.PLUS, s.start, s.pos)
		case '=':
			s.emit(token.EQUAL, s.start, s.pos)
		case '<':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.LEQ, s.start, s.pos)
			} else {
				s.emit(token.LESS, s.start, s.pos)
			}
		case '>':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.GEQ, s.start, s.pos)
			} else {
				s.emit(token.GREAT, s.start, s.pos)
			}
		case '&':
			s.emit(token.AND, s.start, s.pos)
		case '|':
			s.emit(token.BAR, s.start, s.pos)
		case '#':
			s.emit(token.NEQ, s.start, s.pos)
		case '^':
			s.emit(token.CARET, s.start, s.pos)
		case ':':
			c := s.peek()
			if c == '=' {
				s.next()
				s.emit(token.BECOMES, s.start, s.pos)
			} else {
				s.emit(token.COLON, s.start, s.pos)
			}
		case '/':
			c := s.peek()
			if c == '/' {
				s.next()
				s.emit(token.SL_COMMENT_START, s.start, s.pos)
			} else {
				s.emit(token.QUOT, s.start, s.pos)
			}
		case '*':
			c := s.peek()
			if c == ')' {
				s.next()
				s.emit(token.ML_COMMENT_END, s.start, s.pos)
			} else {
				s.emit(token.STAR, s.start, s.pos)
			}
		case '{':
			s.emit(token.LBRACE, s.start, s.pos)
		case '}':
			s.emit(token.RBRACE, s.start, s.pos)
		case '[':
			s.emit(token.LBRACK, s.start, s.pos)
		case ']':
			s.emit(token.RBRACK, s.start, s.pos)
		case '(':
			c := s.peek()
			if c == '*' {
				s.next()
				s.emit(token.ML_COMMENT_START, s.start, s.pos)
			} else {
				s.emit(token.LPAREN, s.start, s.pos)
			}
		case ')':
			s.emit(token.RPAREN, s.start, s.pos)
		case ',':
			s.emit(token.COMMA, s.start, s.pos)
		case '~':
			s.emit(token.NOT, s.start, s.pos)
		case ';':
			s.emit(token.SEMICOLON, s.start, s.pos)
		case ' ', '\t':
			s.ignore()
		case '\n':
			s.emit(token.NEWLINE, s.start, s.pos)
		case '$':
			s.backup()
			return scanHexString
		case '\'', '"':
			s.backup()
			return scanString
		default:
			if s.startsIdent(ch) {
				s.backup()
				return scanIdentifier
			} else if s.isDecDigit(ch) {
				s.backup()
				return scanNumber
			} else {
				s.ignore()
				return s.errorf("invalid character %c", ch)
			}
		}

		ch = s.next()
	}

	s.emit(token.EOF, s.start, s.pos)

	return scanText
}
