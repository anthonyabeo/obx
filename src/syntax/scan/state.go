package scan

import (
	"strconv"
	"strings"
	"unicode"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type StateFn func(*Scanner) StateFn

func scanIdentifier(s *Scanner) StateFn {
	// The first character must be a letter or '_'
	r := s.peek()
	if !s.isLetter(r) && !s.isDecDigit(r) && r != '_' {
		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}
		return s.errorf("invalid identifier: must start with letter or '_'", rng)
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

	lexeme := strings.ToLower(string(s.src.Content[s.start:s.pos]))
	kind := token.Lookup(lexeme)

	rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
	if err != nil {
		panic(err.Error())
	}

	s.emit(kind, rng)

	return scanText
}

func scanNumber(s *Scanner) StateFn {
	isReal := false
	scaleChar := rune(0) // 'E', 'D', or 'S'

	rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
	if err != nil {
		panic(err.Error())
	}

	// Require at least one digit
	if !s.acceptDigits("0123456789") {
		return s.errorf("malformed number literal: expected digit", rng)
	}

	midPos := s.pos
	s.acceptRun("0123456789AaBbCcDdEeFf") // possible hex digits
	if s.accept("Hh") {
		if s.accept("Ll") {
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.INT64_LIT, rng)
			return scanText
		}

		if s.accept("Ii") {
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.INT32_LIT, rng)
			return scanText
		}

		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}
		// Ensure all characters before 'H' were valid hex digits
		if !isValidHex(string(s.src.Content[s.start:midPos])) {
			return s.errorf("malformed number: invalid hex digits before 'H'", rng)
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after hex literal", rng, s.peek())
		}

		s.emit(token.INT_LIT, rng)
		return scanText
	}

	if s.accept("Xx") {
		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}

		// extract the character before 'X'
		hexValue := string(s.src.Content[s.start : s.pos-1])

		// Ensure all characters before 'X' were valid hex digits
		if !isValidHex(hexValue) {
			return s.errorf("malformed number: invalid hex digits before 'X'", rng)
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after hex literal", rng, s.peek())
		}

		var value int
		if len(hexValue) == 2 {
			// 8-bit value (ISO/IEC 8859-1 Latin-1)
			parsed, err := strconv.ParseInt(hexValue, 16, 8)
			if err != nil {
				return s.errorf("malformed number: invalid 8-bit character value: %s", rng, hexValue)
			}
			value = int(parsed)
		} else if len(hexValue) == 4 {
			// 16-bit value (Unicode BMP)
			parsed, err := strconv.ParseInt(hexValue, 16, 16)
			if err != nil {
				return s.errorf("malformed number: invalid 16-bit character value: %s", rng, hexValue)
			}
			value = int(parsed)
		} else {
			return s.errorf("malformed number: invalid character literal, hex value must be 2 or 4 digits", rng)
		}

		// Convert the value to a rune
		character := rune(value)

		// Emit the character token with its Unicode code point
		s.emitWithValue(token.CHAR_LIT, string(character), rng)

		return scanText
	}
	s.pos = midPos // backtrack if not a hex

	// Check for decimal point (required for real)
	if s.accept(".") {
		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}

		isReal = true
		if !s.acceptDigits("0123456789") {
			return s.errorf("malformed number: invalid real number: no digits after decimal point", rng)
		}
	}

	// Check for exponent
	if s.accept("EeDdSs") {
		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}

		isReal = true
		scaleChar = rune(s.src.Content[s.pos-1])
		s.accept("+-")
		if !s.acceptDigits("0123456789") {
			return s.errorf("malformed number: invalid exponent: expected digit after exponent", rng)
		}

		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number: invalid character '%c' after hex literal", rng, s.peek())
		}
	}

	// Optional suffix for integer
	if !isReal {
		if s.accept("Ll") {
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.INT64_LIT, rng)
			return scanText
		}

		if s.accept("Ii") {
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.INT32_LIT, rng)
			return scanText
		}

		rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}
		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number literal. '%c' after hex literal", rng, s.peek())
		}

		s.emit(token.INT_LIT, rng)
		return scanText
	}

	rng, err = s.mgr.Range(s.src.Name, s.start, s.pos)
	if err != nil {
		panic(err.Error())
	}
	// Real number type decision
	switch scaleChar {
	case 'D', 'd':
		s.emit(token.LONGREAL_LIT, rng)
	case 'S', 's':
		s.emit(token.REAL_LIT, rng)
	case 'E', 'e':
		fullNum := string(s.src.Content[s.start:s.pos])
		if canFitInFloat32FromString(fullNum, string(scaleChar)) {
			s.emit(token.REAL_LIT, rng)
		} else {
			s.emit(token.LONGREAL_LIT, rng)
		}
	default:
		if !isNumberTokenBoundary(s.peek()) {
			return s.errorf("malformed number literal. '%c' after hex literal", rng, s.peek())
		}

		s.emit(token.REAL_LIT, rng)
	}

	return scanText
}

func scanHexString(s *Scanner) StateFn {
	rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
	if err != nil {
		panic(err.Error())
	}

	// First, check for the opening dollar sign
	if s.next() != '$' {
		return s.errorf("expected '$' to start hex string", rng)
	}

	var hexDigits []rune // to store the hex digits

	// Loop through the characters in the string
	for {
		r := s.next()

		rng, err = s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}
		// If we reach EOF, the string is unterminated
		if r == eof {
			return s.errorf("unterminated hex string", rng)
		}

		if unicode.IsSpace(r) {
			// Ignore whitespace characters
			continue
		}

		// If we encounter a closing dollar sign, finish the scan
		if r == '$' {
			// Check if we have an even number of hex digits
			if len(hexDigits)%2 != 0 {
				return s.errorf("hex string must have an even number of hex digits", rng)
			}

			// Emit the hex string as a token
			s.emitWithValue(token.HEX_STR_LIT, string(hexDigits), rng)
			return scanText
		}

		// Check if the character is a valid hex digit
		if !s.isHexDigit(r) {
			return s.errorf("invalid hex digit: %q", rng, r)
		}

		// Add the valid hex digit to the hexDigits slice
		hexDigits = append(hexDigits, r)
	}
}

func scanString(s *Scanner) StateFn {
	rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
	if err != nil {
		panic(err.Error())
	}

	quote := s.next() // get the opening quote (either ' or ")
	if quote != '"' && quote != '\'' {
		return s.errorf("invalid string start: expected ' or \"", rng)
	}

	var value []rune // to store the string content

	for {
		r := s.next()

		rng, err = s.mgr.Range(s.src.Name, s.start, s.pos)
		if err != nil {
			panic(err.Error())
		}

		switch r {
		case eof:
			return s.errorf("unterminated string", rng)
		case '\n':
			return s.errorf("string must not span multiple lines", rng)
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
					return s.errorf("quote cannot be the same as the string delimiter", rng)
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
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emitWithValue(token.STR_LIT, string(value), rng)
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
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.RANGE, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.PERIOD, rng)
			}
		case '-':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.MINUS, rng)
		case '+':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.PLUS, rng)
		case '=':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.EQUAL, rng)
		case '<':
			c := s.peek()
			if c == '=' {
				s.next()

				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}

				s.emit(token.LEQ, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.LESS, rng)
			}
		case '>':
			c := s.peek()
			if c == '=' {
				s.next()
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.GEQ, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.GREAT, rng)
			}
		case '&':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.AND, rng)
		case '|':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.BAR, rng)
		case '#':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.NEQ, rng)
		case '^':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}

			s.emit(token.CARET, rng)
		case ':':
			c := s.peek()
			if c == '=' {
				s.next()
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}

				s.emit(token.BECOMES, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.COLON, rng)
			}
		case '/':
			c := s.peek()
			if c == '/' {
				s.next()
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.SL_COMMENT_START, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.QUOT, rng)
			}
		case '*':
			c := s.peek()
			if c == ')' {
				s.next()
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.ML_COMMENT_END, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.STAR, rng)
			}
		case '{':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.LBRACE, rng)
		case '}':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.RBRACE, rng)
		case '[':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.LBRACK, rng)
		case ']':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.RBRACK, rng)
		case '(':
			c := s.peek()
			if c == '*' {
				s.next()
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.ML_COMMENT_START, rng)
			} else {
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}
				s.emit(token.LPAREN, rng)
			}
		case ')':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.RPAREN, rng)
		case ',':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.COMMA, rng)
		case '~':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.NOT, rng)
		case ';':
			rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
			if err != nil {
				panic(err.Error())
			}
			s.emit(token.SEMICOLON, rng)
		case ' ', '\t', '\n':
			s.ignore()
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
				rng, err := s.mgr.Range(s.src.Name, s.start, s.pos)
				if err != nil {
					panic(err.Error())
				}

				s.ignore()
				return s.errorf("invalid character %c", rng, ch)
			}
		}

		ch = s.next()
	}

	s.emit(token.EOF, &report.Range{})

	return scanText
}
