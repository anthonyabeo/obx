package scan

import (
	"github.com/anthonyabeo/obx/src/syntax/token"
	"math"
	"strconv"
	"strings"
	"unicode"
)

func (s *Scanner) isWhiteSpace(ch rune) bool {

	switch ch {
	case ' ', '\t', '\n', '\r':
		return true
	default:
		return false
	}
}

func (s *Scanner) isDecDigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}

func (s *Scanner) isHexDigit(ch rune) bool {
	return s.isDecDigit(ch) ||
		'a' <= ch && ch <= 'f' ||
		'A' <= ch && ch <= 'F'
}

func (s *Scanner) isLetter(ch rune) bool {
	return 'a' <= ch && ch <= 'z' ||
		'A' <= ch && ch <= 'Z'
}

func (s *Scanner) startsIdent(ch rune) bool {
	return s.isLetter(ch) || ch == '_'
}

func (s *Scanner) accept(valid string) bool {
	r := s.next()
	if strings.ContainsRune(valid, r) {
		return true
	}
	s.backup()
	return false
}

func (s *Scanner) acceptRun(valid string) bool {
	start := s.pos
	for strings.ContainsRune(valid, s.next()) {
	}
	s.backup()
	return s.pos > start
}

func (s *Scanner) acceptDigits(valid string) bool {
	start := s.pos
	if !strings.ContainsRune(valid, s.next()) {
		s.backup()
		return false
	}
	for strings.ContainsRune(valid, s.next()) {
	}
	s.backup()
	return s.pos > start
}

func canFitInFloat32FromString(fullNum, scaleChar string) bool {
	// Split on 'e' or 'E' (already validated as present)
	parts := strings.SplitN(fullNum, scaleChar, 2)
	if len(parts) != 2 {
		parts = strings.SplitN(fullNum, "E", 2)
		if len(parts) != 2 {
			return false // malformed number
		}
	}
	//_ := parts[0]
	expStr := parts[1]

	exp, err := strconv.ParseInt(expStr, 10, 64)
	if err != nil {
		return false // invalid exponent
	}

	// Reject out-of-bounds exponents
	if exp < -45 || exp > 38 {
		return false // out of float32 range
	}

	val, err := strconv.ParseFloat(fullNum, 64)
	if err != nil {
		return false
	}
	abs := math.Abs(val)
	return abs <= math.MaxFloat32 && (abs == 0 || abs >= math.SmallestNonzeroFloat32)
}

func isValidHex(s string) bool {
	for _, r := range s {
		if !strings.ContainsRune("0123456789abcdefABCDEF", r) {
			return false
		}
	}
	return true
}

func isNumberTokenBoundary(r rune) bool {
	return r == eof ||
		unicode.IsSpace(r) ||
		strings.ContainsRune(":;,(){}[]", r) || // statement and grouping
		strings.ContainsRune("+-*/%=<>#", r) // typical operators
}

const (
	maxBYTE  = 255
	maxCHAR  = 255
	minINT8  = -128
	maxINT8  = 127
	minINT16 = -32768
	maxINT16 = 32767
	minINT32 = -2147483648
	maxINT32 = 2147483647
	// INT64 covers full int64 range, which ParseInt handles
)

func MinimalIntToken(value int64) token.Kind {
	switch {
	case value >= 0 && value <= maxBYTE:
		return token.BYTE_LIT
	case value >= minINT8 && value <= maxINT8:
		return token.INT8_LIT
	case value >= minINT16 && value <= maxINT16:
		return token.INT16_LIT
	case value >= minINT32 && value <= maxINT32:
		return token.INT32_LIT
	default:
		return token.INT64_LIT
	}
}
