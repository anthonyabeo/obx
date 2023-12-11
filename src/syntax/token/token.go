package token

import "strconv"

type Token int

func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

const (
	ILLEGAL Token = iota
	EOF
	COMMENT

	literal_beg

	IDENT // main
	INT   // 12345

	literal_end

	keyword_beg

	MODULE
	PROC
	PROCEDURE
	BEGIN
	END
	RETURN
	ELSE
	IF
	OR
	VAR
	THEN
	DEFINITION
	IMPORT
	CONST
	TYPE
	ARRAY
	RECORD
	POINTER

	keyword_end

	operator_beg

	PLUS
	MINUS
	STAR
	EQUAL

	LPAREN
	RPAREN
	COMMA
	COLON
	SEMICOLON
	ASSIGN
	PERIOD
	CARET

	operator_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	COMMENT: "COMMENT",

	IDENT: "IDENT",
	INT:   "INT",

	PLUS:  "+",
	MINUS: "-",
	EQUAL: "=",
	STAR:  "*",

	LPAREN:    "(",
	RPAREN:    ")",
	COMMA:     ",",
	COLON:     ":",
	SEMICOLON: ";",
	ASSIGN:    ":=",
	PERIOD:    ".",
	CARET:     "^",

	MODULE:    "module",
	PROC:      "proc",
	BEGIN:     "begin",
	END:       "end",
	RETURN:    "return",
	ELSE:      "else",
	IF:        "if",
	OR:        "or",
	VAR:       "var",
	THEN:      "then",
	CONST:     "const",
	PROCEDURE: "procedure",
	TYPE:      "type",
	ARRAY:     "array",
	POINTER:   "pointer",
	RECORD:    "record",
}

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token, keyword_end-(keyword_beg+1))
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

func Lookup(ident string) Token {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	return IDENT
}
