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
	LOOP
	EXIT
	REPEAT
	FOR
	WITH
	WHILE
	ELSIF
	CASE

	keyword_end

	operator_beg

	PLUS
	MINUS
	STAR
	EQUAL
	IN
	IS
	LESS
	LEQ
	GREAT
	GEQ
	DIV
	AND
	QUOT
	TILDE

	LPAREN
	RPAREN
	LBRACK
	RBRACK
	LBRACE
	RBRACE
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
	LBRACK:    "[",
	RBRACK:    "]",
	LBRACE:    "{",
	RBRACE:    "}",
	COMMA:     ",",
	COLON:     ":",
	SEMICOLON: ";",
	ASSIGN:    ":=",
	PERIOD:    ".",
	CARET:     "^",

	MODULE:     "module",
	PROC:       "proc",
	BEGIN:      "begin",
	END:        "end",
	RETURN:     "return",
	ELSE:       "else",
	IF:         "if",
	OR:         "or",
	VAR:        "var",
	THEN:       "then",
	DEFINITION: "definition",
	CONST:      "const",
	PROCEDURE:  "procedure",
	TYPE:       "type",
	ARRAY:      "array",
	POINTER:    "pointer",
	RECORD:     "record",
	LOOP:       "loop",
	EXIT:       "exit",
	REPEAT:     "repeat",
	FOR:        "for",
	WITH:       "with",
	WHILE:      "while",
	ELSIF:      "elsif",
	CASE:       "case",
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
