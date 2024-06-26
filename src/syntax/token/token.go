package token

import (
	"strconv"
	"strings"
)

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

func (tok Token) IsLiteral() bool { return literal_beg < tok && tok < literal_end }

const (
	ILLEGAL Token = iota
	EOF
	COMMENT

	literal_beg

	IDENT // main
	INT   // 12345
	REAL
	CHAR
	STRING
	HEXSTRING

	BYTE
	INT8
	INT16
	INT32
	INT64
	LONGREAL

	NIL

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
	DO
	IN
	IS
	DIV
	UNTIL
	TRUE
	FALSE
	BY
	MOD
	OF
	TO

	keyword_end

	operator_beg

	PLUS
	MINUS
	STAR
	EQUAL
	NEQ
	LESS
	LEQ
	GREAT
	GEQ
	AND
	QUOT
	NOT
	RANGE

	LPAREN
	RPAREN
	LBRACK
	RBRACK
	LBRACE
	RBRACE
	COMMA
	COLON
	SEMICOLON
	BECOMES
	PERIOD
	CARET
	BAR

	operator_end

	DCOLON
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	COMMENT: "COMMENT",

	IDENT:     "IDENT",
	INT:       "INT",
	REAL:      "REAL",
	CHAR:      "CHAR",
	STRING:    "STRING",
	HEXSTRING: "HEXSTRING",
	BYTE:      "BYTE",
	INT8:      "INT8",
	INT16:     "INT16",
	INT32:     "INT32",
	INT64:     "INT64",
	LONGREAL:  "LONGREAL",

	PLUS:   "+",
	MINUS:  "-",
	EQUAL:  "=",
	NEQ:    "#",
	STAR:   "*",
	LESS:   "<",
	LEQ:    "<=",
	GREAT:  ">",
	GEQ:    ">=",
	AND:    "&",
	QUOT:   "/",
	NOT:    "~",
	RANGE:  "..",
	DCOLON: "::",

	LPAREN:    "(",
	RPAREN:    ")",
	LBRACK:    "[",
	RBRACK:    "]",
	LBRACE:    "{",
	RBRACE:    "}",
	COMMA:     ",",
	COLON:     ":",
	SEMICOLON: ";",
	BECOMES:   ":=",
	PERIOD:    ".",
	CARET:     "^",
	BAR:       "|",

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
	IN:         "in",
	IS:         "is",
	DIV:        "div",
	DO:         "do",
	UNTIL:      "until",
	TRUE:       "true",
	FALSE:      "false",
	BY:         "by",
	IMPORT:     "import",
	MOD:        "mod",
	NIL:        "nil",
	OF:         "of",
	TO:         "to",
}

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token, keyword_end-(keyword_beg+1))
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

func Lookup(ident string) Token {
	ident = strings.ToLower(ident)
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	return IDENT
}
