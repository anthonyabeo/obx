package token

import (
	"fmt"
	"strconv"
)

type Kind int

func (tok Kind) String() string {
	s := ""
	if 0 <= tok && tok < Kind(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

func (tok Kind) IsLiteral() bool  { return literal_begin < tok && tok < literal_end }
func (tok Kind) IsOperator() bool { return operator_beg < tok && tok < operator_end }
func (tok Kind) IsDelim() bool    { return delim_beg < tok && tok < delim_end }

const (
	ILLEGAL Kind = iota
	EOF

	SL_COMMENT_START
	ML_COMMENT_START
	ML_COMMENT_END
	NEWLINE

	IDENTIFIER // main

	literal_begin
	BYTE_LIT
	INT8_LIT
	INT16_LIT
	INT32_LIT
	INT64_LIT
	REAL_LIT
	LONGREAL_LIT
	CHAR_LIT
	HEX_STR_LIT
	STR_LIT
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
	UNTIL
	TRUE
	FALSE
	BY
	OF
	TO
	NIL

	OR
	IN
	DIV
	IS
	MOD

	BOOLEAN
	BYTE
	CHAR
	INT8
	INT16
	INT32
	INT64
	REAL
	LONGREAL
	SET
	WCHAR
	INTEGER
	SHORTINT
	LONGINT
	keyword_end

	operator_beg
	// Arithmetic
	PLUS
	MINUS
	STAR
	QUOT

	AND
	NOT

	EQUAL
	NEQ
	LESS
	LEQ
	GREAT
	GEQ

	CARET
	operator_end

	delim_beg
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
	BAR
	RANGE
	delim_end
)

var tokens = [...]string{
	ILLEGAL:          "ILLEGAL",
	EOF:              "EOF",
	SL_COMMENT_START: "//",
	ML_COMMENT_START: "(*",
	ML_COMMENT_END:   "*)",
	NEWLINE:          "\n",

	IDENTIFIER: "IDENTIFIER",

	BOOLEAN:  "bool",
	BYTE:     "byte",
	CHAR:     "char",
	INT8:     "int8",
	INT16:    "int16",
	INT32:    "int32",
	INT64:    "int64",
	REAL:     "real",
	LONGREAL: "longreal",
	SET:      "set",
	WCHAR:    "wchar",
	INTEGER:  "integer",
	SHORTINT: "shortint",
	LONGINT:  "longint",

	PLUS:  "+",
	MINUS: "-",
	EQUAL: "=",
	NEQ:   "#",
	STAR:  "*",
	LESS:  "<",
	LEQ:   "<=",
	GREAT: ">",
	GEQ:   ">=",
	AND:   "&",
	QUOT:  "/",
	NOT:   "~",
	RANGE: "..",

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

var keywords map[string]Kind

func init() {
	keywords = make(map[string]Kind, keyword_end-(keyword_beg+1))
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

func Lookup(ident string) Kind {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	return IDENTIFIER
}

type Token struct {
	Kind   Kind
	Lexeme string
	Pos    int
	End    int
}

func (i Token) String() string {
	switch i.Kind {
	case EOF:
		return "EOF"
	case ILLEGAL:
		return i.Lexeme
	default:
		if len(i.Lexeme) > 10 {
			return fmt.Sprintf("%.10q...", i.Lexeme)
		}
		return fmt.Sprintf("%q", i.Lexeme)
	}
}
