package parser

import "github.com/anthonyabeo/obx/src/syntax/token"

var stmtStart = map[token.Kind]bool{
	token.EXIT:       true,
	token.WHILE:      true,
	token.FOR:        true,
	token.LOOP:       true,
	token.IF:         true,
	token.RETURN:     true,
	token.CASE:       true,
	token.WITH:       true,
	token.REPEAT:     true,
	token.IDENTIFIER: true,
}

var declStart = map[token.Kind]bool{
	token.IMPORT:    true,
	token.CONST:     true,
	token.TYPE:      true,
	token.VAR:       true,
	token.PROC:      true,
	token.PROCEDURE: true,
}

var exprEnd = map[token.Kind]bool{
	token.COMMA:     true,
	token.COLON:     true,
	token.SEMICOLON: true,
	token.RPAREN:    true,
	token.RBRACK:    true,
	token.RBRACE:    true,
}

var typeStart = map[token.Kind]bool{
	token.ARRAY:      true,
	token.LBRACK:     true,
	token.PROCEDURE:  true,
	token.LPAREN:     true,
	token.POINTER:    true,
	token.CARET:      true,
	token.RECORD:     true,
	token.IDENTIFIER: true,
}

func (p *Parser) addOp() bool {
	return p.tok == token.PLUS ||
		p.tok == token.MINUS ||
		p.tok == token.OR
}

func (p *Parser) mulOp() bool {
	return p.tok == token.STAR ||
		p.tok == token.QUOT ||
		p.tok == token.DIV ||
		p.tok == token.MOD ||
		p.tok == token.AND
}

func (p *Parser) relation() bool {
	return p.tok == token.EQUAL ||
		p.tok == token.LESS ||
		p.tok == token.LEQ ||
		p.tok == token.GREAT ||
		p.tok == token.GEQ ||
		p.tok == token.IN ||
		p.tok == token.IS ||
		p.tok == token.NEQ
}

func (p *Parser) exprStart() bool {
	return p.tok == token.LPAREN ||
		p.tok == token.PLUS ||
		p.tok == token.MINUS ||
		p.tok == token.IDENTIFIER ||
		p.tok == token.TRUE ||
		p.tok == token.FALSE ||
		p.tok == token.NIL ||
		p.tok == token.LBRACE ||
		p.tok == token.STR_LIT ||
		p.tok == token.HEX_STR_LIT ||
		p.tok == token.CHAR_LIT ||
		p.tok == token.INT_LIT ||
		p.tok == token.REAL_LIT ||
		p.tok == token.NOT ||
		p.tok == token.INT32_LIT ||
		p.tok == token.INT64_LIT ||
		p.tok == token.LONGREAL_LIT
}

func (p *Parser) stmtStart() bool {
	return stmtStart[p.tok]
}

func (p *Parser) startsImportOrDecl() bool {
	return p.tok == token.IMPORT || p.startsDecl()
}

func (p *Parser) startsDecl() bool {
	return declStart[p.tok]
}

var basicTypes = map[string]bool{
	"integer":  true,
	"real":     true,
	"longreal": true,
	"boolean":  true,
	"byte":     true,
	"char":     true,
	"set":      true,
	"wchar":    true,
	"int8":     true,
	"int16":    true,
	"int32":    true,
	"int64":    true,
	"bool":     true,
}
