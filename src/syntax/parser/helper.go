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

var exprStart = map[token.Kind]bool{
	token.LPAREN:       true,
	token.PLUS:         true,
	token.MINUS:        true,
	token.IDENTIFIER:   true,
	token.TRUE:         true,
	token.FALSE:        true,
	token.NIL:          true,
	token.LBRACE:       true,
	token.STR_LIT:      true,
	token.HEX_STR_LIT:  true,
	token.CHAR_LIT:     true,
	token.WCHAR_LIT:    true,
	token.BYTE_LIT:     true,
	token.INT8_LIT:     true,
	token.INT16_LIT:    true,
	token.INT32_LIT:    true,
	token.INT64_LIT:    true,
	token.REAL_LIT:     true,
	token.LONGREAL_LIT: true,
	token.NOT:          true,
}

var exprEnd = map[token.Kind]bool{
	token.COMMA:     true,
	token.COLON:     true,
	token.SEMICOLON: true,
	token.RPAREN:    true,
	token.RBRACK:    true,
	token.RBRACE:    true,
	token.THEN:      true,
	token.OF:        true,
	token.DO:        true,
	token.RANGE:     true,
	token.TO:        true,
	token.BY:        true,
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
	token.INT8:       true,
	token.INT16:      true,
	token.INT32:      true,
	token.INT64:      true,
	token.REAL:       true,
	token.LONGREAL:   true,
	token.BYTE:       true,
	token.BOOLEAN:    true,
	token.CHAR:       true,
	token.WCHAR:      true,
	token.INTEGER:    true,
	token.SHORTINT:   true,
	token.LONGINT:    true,
	token.SET:        true,
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
	return exprStart[p.tok] || typeStart[p.tok]
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
