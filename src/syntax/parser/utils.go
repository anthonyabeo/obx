package parser

import "github.com/anthonyabeo/obx/src/syntax/token"

var stmtStart = map[token.Token]bool{
	token.EXIT:   true,
	token.WHILE:  true,
	token.FOR:    true,
	token.LOOP:   true,
	token.IF:     true,
	token.RETURN: true,
	token.CASE:   true,
	token.WITH:   true,
	token.REPEAT: true,
	token.IDENT:  true,
}

var declStart = map[token.Token]bool{
	token.IMPORT:    true,
	token.CONST:     true,
	token.TYPE:      true,
	token.VAR:       true,
	token.PROC:      true,
	token.PROCEDURE: true,
}

var exprEnd = map[token.Token]bool{
	token.COMMA:     true,
	token.COLON:     true,
	token.SEMICOLON: true,
	token.RPAREN:    true,
	token.RBRACK:    true,
	token.RBRACE:    true,
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
		p.tok == token.IDENT ||
		p.tok == token.TRUE ||
		p.tok == token.FALSE ||
		p.tok == token.NIL ||
		p.tok == token.LBRACE ||
		p.tok == token.STRING ||
		p.tok == token.HEXSTRING ||
		p.tok == token.CHAR ||
		p.tok == token.INT ||
		p.tok == token.REAL ||
		p.tok == token.NOT ||
		p.tok == token.BYTE ||
		p.tok == token.INT8 ||
		p.tok == token.INT16 ||
		p.tok == token.INT32 ||
		p.tok == token.INT64 ||
		p.tok == token.LONGREAL
}

func (p *Parser) stmtStart() bool {
	return p.tok == token.EXIT ||
		p.tok == token.IF ||
		p.tok == token.WITH ||
		p.tok == token.RETURN ||
		p.tok == token.REPEAT ||
		p.tok == token.LOOP ||
		p.tok == token.WHILE ||
		p.tok == token.FOR ||
		p.tok == token.CASE ||
		p.tok == token.IDENT
}

func (p *Parser) startsImportOrDecl() bool {
	return p.tok == token.IMPORT || p.startsDecl()
}

func (p *Parser) startsDecl() bool {
	return p.tok == token.VAR ||
		p.tok == token.TYPE ||
		p.tok == token.CONST ||
		p.tok == token.PROC ||
		p.tok == token.PROCEDURE
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
