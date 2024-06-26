package parser

import (
	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Parser struct {
	err diagnostics.ErrReporter
	lex *lexer.Lexer

	// Next token
	tok token.Token
	lit string
	pos *token.Position

	syncPos *token.Position // last synchronization position
	syncCnt int             // number of parser.advance calls without progress
}

func NewParser(lex *lexer.Lexer, rpt diagnostics.ErrReporter) *Parser {
	p := &Parser{lex: lex, err: rpt}
	p.next()

	return p
}

func (p *Parser) errorExpected(pos *token.Position, msg string) {
	msg = "expected " + msg
	if pos == p.pos {
		switch {
		case p.tok.IsLiteral():
			msg += ", found " + p.lit
		default:
			msg += ", found '" + p.tok.String() + "'"
		}
	}

	p.err.AddError(pos, msg)
}

func (p *Parser) match(tok token.Token) {
	if p.tok != tok {
		p.errorExpected(p.pos, "'"+tok.String()+"'")
	}

	p.next()
}

func (p *Parser) next() {
	p.tok, p.lit, p.pos = p.lex.Lex()
}

func (p *Parser) Parse() (unit ast.Unit) {
	switch p.tok {
	case token.MODULE:
		unit = p.parseModule()
	case token.DEFINITION:
		unit = p.parseDefinition()
	default:
		p.errorExpected(p.pos, "MODULE or DEFINITION")
	}

	return
}

// MetaParams = '(' MetaSection { [';'] MetaSection } ')'
func (p *Parser) metaParams() (seq []*ast.MetaSection) {
	p.match(token.LPAREN)

	seq = append(seq, p.parseMetaSection())
	for p.tok == token.SEMICOLON || p.tok == token.CONST || p.tok == token.TYPE {
		if p.tok == token.SEMICOLON {
			p.next()
		}

		seq = append(seq, p.parseMetaSection())
	}

	p.match(token.RPAREN)
	return
}

// MetaSection      = [ TYPE | CONST ] ident { [','] ident } [ ':' TypeConstraint ]
func (p *Parser) parseMetaSection() *ast.MetaSection {
	ms := &ast.MetaSection{}

	if p.tok == token.TYPE || p.tok == token.CONST {
		ms.Mode = p.tok
		p.next()
	}

	ms.Ids = append(ms.Ids, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENT {
		if p.tok == token.COMMA {
			p.next()
		}

		ms.Ids = append(ms.Ids, p.parseIdent())
	}

	if p.tok == token.COLON {
		p.next()
		ms.TyConst = p.parseNamedType()
	}

	return ms
}

func (p *Parser) parseModule() *ast.Module {
	mod := ast.NewModule(p.pos)

	p.match(token.MODULE)
	mod.BName = p.parseIdent()

	if p.tok == token.LPAREN {
		mod.MetaParams = p.metaParams()
	}

	if p.tok == token.SEMICOLON {
		p.next()
	}

	for p.startsImportOrDecl() {
		switch p.tok {
		case token.IMPORT:
			mod.ImportList = p.parseImportList()
		case token.VAR, token.TYPE, token.CONST, token.PROC, token.PROCEDURE:
			mod.DeclSeq = p.parseDeclarationSeq()
		default:
			pos := p.pos
			p.errorExpected(p.pos, "import or declaration")
			p.advance(declStart)
			mod.DeclSeq = append(mod.DeclSeq, &ast.BadDecl{From: pos, To: p.pos})
		}
	}

	if p.tok == token.BEGIN {
		p.next()
		mod.StmtSeq = p.parseStatementSeq()
	}

	p.match(token.END)
	mod.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		p.next()
	}

	return mod
}

// definition   = DEFINITION ident [';']  [ ImportList ] DeclarationSequence2 END ident ['.']
func (p *Parser) parseDefinition() *ast.Definition {
	def := ast.NewDefinition(p.pos)

	p.match(token.DEFINITION)
	def.BName = p.parseIdent()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	if p.tok == token.IMPORT {
		def.ImportList = p.parseImportList()
	}

	for p.startsDecl() {
		def.DeclSeq = p.parseDeclarationSeq2()
	}

	p.match(token.END)

	def.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		p.next()
	}

	return def
}

// ImportList = IMPORT import { [','] import } [';']
func (p *Parser) parseImportList() []*ast.Import {
	var list []*ast.Import

	p.match(token.IMPORT)
	list = append(list, p.parseImport())
	for p.tok == token.COMMA || p.tok == token.IDENT {
		if p.tok == token.COMMA {
			p.next()
		}

		list = append(list, p.parseImport())
	}

	if p.tok == token.SEMICOLON {
		p.next()
	}

	return list
}

// import = [ ident ':=' ] ImportPath ident [ MetaActuals ]
// ImportPath = { ident '.' }
func (p *Parser) parseImport() *ast.Import {
	imp := &ast.Import{NamePos: p.pos}
	var avail bool

	id := p.parseIdent()
	if p.tok == token.BECOMES {
		imp.Alias = id
		p.match(token.BECOMES)

		avail = true
	}

	if avail {
		id = p.parseIdent()
	}

ImportPath:
	if p.tok == token.PERIOD {
		imp.ImportPath = append(imp.ImportPath, id)
		p.match(token.PERIOD)

		id = p.parseIdent()
		goto ImportPath
	}

	imp.Name = id

	if p.tok == token.LPAREN {
		p.next()
		imp.Meta = append(imp.Meta, p.parseExpression())

		for p.tok == token.COMMA || p.exprStart() {
			if p.tok == token.COMMA {
				p.next()
			}

			imp.Meta = append(imp.Meta, p.parseExpression())
		}

		p.match(token.RPAREN)
	}

	return imp
}

//	DeclarationSequence2 = {
//			  CONST { ConstDeclaration [';'] }
//			| TYPE { TypeDeclaration [';'] }
//			| VAR { VariableDeclaration [';'] }
//			| ProcedureHeading [';']
//		}
func (p *Parser) parseDeclarationSeq2() (seq []ast.Declaration) {
	for p.startsDecl() {
		switch p.tok {
		case token.VAR:
			p.match(token.VAR)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseVarDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			p.match(token.TYPE)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseTypeDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			p.match(token.CONST)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseConstDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.PROC, token.PROCEDURE:
			seq = append(seq, p.parseProcHeading())
			if p.tok == token.SEMICOLON {
				p.next()
			}
		default:
			pos := p.pos
			p.errorExpected(pos, "declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{From: pos, To: p.pos})
		}
	}

	return seq
}

//	DeclarationSequence = {
//			  CONST { ConstDeclaration [';'] }
//			| TYPE { TypeDeclaration [';'] }
//			| VAR { VariableDeclaration [';'] }
//			| ProcedureDeclaration [';']
//		}
func (p *Parser) parseDeclarationSeq() (seq []ast.Declaration) {
	for p.startsDecl() {
		switch p.tok {
		case token.VAR:
			p.match(token.VAR)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseVarDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			p.match(token.TYPE)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseTypeDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			p.match(token.CONST)
			for p.tok == token.IDENT {
				seq = append(seq, p.parseConstDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.PROC, token.PROCEDURE:
			seq = append(seq, p.parseProcDecl())
			if p.tok == token.SEMICOLON {
				p.next()
			}
		default:
			pos := p.pos
			p.errorExpected(pos, "declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{From: pos, To: p.pos})
		}
	}

	return seq
}

// TypeDeclaration = identdef '=' type
func (p *Parser) parseTypeDecl() *ast.TypeDecl {
	t := &ast.TypeDecl{Type: p.pos}

	t.Name = p.parseIdentDef()
	p.match(token.EQUAL)
	t.DenotedType = p.parseType()

	return t
}

// ConstDeclaration = identdef '=' ConstExpression
func (p *Parser) parseConstDecl() *ast.ConstDecl {
	c := &ast.ConstDecl{Const: p.pos}

	c.Name = p.parseIdentDef()
	p.match(token.EQUAL)
	c.Value = p.parseExpression()

	return c
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVarDecl() (v *ast.VarDecl) {
	v = &ast.VarDecl{Var: p.pos}

	v.IdentList = p.parseIdentList()
	p.match(token.COLON)
	v.Type = p.parseType()

	return
}

func (p *Parser) parseType() (ty ast.Type) {
	switch p.tok {
	case token.IDENT:
		ty = p.parseNamedType()
	case token.ARRAY, token.LBRACK:
		ty = p.parseArrayType()
	case token.RECORD:
		ty = p.parseRecordType()
	case token.PROCEDURE, token.PROC:
		ty = p.parseProcType()
	case token.POINTER, token.CARET:
		ty = p.parsePtrType()
	case token.LPAREN:
		ty = p.parseEnumType()
	default:
		pos := p.pos
		p.errorExpected(pos, "type")
		p.advance(typeStart)
		return &ast.BadType{From: pos, To: p.pos}
	}

	return
}

func (p *Parser) parseEnumType() *ast.EnumType {
	enum := &ast.EnumType{Enum: p.pos}
	p.next()

	enum.Variants = append(enum.Variants, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENT {
		if p.tok == token.COMMA {
			p.next()
		}
		enum.Variants = append(enum.Variants, p.parseIdent())
	}

	p.match(token.RPAREN)

	return enum
}

func (p *Parser) parsePtrType() *ast.PointerType {
	ptr := &ast.PointerType{Ptr: p.pos}
	tok := p.tok

	p.next()
	if tok == token.POINTER {
		p.match(token.TO)
	}

	ptr.Base = p.parseType()

	return ptr
}

func (p *Parser) parseProcType() *ast.ProcType {
	proc := &ast.ProcType{Proc: p.pos}
	p.next()

	if p.tok == token.LPAREN {
		p.next()
		if p.tok == token.POINTER || p.tok == token.CARET {
			p.next()
		}

		p.match(token.RPAREN)
	}

	if p.tok == token.LPAREN {
		proc.FP = p.parseFormalParameters()
	}

	return proc
}

func (p *Parser) parseArrayType() *ast.ArrayType {
	pos := p.pos
	p.next()

	var ll *ast.LenList
	if p.tok == token.VAR || p.exprStart() {
		ll = new(ast.LenList)
		if p.tok == token.VAR {
			ll.Modifier = p.tok
			p.next()
		}

		ll.List = p.parseExprList()
	}

	if p.tok == token.RBRACK {
		p.next()
		typ := p.parseType()

		return ast.NewArray(pos, ll, typ)
	}

	p.match(token.OF)

	return ast.NewArray(pos, ll, p.parseType())
}

func (p *Parser) parseRecordType() *ast.RecordType {
	rec := &ast.RecordType{Record: p.pos}

	p.match(token.RECORD)

	if p.tok == token.LPAREN {
		p.next()
		rec.BaseType = p.parseNamedType()
		p.match(token.RPAREN)
	}

	if p.tok == token.IDENT {
		rec.Fields = append(rec.Fields, p.fieldList())
		for p.tok == token.SEMICOLON || p.tok == token.IDENT {
			if p.tok == token.SEMICOLON {
				p.next()
			}
			rec.Fields = append(rec.Fields, p.fieldList())
		}
	}

	p.match(token.END)

	return rec
}

func (p *Parser) fieldList() *ast.FieldList {
	fl := &ast.FieldList{}

	fl.IdList = p.parseIdentList()
	p.match(token.COLON)
	fl.Type = p.parseType()

	return fl
}

// advance consumes tokens until the current token p.tok
// is in the 'to' set, or token.EOF. For diagnostics recovery.
func (p *Parser) advance(to map[token.Token]bool) {
	for ; p.tok != token.EOF; p.next() {
		if to[p.tok] {
			if p.pos.Cmp(p.syncPos) == 0 && p.syncCnt < 10 {
				p.syncCnt++
				return
			}

			if p.pos.Cmp(p.syncPos) == 1 {
				p.syncPos = p.pos
				p.syncCnt = 0
				return
			}
		}
	}
}

// expression = SimpleExpression [ relation SimpleExpression ]
func (p *Parser) parseExpression() (expr ast.Expression) {
	expr = p.parseSimpleExpression()

	if p.relation() {
		relExpr := &ast.BinaryExpr{OpPos: p.pos, Left: expr, Op: p.tok}
		p.next()

		relExpr.Right = p.parseSimpleExpression()
		expr = relExpr
	}

	return expr
}

// SimpleExpression = ['+' | '-'] term { AddOperator term }
func (p *Parser) parseSimpleExpression() (expr ast.Expression) {
	var (
		sign = token.ILLEGAL
		pos  = &token.Position{}
	)

	if p.tok == token.MINUS || p.tok == token.PLUS {
		sign = p.tok
		pos = p.pos
		p.next()
	}

	expr = p.parseTerm()
	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{OpPos: pos, Op: sign, X: expr}
	}

	for p.addOp() {
		binExpr := &ast.BinaryExpr{OpPos: p.pos, Left: expr, Op: p.tok}
		p.next()

		binExpr.Right = p.parseTerm()
		expr = binExpr
	}

	return
}

// term = factor {MulOperator factor}
func (p *Parser) parseTerm() (expr ast.Expression) {
	expr = p.parseFactor()

	for p.mulOp() {
		binExpr := &ast.BinaryExpr{OpPos: p.pos, Left: expr, Op: p.tok}
		p.next()

		binExpr.Right = p.parseFactor()
		expr = binExpr
	}

	return
}

// factor = literal | designator [ActualParameters] | '(' expression ')' | '~' factor
func (p *Parser) parseFactor() (expr ast.Expression) {
	switch p.tok {
	case token.NOT:
		pos := p.pos
		p.next()
		expr = &ast.UnaryExpr{OpPos: pos, Op: token.NOT, X: p.parseFactor()}
	case token.LPAREN:
		p.match(token.LPAREN)
		expr = p.parseExpression()
		p.match(token.RPAREN)
	case token.IDENT:
		expr = p.parseDesignator()
		if p.tok == token.LPAREN {
			expr = &ast.FuncCall{Callee: expr, ActualParams: p.parseActualParameters()}
		}
	case token.INT, token.BYTE, token.INT8, token.INT16, token.INT32, token.INT64,
		token.REAL, token.LONGREAL, token.STRING, token.HEXSTRING, token.CHAR,
		token.NIL, token.TRUE, token.FALSE, token.LBRACE:

		expr = p.parseLiteral()
	default:
		pos := p.pos
		p.errorExpected(pos, "literal, parenthesized expression, operator or name")
		p.advance(exprEnd)
		expr = &ast.BadExpr{From: pos, To: p.pos}
	}

	return
}

// ActualParameters = '(' [ExpList] ')'
func (p *Parser) parseActualParameters() (list []ast.Expression) {
	p.match(token.LPAREN)

	if p.exprStart() {
		list = p.parseExprList()
	}

	p.match(token.RPAREN)

	return
}

func (p *Parser) parseExprList() (list []ast.Expression) {
	list = append(list, p.parseExpression())

	for p.tok == token.COMMA {
		p.next()
		list = append(list, p.parseExpression())
	}

	return
}

// designator = qualident {selector}
// selector = '.' ident | '[' ExpList ']' | '^' | '{' qualident '}'
func (p *Parser) parseDesignator() (dsg *ast.Designator) {
	dsg = &ast.Designator{QPos: p.pos, QualifiedIdent: p.parseQualifiedIdent()}

	expr := dsg.QualifiedIdent

	for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LBRACE {
		dsg = &ast.Designator{QPos: p.pos, QualifiedIdent: expr}

		switch p.tok {
		case token.PERIOD:
			p.next()
			dsg.Selector = &ast.DotOp{Field: p.parseIdent()}
		case token.LBRACK:
			p.next()
			List := p.parseExprList()
			p.next()
			dsg.Selector = &ast.IndexOp{List: List}
		case token.CARET:
			dsg.Selector = &ast.PtrDref{}
		case token.LBRACE:
			p.match(token.LBRACE)
			dsg.Selector = &ast.TypeGuard{Ty: p.parseQualifiedIdent()}
			p.match(token.RBRACE)
		}

		expr = dsg
	}

	return
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT, token.BYTE, token.INT8, token.INT16, token.INT32,
		token.INT64, token.REAL, token.LONGREAL, token.STRING, token.HEXSTRING,
		token.CHAR, token.TRUE, token.FALSE:

		lit = &ast.BasicLit{ValuePos: p.pos, Kind: p.tok, Val: p.lit}
		p.next()
	case token.NIL:
	case token.LBRACE:
		p.next()
		set := &ast.Set{}

		if p.exprStart() {
			set.Elem = append(set.Elem, p.parseSetElem())
			for p.tok == token.COMMA {
				p.next()
				set.Elem = append(set.Elem, p.parseSetElem())
			}
		}

		p.match(token.RBRACE)

		lit = set
	}

	return
}

// set = '{' [ element {',' element} ] '}'
// element = expression ['..' expression]
func (p *Parser) parseSetElem() ast.Expression {
	expr := p.parseExpression()
	if p.tok == token.RANGE {
		p.next()
		return &ast.ExprRange{Beg: expr, Ed: p.parseExpression()}
	}

	return expr
}

func (p *Parser) parseNamedType() ast.Type {
	typ := p.parseQualifiedIdent()
	if basicTypes[typ.String()] {
		return ast.NewBasicType(typ.Pos(), typ.String())
	}

	return ast.NewNamedType(typ.Pos(), typ)

}

// qualident = [ ident '.' ] ident
func (p *Parser) parseQualifiedIdent() ast.Expression {
	id := p.parseIdent()

	if p.tok == token.DCOLON {
		p.next()
		sel := p.parseIdent()

		return &ast.QualifiedIdent{Module: id, Sel: sel}
	}

	return id
}

// IdentList = identdef { [','] identdef}
// identdef  = ident ['*' | '-']
func (p *Parser) parseIdentList() (list []*ast.Ident) {
	list = append(list, p.parseIdentDef())

	for p.tok == token.COMMA {
		p.next()
		list = append(list, p.parseIdentDef())
	}

	return
}

// ProcedureDeclaration = ProcedureHeading [ ';' ] ProcedureBody END [ ident ]
func (p *Parser) parseProcDecl() (proc *ast.ProcDecl) {
	proc = &ast.ProcDecl{Proc: p.pos}

	proc.Head = p.parseProcHeading()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	proc.Body = p.parseProcBody()

	p.match(token.END)
	if p.tok == token.IDENT {
		proc.EndName = p.parseIdent()
	}

	return
}

// ProcedureHeading = ( PROCEDURE | PROC ) [Receiver] identdef [ FormalParameters ]
// Receiver = '(' [VAR|IN] ident ':' ident ')'
func (p *Parser) parseProcHeading() (head *ast.ProcHead) {
	head = &ast.ProcHead{}

	if p.tok != token.PROC && p.tok != token.PROCEDURE {
		p.errorExpected(p.pos, "proc or procedure")
		p.advance(declStart)
		return nil
	} else {
		p.next()
		if p.tok == token.LPAREN {
			head.Rcv = p.parseReceiver()
		}

		head.Name = p.parseIdentDef()
		if p.tok == token.LPAREN {
			head.FP = p.parseFormalParameters()
		}
	}

	return
}

func (p *Parser) parseIdentDef() *ast.Ident {
	id := p.parseIdent()
	if p.tok == token.STAR || p.tok == token.MINUS {
		id.IProps |= ast.Exported
		if p.tok == token.MINUS {
			id.IProps |= ast.ReadOnly
		}

		p.next()
	}

	return id
}

// FormalParameters = '(' [ FPSection { [';'] FPSection } ] ')' [ ':' ReturnType ]
func (p *Parser) parseFormalParameters() (fp *ast.FormalParams) {
	fp = &ast.FormalParams{}

	p.match(token.LPAREN)

	if p.tok == token.VAR || p.tok == token.IN || p.tok == token.IDENT {
		fp.Params = append(fp.Params, p.parseFPSection())
		for p.tok == token.SEMICOLON {
			p.next()

			fp.Params = append(fp.Params, p.parseFPSection())
		}
	}

	p.match(token.RPAREN)

	if p.tok == token.COLON {
		p.next()
		fp.RetType = p.parseType()
	}

	return
}

// FPSection = [ VAR | IN ] ident { [','] ident } ':' FormalType
func (p *Parser) parseFPSection() (param *ast.FPSection) {
	param = &ast.FPSection{}

	if p.tok == token.VAR || p.tok == token.IN {
		param.Mod = p.tok
		p.next()
	}

	param.Names = append(param.Names, p.parseIdent())
	for p.tok == token.COMMA {
		p.next()
		param.Names = append(param.Names, p.parseIdent())
	}

	p.match(token.COLON)

	param.Type = p.parseType()

	return
}

func (p *Parser) parseReceiver() (rcv *ast.Receiver) {
	rcv = &ast.Receiver{}

	p.match(token.LPAREN)
	if p.tok == token.VAR || p.tok == token.IN {
		rcv.Mod = p.tok
		p.next()
	}

	rcv.Var = p.parseIdent()
	p.match(token.COLON)
	rcv.Type = p.parseIdent()

	p.match(token.RPAREN)

	return
}

// ProcedureBody = DeclarationSequence [ BEGIN StatementSequence | ReturnStatement [ ';' ] ]
func (p *Parser) parseProcBody() (body *ast.ProcBody) {
	body = &ast.ProcBody{}

	body.DeclSeq = p.parseDeclarationSeq()

	if p.tok == token.BEGIN {
		p.next()
		body.StmtSeq = p.parseStatementSeq()

		if p.tok == token.SEMICOLON {
			p.next()
		}
	}

	return
}

// StatementSequence = statement { [";"] statement}
func (p *Parser) parseStatementSeq() (seq []ast.Statement) {
	seq = append(seq, p.parseStatement())

	for p.tok == token.SEMICOLON || p.stmtStart() {
		if p.tok == token.SEMICOLON {
			p.next()
		}

		seq = append(seq, p.parseStatement())
	}

	return
}

func (p *Parser) parseIdent() *ast.Ident {
	pos := p.pos
	name := "_"
	if p.tok == token.IDENT {
		name = p.lit

		p.next()
	} else {
		p.match(token.IDENT)
	}

	return &ast.Ident{NamePos: pos, Name: name}
}

func (p *Parser) parseStatement() (stmt ast.Statement) {
	switch p.tok {
	case token.LOOP:
		stmt = p.parseLoopStmt()
	case token.EXIT:
		stmt = p.parseExitStmt()
	case token.IF:
		stmt = p.parseIfStmt()
	case token.FOR:
		stmt = p.parseForStmt()
	case token.WITH:
		stmt = p.parseWithStmt()
	case token.CASE:
		stmt = p.parseCaseStmt()
	case token.RETURN:
		stmt = p.parseReturnStmt()
	case token.WHILE:
		stmt = p.parseWhileStmt()
	case token.REPEAT:
		stmt = p.parseRepeatStmt()
	case token.IDENT:
		dsg := p.parseDesignator()
		switch p.tok {
		case token.BECOMES:
			pos := p.pos
			p.next()
			stmt = &ast.AssignStmt{AssignPos: pos, LValue: dsg, RValue: p.parseExpression()}
		case token.LPAREN:
			stmt = &ast.ProcCall{NamePos: dsg.QPos, Callee: dsg, ActualParams: p.parseActualParameters()}
		}
	default:
		pos := p.pos
		p.errorExpected(pos, "statement")
		p.advance(stmtStart)
		stmt = &ast.BadStmt{From: pos, To: p.pos}
	}

	return
}

func (p *Parser) parseLoopStmt() (stmt *ast.LoopStmt) {
	stmt = &ast.LoopStmt{Loop: p.pos}

	p.match(token.LOOP)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.END)

	return
}

func (p *Parser) parseRepeatStmt() (stmt *ast.RepeatStmt) {
	stmt = &ast.RepeatStmt{Repeat: p.pos}

	p.match(token.REPEAT)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.UNTIL)
	stmt.BoolExpr = p.parseExpression()

	return
}

func (p *Parser) parseWhileStmt() (stmt *ast.WhileStmt) {
	stmt = &ast.WhileStmt{While: p.pos}

	p.match(token.WHILE)
	stmt.BoolExpr = p.parseExpression()
	p.match(token.DO)
	stmt.StmtSeq = p.parseStatementSeq()

	for p.tok == token.ELSIF {
		p.next()

		elsif := &ast.ElsIfBranch{BoolExpr: p.parseExpression()}
		p.match(token.DO)
		elsif.ThenPath = p.parseStatementSeq()

		stmt.ElsIfs = append(stmt.ElsIfs, elsif)
	}

	p.match(token.END)

	return
}

func (p *Parser) parseReturnStmt() (stmt *ast.ReturnStmt) {
	stmt = &ast.ReturnStmt{Return: p.pos}

	p.match(token.RETURN)
	if p.exprStart() {
		stmt.Value = p.parseExpression()
	}

	return
}

// IfStatement = IF expression THEN StatementSequence {ElsifStatement} [ElseStatement] END
func (p *Parser) parseIfStmt() (stmt *ast.IfStmt) {
	stmt = &ast.IfStmt{If: p.pos}

	p.match(token.IF)
	stmt.BoolExpr = p.parseExpression()
	p.match(token.THEN)
	stmt.ThenPath = p.parseStatementSeq()

	for p.tok == token.ELSIF {
		p.next()

		elsif := &ast.ElsIfBranch{BoolExpr: p.parseExpression()}
		p.match(token.THEN)
		elsif.ThenPath = p.parseStatementSeq()

		stmt.ElseIfBranches = append(stmt.ElseIfBranches, elsif)
	}

	if p.tok == token.ELSE {
		p.next()
		stmt.ElsePath = p.parseStatementSeq()
	}

	p.match(token.END)

	return
}

// CaseStatement = CASE expression OF ['|'] Case { '|' Case } [ ELSE StatementSequence ] END
func (p *Parser) parseCaseStmt() (stmt *ast.CaseStmt) {
	stmt = &ast.CaseStmt{Case: p.pos}

	p.match(token.CASE)

	stmt.Expr = p.parseExpression()
	p.match(token.OF)

	if p.tok == token.BAR {
		p.next()
	}

	stmt.Cases = append(stmt.Cases, p.parseCase())
	for p.tok == token.BAR {
		p.next()
		stmt.Cases = append(stmt.Cases, p.parseCase())
	}

	if p.tok == token.ELSE {
		p.next()
		stmt.Else = p.parseStatementSeq()
	}

	p.match(token.END)

	return
}

func (p *Parser) parseCase() *ast.Case {
	c := &ast.Case{}

	c.CaseLabelList = append(c.CaseLabelList, p.parseLabelRange())
	for p.tok == token.COMMA {
		p.next()
		c.CaseLabelList = append(c.CaseLabelList, p.parseLabelRange())
	}

	p.match(token.COLON)

	c.StmtSeq = p.parseStatementSeq()

	return c
}

func (p *Parser) parseLabelRange() *ast.LabelRange {
	r := &ast.LabelRange{Begin: p.parseExpression()}
	if p.tok == token.RANGE {
		p.next()
		r.End = p.parseExpression()
	}

	return r
}

// ForStatement = FOR ident ':=' expression TO expression [BY ConstExpression] DO StatementSequence END
func (p *Parser) parseForStmt() (stmt *ast.ForStmt) {
	stmt = &ast.ForStmt{For: p.pos}

	p.match(token.FOR)
	stmt.CtlVar = p.parseIdent()
	p.match(token.BECOMES)
	stmt.InitVal = p.parseExpression()
	p.match(token.TO)
	stmt.FinalVal = p.parseExpression()

	if p.tok == token.BY {
		p.next()
		stmt.By = p.parseExpression()
	}

	p.match(token.DO)

	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.END)

	return
}

// ExitStatement = EXIT
func (p *Parser) parseExitStmt() *ast.ExitStmt {
	stmt := &ast.ExitStmt{Exit: p.pos}
	p.match(token.EXIT)

	return stmt
}

// WithStatement = WITH ['|'] Guard DO StatementSequence { '|' Guard DO StatementSequence} [ ELSE StatementSequence ] END
// Guard         = qualident ':' qualident
func (p *Parser) parseWithStmt() *ast.WithStmt {
	stmt := &ast.WithStmt{With: p.pos}

	p.match(token.WITH)

	if p.tok == token.BAR {
		p.next()
	}

	stmt.Arms = append(stmt.Arms, p.parseGuard())
	for p.tok == token.BAR {
		p.next()
		stmt.Arms = append(stmt.Arms, p.parseGuard())
	}

	if p.tok == token.ELSE {
		stmt.Else = p.parseStatementSeq()
	}

	return stmt
}

func (p *Parser) parseGuard() *ast.Guard {
	grd := &ast.Guard{Expr: p.parseQualifiedIdent(), Type: p.parseQualifiedIdent()}
	p.match(token.DO)
	grd.StmtSeq = p.parseStatementSeq()

	return grd
}
