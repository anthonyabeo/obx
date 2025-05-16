package parser

import (
	"strings"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/scan"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Parser struct {
	err     diagnostics.ErrReporter
	scanner *scan.Scanner

	// Next token
	tok token.Kind
	lit string
}

func NewParser(lex *scan.Scanner, rpt diagnostics.ErrReporter) *Parser {
	p := &Parser{scanner: lex, err: rpt}
	p.next()

	return p
}

func (p *Parser) errorExpected(msg string) {
	msg = "expected " + msg

	switch {
	case p.tok.IsLiteral():
		msg += ", found " + p.lit
	default:
		msg += ", found '" + p.tok.String() + "'"
	}

	p.err.AddError(msg)
}

func (p *Parser) match(tok token.Kind) {
	if p.tok != tok {
		p.errorExpected("'" + tok.String() + "'")
	}

	p.next()
}

func (p *Parser) next() {
	tok := p.scanner.NextItem()
	p.tok = tok.Kind
	p.lit = tok.Val
}

func (p *Parser) Parse() (unit ast.CompilationUnit) {
	switch p.tok {
	case token.MODULE:
		unit = p.parseModule()
	case token.DEFINITION:
		unit = p.parseDefinition()
	default:
		p.errorExpected("MODULE or DEFINITION")
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
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
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
	mod := ast.NewModule()

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
			p.errorExpected("import or declaration")
			p.advance(declStart)
			mod.DeclSeq = append(mod.DeclSeq, &ast.BadDecl{})
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
	def := ast.NewDefinition()

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
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
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
	imp := &ast.Import{}
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

	imp.ImportPath = append(imp.ImportPath, id)
	imp.Name = strings.Join(imp.ImportPath, ".")

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
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseVariableDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseTypeDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseConstantDecl())
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
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{})
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
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseVariableDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseTypeDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				seq = append(seq, p.parseConstantDecl())
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.PROC, token.PROCEDURE:
			seq = append(seq, p.parseProcedureDecl())
			if p.tok == token.SEMICOLON {
				p.next()
			}
		default:
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{})
		}
	}

	return seq
}

// TypeDeclaration = identdef '=' type
func (p *Parser) parseTypeDecl() *ast.TypeDecl {
	return &ast.TypeDecl{
		Name: p.parseIdentifierDef(),
		DenotedType: func() ast.Type {
			p.match(token.EQUAL)
			return p.parseType()
		}(),
	}
}

// ConstDeclaration = identdef '=' ConstExpression
func (p *Parser) parseConstantDecl() *ast.ConstantDecl {
	return &ast.ConstantDecl{
		Name: p.parseIdentifierDef(),
		Value: func() ast.Expression {
			p.match(token.EQUAL)
			return p.parseExpression()
		}(),
	}
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVariableDecl() (v *ast.VariableDecl) {
	return &ast.VariableDecl{
		IdentList: p.parseIdentList(),
		Type: func() ast.Type {
			p.match(token.COLON)
			return p.parseType()
		}(),
	}
}

func (p *Parser) parseType() (ty ast.Type) {
	switch p.tok {
	case token.IDENTIFIER:
		ty = p.parseNamedType()
	case token.ARRAY, token.LBRACK:
		ty = p.parseArrayType()
	case token.RECORD:
		ty = p.parseRecordType()
	case token.PROCEDURE, token.PROC:
		ty = p.parseProcedureType()
	case token.POINTER, token.CARET:
		ty = p.parsePtrType()
	case token.LPAREN:
		ty = p.parseEnumType()
	default:
		p.errorExpected("type")
		p.advance(typeStart)
		return &ast.BadType{}
	}

	return
}

func (p *Parser) parseEnumType() *ast.EnumType {
	enum := &ast.EnumType{}
	p.next()

	enum.Variants = append(enum.Variants, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
		if p.tok == token.COMMA {
			p.next()
		}
		enum.Variants = append(enum.Variants, p.parseIdent())
	}

	p.match(token.RPAREN)

	return enum
}

func (p *Parser) parsePtrType() *ast.PointerType {
	ptr := &ast.PointerType{}
	tok := p.tok

	p.next()
	if tok == token.POINTER {
		p.match(token.TO)
	}

	ptr.Base = p.parseType()

	return ptr
}

func (p *Parser) parseProcedureType() *ast.ProcedureType {
	proc := &ast.ProcedureType{}
	p.match(token.PROCEDURE)

	if p.tok == token.LPAREN {
		proc.FP = p.parseFormalParameters()
	}

	return proc
}

func (p *Parser) parseArrayType() *ast.ArrayType {
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

		return ast.NewArray(ll, typ)
	}

	p.match(token.OF)

	return ast.NewArray(ll, p.parseType())
}

func (p *Parser) parseRecordType() *ast.RecordType {
	rec := &ast.RecordType{}

	p.match(token.RECORD)

	if p.tok == token.LPAREN {
		p.next()
		rec.Base = p.parseNamedType()
		p.match(token.RPAREN)
	}

	if p.tok == token.IDENTIFIER {
		rec.Fields = append(rec.Fields, p.fieldList())
		for p.tok == token.SEMICOLON || p.tok == token.IDENTIFIER {
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
	fl := &ast.FieldList{List: p.parseIdentList()}

	p.match(token.COLON)
	fl.Type = p.parseType()

	return fl
}

// advance consumes tokens until the current token p.tok
// is in the 'to' set, or token.EOF. For diagnostics recovery.
func (p *Parser) advance(to map[token.Kind]bool) {
	for ; p.tok != token.EOF; p.next() {
		if to[p.tok] {
			return
		}
	}
}

// expression = SimpleExpression [ relation SimpleExpression ]
func (p *Parser) parseExpression() (expr ast.Expression) {
	expr = p.parseSimpleExpression()

	if p.relation() {
		relExpr := &ast.BinaryExpr{Left: expr, Op: p.tok}
		p.next()

		relExpr.Right = p.parseSimpleExpression()
		expr = relExpr
	}

	return expr
}

// SimpleExpression = ['+' | '-'] term { AddOperator term }
func (p *Parser) parseSimpleExpression() (expr ast.Expression) {
	var sign = token.ILLEGAL

	if p.tok == token.MINUS || p.tok == token.PLUS {
		sign = p.tok
		p.next()
	}

	expr = p.parseTerm()
	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{Op: sign, Operand: expr}
	}

	for p.addOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok}
		p.next()

		bin.Right = p.parseTerm()
		expr = bin
	}

	return
}

// term = factor {MulOperator factor}
func (p *Parser) parseTerm() (expr ast.Expression) {
	expr = p.parseFactor()

	for p.mulOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok}
		p.next()

		bin.Right = p.parseFactor()
		expr = bin
	}

	return
}

// factor = literal | designator [ActualParameters] | '(' expression ')' | '~' factor
func (p *Parser) parseFactor() (expr ast.Expression) {
	switch p.tok {
	case token.NOT:
		p.next()
		expr = &ast.UnaryExpr{Op: token.NOT, Operand: p.parseFactor()}
	case token.LPAREN:
		p.match(token.LPAREN)
		expr = p.parseExpression()
		p.match(token.RPAREN)
	case token.IDENTIFIER:
		expr = p.parseDesignator()
		if p.tok == token.LPAREN {
			expr = &ast.FunctionCall{Callee: expr, ActualParams: p.parseActualParameters()}
		}
	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.NIL, token.TRUE, token.FALSE, token.LBRACE:

		expr = p.parseLiteral()
	default:
		p.errorExpected("literal, parenthesized expression, operator or name")
		p.advance(exprEnd)
		expr = &ast.BadExpr{}
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
	dsg = &ast.Designator{QualifiedIdent: p.parseQualifiedIdent()}

	expr := dsg.QualifiedIdent

	for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LBRACE {
		dsg = &ast.Designator{QualifiedIdent: expr}

		switch p.tok {
		case token.PERIOD:
			p.next()
			dsg.Select = &ast.DotOp{Field: p.parseIdent()}
		case token.LBRACK:
			p.next()
			List := p.parseExprList()
			p.match(token.RBRACK)
			dsg.Select = &ast.IndexOp{List: List}
		case token.CARET:
			dsg.Select = &ast.PtrDeref{}
		case token.LBRACE:
			p.match(token.LBRACE)
			dsg.Select = &ast.TypeGuard{Ty: p.parseQualifiedIdent()}
			p.match(token.RBRACE)
		default:
		}

		expr = dsg
	}

	return
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.TRUE, token.FALSE, token.NIL:

		lit = &ast.BasicLit{Kind: p.tok, Val: p.lit}
		p.next()
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
	default:
		p.errorExpected("literal or set")
		p.advance(exprEnd)
		lit = &ast.BadExpr{}
	}

	return
}

// set = '{' [ element {',' element} ] '}'
// element = expression ['..' expression]
func (p *Parser) parseSetElem() ast.Expression {
	expr := p.parseExpression()
	if p.tok == token.RANGE {
		p.next()
		return &ast.ExprRange{Beg: expr, End: p.parseExpression()}
	}

	return expr
}

func (p *Parser) parseNamedType() ast.Type {
	typ := p.parseQualifiedIdent()
	if basicTypes[typ.String()] {
		return ast.NewBasicType(typ.String())
	}

	return ast.NewNamedType(typ)

}

// qualident = [ ident '.' ] ident
func (p *Parser) parseQualifiedIdent() *ast.QualifiedIdent {
	id := &ast.QualifiedIdent{}

	// Parse the first identifier
	id.Prefix = p.parseIdent()

	if p.tok == token.PERIOD {
		p.next()
		id.Name = p.parseIdent()
	} else {
		id.Name = id.Prefix
		id.Prefix = ""
	}

	return id
}

// IdentList = identdef { [','] identdef}
// identdef  = ident ['*' | '-']
func (p *Parser) parseIdentList() (list []*ast.IdentifierDef) {
	list = append(list, p.parseIdentifierDef())

	for p.tok == token.COMMA {
		p.next()
		list = append(list, p.parseIdentifierDef())
	}

	return
}

// IdentDef = ident ['*' | '-']
func (p *Parser) parseIdentifierDef() *ast.IdentifierDef {
	id := &ast.IdentifierDef{Name: p.parseIdent()}

	switch p.tok {
	case token.STAR:
		id.Props = ast.Exported
	case token.MINUS:
		id.Props = ast.ExportedReadOnly
	default:
		id.Props = ast.Unexported
	}

	if p.tok == token.STAR || p.tok == token.MINUS {
		p.next()
	}

	return id
}

// ProcedureDeclaration = ProcedureHeading [ ';' ] ProcedureBody END [ ident ]
func (p *Parser) parseProcedureDecl() (proc *ast.ProcedureDecl) {
	proc = &ast.ProcedureDecl{}

	proc.Head = p.parseProcHeading()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	proc.Body = p.parseProcBody()

	p.match(token.END)
	if p.tok == token.IDENTIFIER {
		proc.EndName = p.parseIdent()
	}

	return
}

// ProcedureHeading = ( PROCEDURE | PROC ) [Receiver] identdef [ FormalParameters ]
// Receiver = '(' [VAR|IN] ident ':' ident ')'
func (p *Parser) parseProcHeading() (head *ast.ProcedureHeading) {
	head = &ast.ProcedureHeading{}

	if p.tok != token.PROC && p.tok != token.PROCEDURE {
		p.errorExpected("proc or procedure")
		p.advance(declStart)
		return nil
	} else {
		p.next()
		if p.tok == token.LPAREN {
			head.Rcv = p.parseReceiver()
		}

		head.Name = p.parseIdentifierDef()
		if p.tok == token.LPAREN {
			head.FP = p.parseFormalParameters()
		}
	}

	return
}

// FormalParameters = '(' [ FPSection { [';'] FPSection } ] ')' [ ':' ReturnType ]
func (p *Parser) parseFormalParameters() (fp *ast.FormalParams) {
	fp = &ast.FormalParams{}

	p.match(token.LPAREN)

	if p.tok == token.VAR || p.tok == token.IN || p.tok == token.IDENTIFIER {
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
	rcv.Type = p.parseNamedType()

	p.match(token.RPAREN)

	return
}

// ProcedureBody = DeclarationSequence [ BEGIN StatementSequence | ReturnStatement [ ';' ] ]
func (p *Parser) parseProcBody() (body *ast.ProcedureBody) {
	body = &ast.ProcedureBody{}

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

func (p *Parser) parseIdent() string {
	name := "_"
	if p.tok == token.IDENTIFIER {
		name = p.lit
		p.next()
	} else {
		p.match(token.IDENTIFIER)
	}

	return name
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
	case token.IDENTIFIER:
		dsg := p.parseDesignator()
		switch p.tok {
		case token.BECOMES:
			p.next()
			stmt = &ast.AssignmentStmt{LValue: dsg, RValue: p.parseExpression()}
		default:
			var list []ast.Expression

			if p.tok == token.LPAREN {
				list = p.parseActualParameters()
			}
			stmt = &ast.ProcedureCall{Callee: dsg, ActualParams: list}
		}
	default:
		p.errorExpected("statement")
		p.advance(stmtStart)
		stmt = &ast.BadStmt{}
	}

	return
}

func (p *Parser) parseLoopStmt() (stmt *ast.LoopStmt) {
	stmt = &ast.LoopStmt{}

	p.match(token.LOOP)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.END)

	return
}

func (p *Parser) parseRepeatStmt() (stmt *ast.RepeatStmt) {
	stmt = &ast.RepeatStmt{}

	p.match(token.REPEAT)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.UNTIL)
	stmt.BoolExpr = p.parseExpression()

	return
}

func (p *Parser) parseWhileStmt() (stmt *ast.WhileStmt) {
	stmt = &ast.WhileStmt{}

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
	stmt = &ast.ReturnStmt{}

	p.match(token.RETURN)
	if p.exprStart() {
		stmt.Value = p.parseExpression()
	}

	return
}

// IfStatement = IF expression THEN StatementSequence {ElsifStatement} [ElseStatement] END
func (p *Parser) parseIfStmt() (stmt *ast.IfStmt) {
	stmt = &ast.IfStmt{}

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
	stmt = &ast.CaseStmt{}

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
	stmt = &ast.ForStmt{}

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
	p.match(token.EXIT)

	return &ast.ExitStmt{}
}

// WithStatement = WITH ['|'] Guard DO StatementSequence { '|' Guard DO StatementSequence} [ ELSE StatementSequence ] END
// Guard         = qualident ':' qualident
func (p *Parser) parseWithStmt() *ast.WithStmt {
	stmt := &ast.WithStmt{}

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
