package parser

import (
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Parser struct {
	errors lexer.ErrorList
	lex    *lexer.Lexer

	// Next token
	tok token.Token
	lit string
	pos *token.Position

	syncPos *token.Position // last synchronization position
	syncCnt int             // number of parser.advance calls without progress
}

func (p *Parser) InitParser(lex *lexer.Lexer) {
	p.lex = lex

	p.next()
}

func (p *Parser) error(pos *token.Position, msg string) {
	n := len(p.errors)
	if n > 10 {
		for _, err := range p.errors {
			println(err.Error())
		}

		panic("too many errors")
	}

	p.errors.Append(pos, msg)
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

	p.error(pos, msg)
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

func (p *Parser) Oberon() *ast.Oberon {
	ob := ast.NewOberon()

	switch p.tok {
	case token.MODULE:
		mod := p.parseModule()
		ob.Program[mod.BeginName.Name] = mod
	case token.DEFINITION:
	default:
		p.errorExpected(p.pos, "MODULE or DEFINITION")
	}

	return ob
}

func (p *Parser) parseModule() *ast.Module {
	mod := new(ast.Module)

	p.match(token.MODULE)
	mod.BeginName = p.parseIdent()

	if p.tok == token.LPAREN {
		p.metaParams()
	}

	if p.tok == token.SEMICOLON {
		p.next()
	}

	for p.startsImportOrDecl() {
		switch p.tok {
		case token.IMPORT:
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
	mod.EndName = p.parseIdent()
	if p.tok == token.PERIOD {
		p.next()
	}

	return mod
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

func (p *Parser) parseDeclarationSeq() (seq []ast.Declaration) {
	for p.startsDecl() {
		switch p.tok {
		case token.VAR:
			seq = append(seq, p.parseVarDecl())
		case token.TYPE:
			seq = append(seq, p.parseTypeDecl())
		case token.CONST:
			seq = append(seq, p.parseConstDecl())
		case token.PROC, token.PROCEDURE:
			seq = append(seq, p.parseProcDecl())
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

	p.match(token.TYPE)
	t.Name = p.parseIdentDef()
	p.match(token.EQUAL)
	t.DenotedType = p.parseType()

	return t
}

// ConstDeclaration = identdef '=' ConstExpression
func (p *Parser) parseConstDecl() *ast.ConstDecl {
	c := &ast.ConstDecl{Const: p.pos}

	p.match(token.CONST)

	c.Name = p.parseIdentDef()
	p.match(token.EQUAL)
	c.Value = p.parseExpression()

	return c
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVarDecl() (v *ast.VarDecl) {
	v = &ast.VarDecl{Var: p.pos}

	p.match(token.VAR)

	v.IdentList = p.parseIdentList()
	p.match(token.COLON)
	v.Type = p.parseType()

	return
}

func (p *Parser) parseType() ast.Expression {
	typ := p.parseIdentOrType()

	if typ == nil {
		pos := p.pos
		p.errorExpected(pos, "type")
		p.advance(exprEnd)
		return &ast.BadExpr{From: pos, To: p.pos}
	}

	return typ
}

func (p *Parser) parseIdentOrType() ast.Expression {
	switch p.tok {
	case token.IDENT:
		typ := p.parseNamedType()
		return typ
	case token.ARRAY, token.LBRACK:
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
			p.match(token.RBRACK)
			typ := p.parseType()

			return ast.NewArray(pos, ll, typ)
		}

		p.match(token.OF)

		return ast.NewArray(pos, ll, p.parseType())

	case token.RECORD:
	case token.PROCEDURE:
	case token.POINTER, token.CARET:
	case token.LPAREN:
	}

	return nil
}

// advance consumes tokens until the current token p.tok
// is in the 'to' set, or token.EOF. For error recovery.
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

	for p.addOp() {
		binExpr := &ast.BinaryExpr{OpPos: p.pos, Left: expr, Op: p.tok}
		p.next()

		binExpr.Right = p.parseTerm()
		expr = binExpr
	}

	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{OpPos: pos, Op: sign, X: expr}
	}

	return
}

func (p *Parser) relation() bool {
	return p.tok == token.EQUAL ||
		p.tok == token.LESS ||
		p.tok == token.LEQ ||
		p.tok == token.GREAT ||
		p.tok == token.GEQ ||
		p.tok == token.IN ||
		p.tok == token.IS ||
		p.tok == token.HASH
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
			expr = &ast.FuncCall{Dsg: expr, ActualParams: p.parseActualParameters()}
		}
	case token.INT, token.REAL, token.STRING, token.HEXSTR, token.CHAR, token.NIL, token.TRUE, token.FALSE, token.LBRACE:
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

	if p.tok == token.PLUS || p.tok == token.MINUS || p.tok == token.NOT || p.tok == token.LPAREN ||
		p.tok == token.IDENT || p.tok == token.INT /* or other literals */ {

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
// selector = '.' ident | '[' ExpList ']' | '^' | '(' qualident ')'
func (p *Parser) parseDesignator() (d *ast.Designator) {
	d = &ast.Designator{QIdentPos: p.pos}

	d.QualifiedIdent = p.parseQualifiedIdent(nil)

	//for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LPAREN {
	//	switch p.tok {
	//	case token.PERIOD:
	//		p.next()
	//		d.Selector = &ast.DotOp{Field: p.parseIdent()}
	//	case token.LBRACK:
	//		p.next()
	//		List := p.parseExprList()
	//		p.next()
	//		d.Selector = &ast.IndexOp{List: List}
	//	case token.CARET:
	//		d.Selector = &ast.PointerDeref{}
	//	case token.LPAREN:
	//		p.match(token.LPAREN)
	//		Typ := p.parseQualifiedIdent(d.QualifiedIdent)
	//		p.match(token.RPAREN)
	//		d.Selector = &ast.TypeGuard{Typ: Typ}
	//	}
	//}

	return
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT, token.REAL, token.STRING, token.HEXSTR, token.CHAR:
		lit = &ast.BasicLit{ValuePos: p.pos, Kind: p.tok, Value: p.lit}
		p.next()
	case token.TRUE, token.FALSE:
	case token.NIL:
	case token.LBRACE:
		p.match(token.LBRACE)
		set := &ast.Set{}

		if p.exprStart() {
			set.Elem = append(set.Elem, p.parseExpression())
			for p.tok == token.COMMA {
				p.next()
				set.Elem = append(set.Elem, p.parseExpression())
			}
		}

		p.match(token.RBRACE)

		lit = set
	}

	return
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
		p.tok == token.HEXSTR ||
		p.tok == token.CHAR ||
		p.tok == token.INT ||
		p.tok == token.REAL ||
		p.tok == token.NOT

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
		p.tok == token.AND
}

func (p *Parser) parseNamedType() ast.Expression {
	typ := p.parseQualifiedIdent(nil)
	return ast.NewBasicType(typ.String())
}

func (p *Parser) parseQualifiedIdent(id ast.Expression) ast.Expression {
	if id == nil {
		id = p.parseIdent()
	}

	if p.tok == token.PERIOD {
		p.next()
		sel := p.parseIdent()

		return &ast.QualifiedIdent{X: id, Sel: sel}
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

// ProcedureDeclaration = ProcedureHeading [ ';' ] ProcedureBody END ident
func (p *Parser) parseProcDecl() (proc *ast.ProcDecl) {
	proc = &ast.ProcDecl{Proc: p.pos}

	proc.Head = p.parseProcHeading()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	proc.Body = p.parseProcBody()

	p.match(token.END)
	p.match(token.IDENT)

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
		id.IProps |= ast.IsExported
		if p.tok == token.MINUS {
			id.IProps |= ast.IsReadOnly
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

func (p *Parser) metaParams() {}

func (p *Parser) parseStatement() (stmt ast.Statement) {
	switch p.tok {
	case token.LOOP:
		stmt = p.parseLoopStmt()
	case token.EXIT:
	case token.IF:
		stmt = p.parseIfStmt()
	case token.FOR:
	case token.WITH:
	case token.CASE:
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
			p.next()
			stmt = &ast.AssignStmt{LValue: dsg, RValue: p.parseExpression()}
		case token.LPAREN:
			stmt = &ast.ProcCall{Dsg: dsg, ActualParams: p.parseActualParameters()}
		default:
			pos := p.pos
			p.errorExpected(p.pos, ":= or (")
			p.advance(exprEnd)
			stmt = &ast.BadStmt{From: pos, To: p.pos}
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
	stmt.Value = p.parseExpression()

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
