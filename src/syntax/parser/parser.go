package parser

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Parser struct {
	errors ErrorList
	lex    *Lexer

	// Next token
	tok token.Token
	lit string
}

func (p *Parser) InitParser(lex *Lexer) {
	p.lex = lex

	p.next()
}

func (p *Parser) error(msg string) {
	n := len(p.errors)
	if n > 10 {
		for _, err := range p.errors {
			println(err)
		}

		panic("too many errors")
	}

	p.errors.Append(msg)
}

func (p *Parser) match(tok token.Token) {
	if p.tok != tok {
		p.error(fmt.Sprintf("expected %v, but found %v", tok, p.tok))
	}

	p.next()
}

func (p *Parser) next() {
	p.tok, p.lit = p.lex.Lex()
}

func (p *Parser) Oberon() *ast.Oberon {
	ob := ast.NewOberon()

	switch p.tok {
	case token.MODULE:
		mod := p.parseModule()
		ob.Program[mod.BeginName.Name] = mod
	case token.DEFINITION:
	default:
		p.error(fmt.Sprintf("expected MODULE or DEFINITION, found %v", p.tok))
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

	for p.beginsImportOrDecl() {
		switch p.tok {
		case token.IMPORT:
		case token.VAR, token.TYPE, token.CONST, token.PROC, token.PROCEDURE:
			mod.DeclSeq = p.parseDeclarationSeq()
		default:
			p.error(fmt.Sprintf("expected an import or declaration, found %v", p.tok))
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

func (p *Parser) beginsImportOrDecl() bool {
	return p.tok == token.IMPORT ||
		p.tok == token.VAR ||
		p.tok == token.TYPE ||
		p.tok == token.CONST ||
		p.tok == token.PROC ||
		p.tok == token.PROCEDURE
}

func (p *Parser) parseDeclarationSeq() (seq []ast.Declaration) {
	switch p.tok {
	case token.VAR:
		p.next()
		seq = append(seq, p.varDecl())
	case token.TYPE:
	case token.CONST:
	case token.PROC, token.PROCEDURE:
		seq = append(seq, p.parseProcDecl())
	default:

	}

	return seq
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) varDecl() (v *ast.VarDecl) {
	v = &ast.VarDecl{}

	v.IdentList = p.parseIdentList()
	p.match(token.COLON)
	v.Type = p.parseType()

	return
}

func (p *Parser) parseType() ast.Expression {
	switch p.tok {
	case token.IDENT:
		typ := p.parseNamedType()
		if typ == nil {
			p.errors.Append("expected type")
			p.advance(exprEnd)
			return &ast.BadExpr{}
		}

		return typ
	case token.ARRAY:
	case token.RECORD:
	case token.POINTER, token.CARET:
	case token.LPAREN:
	default:

	}

	return nil
}

// advance consumes tokens until the current token p.tok
// is in the 'to' set, or token.EOF. For error recovery.
func (p *Parser) advance(to map[token.Token]bool) {
	for ; p.tok != token.EOF; p.next() {
		if to[p.tok] {
			return
		}
	}
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
		relExpr := &ast.BinaryExpr{X: expr, Op: p.tok}
		p.next()

		relExpr.Y = p.parseSimpleExpression()
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

	for p.addOp() {
		binExpr := &ast.BinaryExpr{X: expr, Op: p.tok}
		p.next()

		binExpr.Y = p.parseTerm()
		expr = binExpr
	}

	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{Op: sign, X: expr}
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
		binExpr := &ast.BinaryExpr{X: expr, Op: p.tok}
		p.next()

		binExpr.Y = p.parseFactor()
		expr = binExpr
	}

	return
}

// factor = literal
//
//		  | designator [ActualParameters]
//	      | '(' expression ')'
//	      | '~' factor
func (p *Parser) parseFactor() (expr ast.Expression) {
	switch p.tok {
	case token.TILDE:
	case token.LPAREN:
		p.match(token.LPAREN)
		expr = p.parseExpression()
		p.match(token.RPAREN)
	case token.IDENT: // designator [ActualParameters]
		expr = p.parseDesignator()
		if p.tok == token.LPAREN {
			expr = &ast.FuncCall{Dsg: expr, ActualParams: p.parseActualParameters()}
		}
	case token.INT: // literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
		expr = p.parseLiteral()
	}

	return
}

// ActualParameters = '(' [ExpList] ')'
func (p *Parser) parseActualParameters() (list []ast.Expression) {
	p.match(token.LPAREN)

	if p.tok == token.PLUS || p.tok == token.MINUS || p.tok == token.TILDE || p.tok == token.LPAREN ||
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
	d = &ast.Designator{}

	d.QualifiedIdent = p.parseQualifiedIdent(nil)

	//for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LPAREN {
	//	switch p.tok {
	//	case token.PERIOD:
	//	case token.LBRACK:
	//	case token.CARET:
	//	case token.LPAREN:
	//	}
	//}

	return
}

func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT:
		lit = &ast.UInt{Value: p.lit}
		p.next()
	}

	return
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
	return p.parseQualifiedIdent(nil)
}

func (p *Parser) parseQualifiedIdent(id *ast.Ident) ast.Expression {
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
	proc = &ast.ProcDecl{}

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
		p.error(fmt.Sprintf("expected 'proc' or 'procedure', found %v", p.tok))
		p.advance(declStart)
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
		id.Exported = true
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
	name := "_"
	if p.tok == token.IDENT {
		name = p.lit
		p.next()
	} else {
		p.match(token.IDENT)
	}

	return &ast.Ident{Name: name}
}

func (p *Parser) metaParams() {}

// statement = [
//
//	assignment | ProcedureCall
//	| IfStatement | CaseStatement
//	| WithStatement | LoopStatement
//	| ExitStatement | ReturnStatement
//	| WhileStatement | RepeatStatement | ForStatement ]
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
		case token.ASSIGN:
			p.next()
			stmt = &ast.AssignStmt{LValue: dsg, RValue: p.parseExpression()}
		case token.LPAREN:
			stmt = &ast.ProcCall{ProcName: dsg, ActualParams: p.parseActualParameters()}
		default:
			p.errors.Append(
				fmt.Sprintf("expected '%v' or '%v', found %v", token.ASSIGN, token.LPAREN, p.tok))
		}
	}

	return
}

func (p *Parser) parseLoopStmt() (stmt *ast.LoopStmt) {
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
	p.match(token.RETURN)
	stmt = &ast.ReturnStmt{Value: p.parseExpression()}

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
