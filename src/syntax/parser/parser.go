package parser

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/scan"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Parser struct {
	err     report.Reporter
	envs    map[string]*ast.Environment
	env     *ast.Environment
	scanner *scan.Scanner

	// Next token
	tok token.Kind
	lit string
	rng *report.Range

	list []ast.Expression
}

func NewParser(scanner *scan.Scanner, rpt report.Reporter, env *ast.Environment, envs map[string]*ast.Environment) *Parser {
	p := &Parser{scanner: scanner, err: rpt, env: env, envs: envs}
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

	p.err.Report(report.Diagnostic{
		Severity: report.Error,
		Message:  msg,
		Pos:      p.rng.Start,
		Range:    p.rng,
	})
}

func (p *Parser) match(tok token.Kind) {
	if p.tok != tok {
		p.errorExpected("'" + tok.String() + "'")
	}

	p.next()
}

func (p *Parser) next() {
	tok := p.scanner.NextToken()
	p.tok = tok.Kind
	p.lit = tok.Lexeme
	p.rng = tok.Range
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
		ms.TyConst = ast.NewNamedType(p.parseQualifiedIdent())
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

	// add module imports to the environment
	for _, imp := range list {
		if imp.Alias != "" {
			if sym := p.env.Insert(ast.NewModuleSymbol(imp.Alias)); sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate module import",
					Pos:      p.rng.Start,
					Range:    p.rng,
				})
			}
		} else {
			if sym := p.env.Insert(ast.NewModuleSymbol(imp.Name)); sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate module import",
					Pos:      p.rng.Start,
					Range:    p.rng,
				})
			}
		}
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
	name := p.parseIdentifierDef()
	p.match(token.EQUAL)
	Typ := p.parseType()

	if sym := p.env.Insert(ast.NewTypeSymbol(name.Name, name.Props, Typ)); sym != nil {
		p.err.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "duplicate type declaration " + name.Name,
			Pos:      p.rng.Start,
			Range:    p.rng,
		})
	}

	return &ast.TypeDecl{Name: name, DenotedType: Typ}
}

// ConstDeclaration = identdef '=' ConstExpression
func (p *Parser) parseConstantDecl() *ast.ConstantDecl {
	name := p.parseIdentifierDef()
	p.match(token.EQUAL)
	value := p.parseExpression()

	if sym := p.env.Insert(ast.NewConstantSymbol(name.Name, name.Props, value)); sym != nil {
		p.err.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "duplicate constant declaration " + name.Name,
			Pos:      p.rng.Start,
			Range:    p.rng,
		})
	}

	return &ast.ConstantDecl{Name: name, Value: value}
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVariableDecl() *ast.VariableDecl {
	decl := &ast.VariableDecl{IdentList: p.parseIdentList()}
	p.match(token.COLON)
	decl.Type = p.parseType()

	// add the variables to the environment
	for _, id := range decl.IdentList {
		sym := p.env.Insert(ast.NewVariableSymbol(id.Name, id.Props, decl.Type))
		if sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate variable declaration " + id.Name,
				Pos:      p.rng.Start,
				Range:    p.rng,
			})
		}
	}

	return decl
}

func (p *Parser) parseType() (ty ast.Type) {
	switch p.tok {
	case token.INTEGER, token.BOOLEAN, token.CHAR, token.REAL, token.LONGREAL, token.INT8,
		token.INT16, token.INT32, token.INT64, token.WCHAR, token.BYTE, token.SHORTINT, token.LONGINT, token.SET:
		ty = ast.NewBasicType(p.lit)
		p.next()
	case token.IDENTIFIER:
		ty = ast.NewNamedType(p.parseQualifiedIdent())
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

	baseEnv := p.parseRecordBase(rec)
	rec.Env = ast.NewRecordEnv(baseEnv, p.env)

	if p.tok == token.IDENTIFIER {
		rec.Fields = p.parseRecordFields()
	}

	p.match(token.END)
	p.addFieldsToEnv(rec)

	return rec
}

func (p *Parser) parseRecordBase(rec *ast.RecordType) *ast.RecordEnv {
	if p.tok != token.LPAREN {
		return nil
	}

	p.next()
	rec.Base = ast.NewNamedType(p.parseQualifiedIdent())
	p.match(token.RPAREN)

	baseType, ok := rec.Base.(*ast.NamedType)
	if !ok {
		p.errorExpected("named type")
		return nil
	}

	base := p.lookupBaseType(baseType)
	if base == nil {
		return nil
	}

	recordType := p.getRecordType(base)
	if recordType == nil {
		return nil
	}

	return recordType.Env
}

func (p *Parser) lookupBaseType(baseType *ast.NamedType) ast.Symbol {
	if env, ok := p.envs[baseType.Name.Prefix]; ok {
		base := env.Lookup(baseType.Name.Name)
		if base == nil || base.Props() != ast.Exported {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("object %s is not exported", baseType.Name),
				Pos:      p.rng.Start,
				Range:    p.rng,
			})
			p.advance(declStart)
			return nil
		}
		return base
	}

	return p.env.Lookup(baseType.Name.Name)
}

func (p *Parser) getRecordType(base ast.Symbol) *ast.RecordType {
	typeSymbol, ok := base.(*ast.TypeSymbol)
	if !ok || typeSymbol == nil {
		p.errorExpected("record type")
		p.advance(declStart)
		return nil
	}

	switch ty := typeSymbol.Type().(type) {
	case *ast.RecordType:
		return ty
	case *ast.PointerType:
		if recordType, ok := ty.Base.(*ast.RecordType); ok {
			return recordType
		}
	}

	p.errorExpected("record type")
	p.advance(declStart)
	return nil
}

func (p *Parser) parseRecordFields() []*ast.FieldList {
	var fields []*ast.FieldList
	fields = append(fields, p.fieldList())

	for p.tok == token.SEMICOLON || p.tok == token.IDENTIFIER {
		if p.tok == token.SEMICOLON {
			p.next()
		}
		fields = append(fields, p.fieldList())
	}
	return fields
}

func (p *Parser) addFieldsToEnv(rec *ast.RecordType) {
	for _, field := range rec.Fields {
		for _, id := range field.List {
			sym := rec.Env.Insert(ast.NewFieldSymbol(id.Name, id.Props, field.Type))
			if sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate field declaration " + id.Name,
					Pos:      p.rng.Start,
					Range:    p.rng,
				})
			}
		}
	}
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
func (p *Parser) parseFactor() ast.Expression {
	switch p.tok {
	case token.NOT:
		p.next()
		return &ast.UnaryExpr{Op: token.NOT, Operand: p.parseFactor()}

	case token.LPAREN:
		p.match(token.LPAREN)
		expr := p.parseExpression()
		p.match(token.RPAREN)
		return expr

	case token.IDENTIFIER:
		dsg := p.parseDesignator()
		env := p.env
		if dsg.QIdent.Prefix != "" {
			env = p.envs[dsg.QIdent.Prefix]
		}

		if sym := env.Lookup(dsg.QIdent.Name); sym != nil &&
			sym.Kind() == ast.ProcedureSymbolKind &&
			p.tok == token.RPAREN {
			call := &ast.FunctionCall{Callee: dsg, ActualParams: p.list}
			p.list = nil
			p.next()
			return call
		}
		return dsg

	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.NIL, token.TRUE, token.FALSE, token.LBRACE:
		return p.parseLiteral()

	default:
		p.errorExpected("factor expression")
		p.advance(exprEnd)
		return &ast.BadExpr{}
	}
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
// selector = '.' ident | '[' ExpList ']' | '^' | '(' qualident ')'
func (p *Parser) parseDesignator() *ast.Designator {
	dsg := &ast.Designator{QIdent: p.parseQualifiedIdent()}

	for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LPAREN {
		switch p.tok {
		case token.PERIOD:
			p.next()
			dsg.Select = append(dsg.Select, &ast.DotOp{Field: p.parseIdent()})

		case token.LBRACK:
			p.next()
			list := p.parseExprList()
			p.match(token.RBRACK)
			dsg.Select = append(dsg.Select, &ast.IndexOp{List: list})

		case token.CARET:
			p.next()
			dsg.Select = append(dsg.Select, &ast.PtrDeref{})

		case token.LPAREN:
			p.next()
			if !p.parseTypeGuard(dsg) {
				return dsg
			}
		}
	}

	return dsg
}

func (p *Parser) parseTypeGuard(dsg *ast.Designator) bool {
	if !p.exprStart() {
		return false
	}

	p.list = p.parseExprList()
	if len(p.list) == 0 {
		return false
	}

	d, ok := p.list[0].(*ast.Designator)
	if !ok {
		return false
	}

	env := p.env
	if d.QIdent.Prefix != "" {
		env = p.envs[d.QIdent.Prefix]
	}

	sym := env.Lookup(d.QIdent.Name)
	if sym == nil || sym.Kind() != ast.TypeSymbolKind {
		return false
	}

	switch t := sym.Type().(type) {
	case *ast.RecordType:
	case *ast.PointerType:
		if _, ok := t.Base.(*ast.RecordType); !ok {
			return false
		}
	default:
		return false
	}

	dsg.Select = append(dsg.Select, &ast.TypeGuard{Ty: d.QIdent})
	p.match(token.RPAREN)
	return true
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.TRUE, token.FALSE:

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
	case token.NIL:
		p.next()
		lit = &ast.Nil{}
	default:
		p.errorExpected("literal or set")
		p.advance(exprStart)
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

//func (p *Parser) parseNamedType() ast.Type {
//	return ast.NewNamedType(p.parseQualifiedIdent())
//
//}

// qualident = [ ident '.' ] ident
func (p *Parser) parseQualifiedIdent() *ast.QualifiedIdent {
	id := &ast.QualifiedIdent{}

	// Parse the first identifier
	id.Prefix = p.parseIdent()

	sym := p.env.Lookup(id.Prefix)
	if sym != nil && sym.Kind() == ast.ModuleSymbolKind && p.tok == token.PERIOD {
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

	parent := p.env
	p.env = ast.NewEnvironment(parent, "")
	proc.Env = p.env
	defer func() {
		p.env = parent
	}()

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
	}

	p.next()

	if p.tok == token.LPAREN {
		head.Rcv = p.parseReceiver()
	}

	head.Name = p.parseIdentifierDef()
	if p.tok == token.LPAREN {
		head.FP = p.parseFormalParameters()
	}

	// check that head.Rcv.Type has been declared as a record type and retrieve its env
	// insert the receiver into the procedure's environment and the rest of the procedure's
	// parameters into the receiver's environment
	if head.Rcv != nil {
		sym := p.env.Lookup(head.Rcv.Type.String())
		if sym == nil {
			p.errorExpected("record type")
			p.advance(declStart)
			return head
		}

		typeSymbol, ok := sym.(*ast.TypeSymbol)
		if !ok || typeSymbol == nil {
			p.errorExpected("record type")
			p.advance(declStart)
			return head
		}

		var recordType *ast.RecordType
		switch t := typeSymbol.Type().(type) {
		case *ast.RecordType:
			recordType = t
		case *ast.PointerType:
			if t.Base == nil {
				p.errorExpected("record type")
				p.advance(declStart)
				return head
			}

			recordType, ok = t.Base.(*ast.RecordType)
			if !ok {
				p.errorExpected("record type")
				p.advance(declStart)
				return head
			}
		default:
			p.errorExpected("record type")
			p.advance(declStart)
			return head
		}

		// add the receiver to the procedure's parent environment
		if sym := p.env.Insert(ast.NewParamSymbol(head.Rcv.Var, head.Rcv.Mod, head.Rcv.Type)); sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate parameter declaration",
				Pos:      p.rng.Start,
				Range:    p.rng,
			})
		}

		// add the procedure to the receiver's environment
		if sym := recordType.Env.Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props)); sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate procedure declaration",
				Pos:      p.rng.Start,
				Range:    p.rng,
			})
		}
	}

	// add the formal parameters into the receiver's environment
	for _, param := range head.FP.Params {
		for _, id := range param.Names {
			if sym := p.env.Insert(ast.NewParamSymbol(id, param.Mod, param.Type)); sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate field declaration",
					Pos:      p.rng.Start,
					Range:    p.rng,
				})
			}
		}
	}

	return head
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
	rcv.Type = ast.NewNamedType(p.parseQualifiedIdent())

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
		case token.RPAREN:
			stmt = &ast.ProcedureCall{Callee: dsg, ActualParams: p.list}
			p.list = make([]ast.Expression, 0)
			p.next()
		default:
			stmt = &ast.BadStmt{}
			p.errorExpected("assignment or procedure call")
			p.advance(stmtStart)
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
