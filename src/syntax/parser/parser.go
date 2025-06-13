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
	sc  *scan.Scanner
	ctx *report.Context

	// Next token
	tok    token.Kind
	lexeme string
	pos    int
	end    int

	list []ast.Expression
}

func NewParser(ctx *report.Context) *Parser {
	p := &Parser{sc: scan.Scan(ctx), ctx: ctx}
	p.next()

	return p
}

func (p *Parser) errorExpected(msg string) {
	msg = "expected " + msg

	switch {
	case p.tok.IsLiteral():
		msg += ", found " + p.lexeme
	default:
		msg += ", found '" + p.tok.String() + "'"
	}

	p.ctx.Reporter.Report(report.Diagnostic{
		Severity: report.Error,
		Message:  msg,
		Range:    p.ctx.Source.Span(p.ctx.FileName, p.pos, p.end),
	})
}

func (p *Parser) match(tok token.Kind) {
	if p.tok != tok {
		p.errorExpected("'" + tok.String() + "'")
	}

	p.next()
}

func (p *Parser) next() {
	for {
		tok := p.sc.NextToken()
		p.tok = tok.Kind
		p.lexeme = tok.Lexeme
		p.pos = tok.Pos
		p.end = tok.End

		switch tok.Kind {
		case token.SL_COMMENT_START:
			// Skip until end of line
			for tok.Kind != token.EOF && tok.Kind != token.NEWLINE {
				tok = p.sc.NextToken()
			}
		case token.ML_COMMENT_START:
			// Skip until ML_COMMENT_END
			for tok.Kind != token.EOF && tok.Kind != token.ML_COMMENT_END {
				tok = p.sc.NextToken()
			}
			if tok.Kind == token.ML_COMMENT_END {
				tok = p.sc.NextToken() // Consume the end token
			}
		case token.NEWLINE:
			continue
		default:
			return
		}
	}

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
	pos := p.pos
	ms := &ast.MetaSection{StartOffset: pos}

	if p.tok == token.TYPE || p.tok == token.CONST {
		ms.Mode = p.tok
		p.next()
	}

	ms.EndOffset = p.end
	ms.Ids = append(ms.Ids, p.parseIdentifierDef())
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
		if p.tok == token.COMMA {
			p.next()
		}

		ms.EndOffset = p.end
		ms.Ids = append(ms.Ids, p.parseIdentifierDef())
	}

	if p.tok == token.COLON {
		p.next()
		q := p.parseQualifiedIdent()
		ms.TyConst = ast.NewNamedType(q, q.StartOffset, q.EndOffset)
		ms.EndOffset = q.EndOffset
	}

	return ms
}

func (p *Parser) parseModule() *ast.Module {
	pos := p.pos
	mod := ast.NewModule(pos, p.ctx.Env)

	p.match(token.MODULE)
	mod.BName = p.parseIdent()

	if p.ctx.Env == nil || p.ctx.Env.Name() != mod.BName {
		p.ctx.Env = ast.NewEnvironment(ast.GlobalEnviron, mod.BName)
	}
	mod.Env = p.ctx.Env

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
			p.errorExpected("import or declaration")
			p.advance(declStart)
			mod.DeclSeq = append(mod.DeclSeq, &ast.BadDecl{StartOffset: pos, EndOffset: p.pos})
		}
	}

	if p.tok == token.BEGIN {
		p.next()
		mod.StmtSeq = p.parseStatementSeq()
	}

	p.match(token.END)
	mod.EndOffset = p.end
	mod.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		mod.EndOffset = p.end
		p.next()
	}

	return mod
}

// definition   = DEFINITION ident [';']  [ ImportList ] DeclarationSequence2 END ident ['.']
func (p *Parser) parseDefinition() *ast.Definition {
	pos := p.pos
	def := ast.NewDefinition(pos)

	p.match(token.DEFINITION)
	def.BName = p.parseIdent()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	def.Env = ast.NewEnvironment(ast.GlobalEnviron, def.BName)
	p.ctx.Env = def.Env

	if p.tok == token.IMPORT {
		def.ImportList = p.parseImportList()
	}

	for p.startsDecl() {
		def.DeclSeq = p.parseDeclarationSeq2()
	}

	p.match(token.END)
	def.EndOffset = p.end
	def.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		def.EndOffset = p.end
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
			if sym := p.ctx.Env.Insert(ast.NewImportSymbol(imp.Alias)); sym != nil {
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate module import",
					Range:    p.ctx.Source.Span(p.ctx.FileName, imp.StartOffset, imp.EndOffset),
				})
			}
		} else {
			if sym := p.ctx.Env.Insert(ast.NewImportSymbol(imp.Name)); sym != nil {
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate module import",
					Range:    p.ctx.Source.Span(p.ctx.FileName, imp.StartOffset, imp.EndOffset),
				})
			}
		}
	}

	return list
}

// import = [ ident ':=' ] ImportPath ident [ MetaActuals ]
// ImportPath = { ident '.' }
func (p *Parser) parseImport() *ast.Import {
	pos := p.pos
	imp := &ast.Import{StartOffset: pos}

	id := p.parseIdent()
	if p.tok == token.BECOMES {
		imp.Alias = id
		p.match(token.BECOMES)
		id = p.parseIdent()
	}

	for {
		imp.ImportPath = append(imp.ImportPath, id)
		if p.tok != token.PERIOD {
			break
		}
		p.match(token.PERIOD)
		id = p.parseIdent()
	}

	imp.Name = strings.Join(imp.ImportPath, ".")
	imp.EndOffset = p.end

	if p.tok == token.LPAREN {
		p.next()
		imp.Meta = append(imp.Meta, p.parseExpression())

		for p.tok == token.COMMA {
			p.next()
			imp.Meta = append(imp.Meta, p.parseExpression())
		}

		imp.EndOffset = p.end
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
			pos := p.pos
			p.match(token.VAR)
			for p.tok == token.IDENTIFIER {
				decl := p.parseVariableDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			pos := p.pos
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				decl := p.parseTypeDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			pos := p.pos
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				decl := p.parseConstantDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
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
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{StartOffset: pos, EndOffset: p.pos})
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
			pos := p.pos
			p.match(token.VAR)
			for p.tok == token.IDENTIFIER {
				decl := p.parseVariableDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			pos := p.pos
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				decl := p.parseTypeDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			pos := p.pos
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				decl := p.parseConstantDecl()
				decl.StartOffset = pos

				seq = append(seq, decl)
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
			pos := p.pos
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{StartOffset: pos, EndOffset: p.pos})
		}
	}

	return seq
}

// TypeDeclaration = identdef '=' type
func (p *Parser) parseTypeDecl() *ast.TypeDecl {
	name := p.parseIdentifierDef()
	p.match(token.EQUAL)
	Typ := p.parseType()

	if sym := p.ctx.Env.Insert(ast.NewTypeSymbol(name.Name, name.Props, Typ)); sym != nil {
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "duplicate type declaration " + name.Name,
			Range:    p.ctx.Source.Span(p.ctx.FileName, name.StartOffset, name.EndOffset),
		})
	}

	return &ast.TypeDecl{
		Name:        name,
		DenotedType: Typ,
		EndOffset:   Typ.End(),
	}
}

// ConstDeclaration = identdef '=' ConstExpression
func (p *Parser) parseConstantDecl() *ast.ConstantDecl {
	name := p.parseIdentifierDef()
	p.match(token.EQUAL)
	value := p.parseExpression()

	if sym := p.ctx.Env.Insert(ast.NewConstantSymbol(name.Name, name.Props, value)); sym != nil {
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "duplicate constant declaration " + name.Name,
			Range:    p.ctx.Source.Span(p.ctx.FileName, name.StartOffset, name.EndOffset),
		})
	}

	return &ast.ConstantDecl{
		Name:      name,
		Value:     value,
		EndOffset: value.End(),
	}
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVariableDecl() *ast.VariableDecl {
	decl := &ast.VariableDecl{IdentList: p.parseIdentList()}
	p.match(token.COLON)
	decl.Type = p.parseType()

	decl.EndOffset = decl.Type.End()

	// add the variables to the environment
	for _, id := range decl.IdentList {
		sym := p.ctx.Env.Insert(ast.NewVariableSymbol(id.Name, id.Props, decl.Type))
		if sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate variable declaration " + id.Name,
				Range:    p.ctx.Source.Span(p.ctx.FileName, id.StartOffset, id.EndOffset),
			})
		}
	}

	return decl
}

func (p *Parser) parseType() (ty ast.Type) {
	switch p.tok {
	case token.INTEGER, token.BOOLEAN, token.CHAR, token.REAL, token.LONGREAL, token.INT8,
		token.INT16, token.INT32, token.INT64, token.WCHAR, token.BYTE, token.SHORTINT, token.LONGINT, token.SET:
		ty = ast.NewBasicType(p.tok, p.pos, p.end)
		p.next()
	case token.IDENTIFIER:
		q := p.parseQualifiedIdent()
		ty = ast.NewNamedType(q, q.StartOffset, q.EndOffset)
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
		pos := p.pos
		p.errorExpected("type")
		p.advance(declStart)
		return &ast.BadType{StartOffset: pos, EndOffset: p.pos}
	}

	return
}

func (p *Parser) parseEnumType() *ast.EnumType {
	pos := p.pos
	enum := &ast.EnumType{StartOffset: pos}
	p.next()

	enum.Variants = append(enum.Variants, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
		if p.tok == token.COMMA {
			p.next()
		}
		enum.Variants = append(enum.Variants, p.parseIdent())
	}

	enum.EndOffset = p.pos
	p.match(token.RPAREN)

	return enum
}

func (p *Parser) parsePtrType() *ast.PointerType {
	pos := p.pos

	ptr := &ast.PointerType{StartOffset: pos}
	tok := p.tok

	p.next()
	if tok == token.POINTER {
		p.match(token.TO)
	}

	ptr.Base = p.parseType()
	ptr.EndOffset = ptr.Base.End()

	return ptr
}

func (p *Parser) parseProcedureType() *ast.ProcedureType {
	//pos := p.pos
	proc := &ast.ProcedureType{StartOffset: p.pos, EndOffset: p.end}
	p.match(token.PROCEDURE)

	if p.tok == token.LPAREN {
		proc.FP = p.parseFormalParameters()
		if proc.FP != nil && len(proc.FP.Params) != 0 {
			last := proc.FP.Params[len(proc.FP.Params)-1]

			if proc.FP.RetType != nil {
				proc.EndOffset = proc.FP.RetType.End()
			} else {
				proc.EndOffset = last.End()
			}
		}
	}

	return proc
}

func (p *Parser) parseArrayType() *ast.ArrayType {
	pos := p.pos
	array := &ast.ArrayType{StartOffset: pos}

	if p.tok == token.ARRAY {
		p.next()
		if p.tok != token.OF {
			array.LenList = p.parseLengthList()
		}
		p.match(token.OF)
		array.ElemType = p.parseType()
	} else if p.tok == token.LBRACK {
		p.next()
		if p.tok != token.RBRACK {
			array.LenList = p.parseLengthList()
		}
		p.match(token.RBRACK)
		array.ElemType = p.parseType()
	}

	array.EndOffset = array.ElemType.End()

	return array
}

func (p *Parser) parseLengthList() *ast.LenList {
	pos := p.pos
	list := &ast.LenList{Modifier: token.ILLEGAL, StartOffset: pos}

	if p.tok == token.VAR {
		list.Modifier = p.tok
		p.next()
	}

	list.List = append(list.List, p.parseExpression())
	for p.tok == token.COMMA {
		p.next()
		list.List = append(list.List, p.parseExpression())
	}

	last := list.List[len(list.List)-1]
	list.EndOffset = last.End()

	return list
}

func (p *Parser) parseRecordType() *ast.RecordType {
	pos := p.pos

	rec := &ast.RecordType{StartOffset: pos}
	p.match(token.RECORD)

	baseEnv := p.parseRecordBase(rec)
	rec.Env = ast.NewRecordEnv(baseEnv, p.ctx.Env)

	if p.tok == token.IDENTIFIER {
		rec.Fields = p.parseRecordFields()
	}

	rec.EndOffset = p.end
	p.match(token.END)
	p.addFieldsToEnv(rec)

	return rec
}

func (p *Parser) parseRecordBase(rec *ast.RecordType) *ast.RecordEnv {
	if p.tok != token.LPAREN {
		return nil
	}

	p.next()
	q := p.parseQualifiedIdent()
	rec.Base = ast.NewNamedType(q, q.StartOffset, q.EndOffset)
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
	if env, ok := p.ctx.Envs[baseType.Name.Prefix]; ok {
		base := env.Lookup(baseType.Name.Name)
		if base == nil || base.Props() != ast.Exported {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("object %s is not exported", baseType.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, baseType.Name.StartOffset, baseType.Name.EndOffset),
			})
			p.advance(declStart)
			return nil
		}
		return base
	}

	return p.ctx.Env.Lookup(baseType.Name.Name)
}

func (p *Parser) getRecordType(base ast.Symbol) *ast.RecordType {
	typeSymbol, ok := base.(*ast.TypeSymbol)
	if !ok || typeSymbol == nil {
		p.errorExpected("record type")
		p.advance(declStart)
		return nil
	}

	switch ty := typeSymbol.TypeNode().(type) {
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
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate field declaration " + id.Name,
					Range:    p.ctx.Source.Span(p.ctx.FileName, id.StartOffset, id.EndOffset),
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
		relExpr := &ast.BinaryExpr{Left: expr, Op: p.tok, StartOffset: expr.Pos()}
		p.next()

		relExpr.Right = p.parseSimpleExpression()
		relExpr.EndOffset = relExpr.Right.End()

		expr = relExpr
	}

	return expr
}

// SimpleExpression = ['+' | '-'] term { AddOperator term }
func (p *Parser) parseSimpleExpression() (expr ast.Expression) {
	var sign = token.ILLEGAL

	pos := p.pos
	if p.tok == token.MINUS || p.tok == token.PLUS {
		sign = p.tok
		p.next()
	}

	expr = p.parseTerm()
	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{Op: sign, Operand: expr, StartOffset: pos, EndOffset: expr.End()}
	}

	for p.addOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok, StartOffset: pos}
		p.next()

		bin.Right = p.parseTerm()
		bin.EndOffset = bin.Right.End()

		expr = bin
	}

	return
}

// term = factor {MulOperator factor}
func (p *Parser) parseTerm() (expr ast.Expression) {
	expr = p.parseFactor()

	for p.mulOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok, StartOffset: expr.Pos()}
		p.next()

		bin.Right = p.parseFactor()
		bin.EndOffset = bin.Right.End()

		expr = bin
	}

	return
}

// factor = literal | designator [ActualParameters] | '(' expression ')' | '~' factor
func (p *Parser) parseFactor() ast.Expression {
	switch p.tok {
	case token.NOT:
		pos := p.pos
		p.next()

		operand := p.parseFactor()
		return &ast.UnaryExpr{
			Op:          token.NOT,
			Operand:     operand,
			StartOffset: pos,
			EndOffset:   operand.End(),
		}

	case token.LPAREN:
		p.match(token.LPAREN)
		expr := p.parseExpression()
		p.match(token.RPAREN)
		return expr

	case token.IDENTIFIER:
		dsg := p.parseDesignator()
		env := p.ctx.Env
		if dsg.QIdent.Prefix != "" {
			if env = p.ctx.Envs[dsg.QIdent.Prefix]; env == nil {
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Fatal,
					Message:  fmt.Sprintf("cannot find definition or module for %s", dsg.QIdent.Prefix),
					Range:    p.ctx.Source.Span(p.ctx.FileName, dsg.StartOffset, dsg.EndOffset),
				})

				p.advance(exprEnd)
				return &ast.BadExpr{StartOffset: dsg.StartOffset, EndOffset: dsg.EndOffset}
			}
		}

		if sym := env.Lookup(dsg.QIdent.Name); sym != nil && sym.Kind() == ast.ProcedureSymbolKind && p.tok == token.RPAREN {
			call := &ast.FunctionCall{
				Callee:       dsg,
				ActualParams: p.list,
				StartOffset:  dsg.Pos(),
				EndOffset:    p.end,
			}
			p.list = nil
			p.next()
			return call
		}

		return dsg

	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.NIL, token.TRUE, token.FALSE, token.LBRACE:
		return p.parseLiteral()

	default:
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  "unexpected token: " + p.lexeme + "does not initiate an expression",
			Range:    p.ctx.Source.Span(p.ctx.FileName, p.pos, p.end),
		})
		pos := p.pos
		p.advance(exprEnd)
		return &ast.BadExpr{StartOffset: pos, EndOffset: p.pos}
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
	qident := p.parseQualifiedIdent()
	dsg := &ast.Designator{QIdent: qident, StartOffset: qident.Pos(), EndOffset: qident.EndOffset}

	for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LPAREN {
		switch p.tok {
		case token.PERIOD:
			pos := p.pos
			p.next()

			dot := &ast.DotOp{StartOffset: pos, EndOffset: p.end}
			dot.Field = p.parseIdent()

			dsg.Select = append(dsg.Select, dot)
			dsg.EndOffset = dot.EndOffset
		case token.LBRACK:
			pos := p.pos
			p.next()

			list := p.parseExprList()

			end := p.end
			p.match(token.RBRACK)
			dsg.Select = append(dsg.Select, &ast.IndexOp{
				List:        list,
				StartOffset: pos,
				EndOffset:   end,
			})

			dsg.EndOffset = end
		case token.CARET:
			pos := p.pos
			end := p.end

			p.next()

			dsg.Select = append(dsg.Select, &ast.PtrDeref{
				StartOffset: pos,
				EndOffset:   end,
			})

			dsg.EndOffset = end
		case token.LPAREN:
			pos := p.pos
			p.next()
			if !p.parseTypeGuard(dsg, pos) {
				return dsg
			}
		}
	}

	return dsg
}

func (p *Parser) parseTypeGuard(dsg *ast.Designator, pos int) bool {
	if !p.exprStart() {
		return false
	}

	p.list = p.parseExprList()
	if len(p.list) != 1 {
		return false
	}

	d, ok := p.list[0].(*ast.Designator)
	if !ok {
		return false
	}

	env := p.ctx.Env
	if d.QIdent.Prefix != "" {
		env = p.ctx.Envs[d.QIdent.Prefix]
	}

	sym := env.Lookup(d.QIdent.Name)
	if sym == nil || sym.Kind() != ast.TypeSymbolKind {
		return false
	}

	switch t := sym.(*ast.TypeSymbol).TypeNode().(type) {
	case *ast.RecordType:
	case *ast.PointerType:
		if _, ok := t.Base.(*ast.RecordType); !ok {
			return false
		}
	default:
		return false
	}

	end := p.end
	dsg.Select = append(dsg.Select, &ast.TypeGuard{
		Ty:          d.QIdent,
		StartOffset: pos,
		EndOffset:   end,
	})

	dsg.EndOffset = end
	p.match(token.RPAREN)
	return true
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.TRUE, token.FALSE:

		lit = &ast.BasicLit{Kind: p.tok, Val: p.lexeme, StartOffset: p.pos, EndOffset: p.end}
		p.next()
	case token.LBRACE:
		pos := p.pos

		p.next()
		set := &ast.Set{}

		if p.exprStart() {
			set.Elem = append(set.Elem, p.parseSetElem())
			for p.tok == token.COMMA {
				p.next()
				set.Elem = append(set.Elem, p.parseSetElem())
			}
		}

		end := p.end
		p.match(token.RBRACE)

		set.StartOffset = pos
		set.EndOffset = end

		lit = set
	case token.NIL:
		lit = &ast.Nil{StartOffset: p.pos, EndOffset: p.end}
		p.next()
	default:
		p.errorExpected("literal or set")
		pos := p.pos
		p.advance(exprEnd)
		lit = &ast.BadExpr{StartOffset: pos, EndOffset: p.pos}
	}

	return
}

// set = '{' [ element {',' element} ] '}'
// element = expression ['..' expression]
func (p *Parser) parseSetElem() ast.Expression {
	beg := p.parseExpression()
	if p.tok != token.RANGE {
		return beg
	}

	p.next()
	end := p.parseExpression()

	return &ast.ExprRange{
		Low:         beg,
		High:        end,
		StartOffset: beg.Pos(),
		EndOffset:   end.End(),
	}
}

// qualident = [ ident '.' ] ident
func (p *Parser) parseQualifiedIdent() *ast.QualifiedIdent {
	id := &ast.QualifiedIdent{StartOffset: p.pos, EndOffset: p.end}

	// Parse the first identifier
	id.Prefix = p.parseIdent()

	if sym := p.ctx.Env.Lookup(id.Prefix); sym != nil && sym.Kind() == ast.ImportSymbolKind && p.tok == token.PERIOD {
		p.next()
		id.EndOffset = p.end // Update the end position
		id.Name = p.parseIdent()
	} else {
		id.Name, id.Prefix = id.Prefix, "" // Assign Name and clear Prefix
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
	start := p.pos
	end := p.end

	id := &ast.IdentifierDef{
		Name:        p.parseIdent(),
		StartOffset: start,
		EndOffset:   end,
		Props: func() ast.IdentProps {
			switch p.tok {
			case token.STAR:
				return ast.Exported
			case token.MINUS:
				return ast.ExportedReadOnly
			default:
				return ast.Unexported
			}
		}(),
	}

	if p.tok == token.STAR || p.tok == token.MINUS {
		p.next()
		id.EndOffset = p.end
	}

	return id
}

// ProcedureDeclaration = ProcedureHeading [ ';' ] ProcedureBody END [ ident ]
func (p *Parser) parseProcedureDecl() (proc *ast.ProcedureDecl) {
	proc = &ast.ProcedureDecl{StartOffset: p.pos}

	parent := p.ctx.Env
	p.ctx.Env = ast.NewEnvironment(parent, "")
	proc.Env = p.ctx.Env
	defer func() {
		p.ctx.Env = parent
	}()

	proc.Head = p.parseProcHeading()
	if p.tok == token.SEMICOLON {
		p.next()
	}

	proc.Body = p.parseProcBody()

	proc.EndOffset = p.end
	p.match(token.END)
	if p.tok == token.IDENTIFIER {
		proc.EndOffset = p.end
		proc.EndName = p.parseIdent()
	}

	proc.Env.SetName(proc.Head.Name.Name)
	return
}

// ProcedureHeading = ( PROCEDURE | PROC ) [Receiver] identdef [ FormalParameters ]
// Receiver = '(' [VAR|IN] ident ':' ident ')'
func (p *Parser) parseProcHeading() (head *ast.ProcedureHeading) {
	pos := p.pos
	head = &ast.ProcedureHeading{StartOffset: pos}

	if p.tok != token.PROC && p.tok != token.PROCEDURE {
		p.errorExpected("proc or procedure")
		p.advance(declStart)
		return head
	}

	p.next()

	if p.tok == token.LPAREN {
		head.Rcv = p.parseReceiver()
	}

	head.Name = p.parseIdentifierDef()
	head.EndOffset = head.Name.EndOffset

	if p.tok == token.LPAREN {
		head.FP = p.parseFormalParameters()
		if head.FP != nil {
			head.EndOffset = head.FP.EndOffset
		}
	}

	// check that head.Rcv.Type has been declared as a record type and retrieve its env
	// insert the receiver into the procedure's environment and the rest of the procedure's
	// parameters into the receiver's environment
	if head.Rcv != nil {
		rcvType := head.Rcv.Type.String()
		sym := p.ctx.Env.Lookup(rcvType)
		if sym == nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("reciever type '%s' not declared", rcvType),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
			p.advance(declStart)
			return head
		}

		typeSymbol, ok := sym.(*ast.TypeSymbol)
		if !ok || typeSymbol == nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("reciever type '%s' not declared as a type", rcvType),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
			p.advance(declStart)
			return head
		}

		var recordType *ast.RecordType
		switch t := typeSymbol.TypeNode().(type) {
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
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("receiver '%s' must be a record type", rcvType),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
			p.advance(declStart)
			return head
		}

		// add the receiver to the procedure's parent environment
		if sym := p.ctx.Env.Insert(ast.NewParamSymbol(head.Rcv.Var, head.Rcv.Mod, head.Rcv.Type)); sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate parameter declaration",
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
		}

		// add the procedure to the receiver's environment
		if sym := recordType.Env.Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, p.ctx.Env)); sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate procedure declaration",
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}
	} else {
		if sym := p.ctx.Env.Parent().Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, p.ctx.Env)); sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate procedure declaration: '%v'" + head.Name.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}
	}

	// add the formal parameters into the receiver's environment
	if head.FP != nil {
		for _, param := range head.FP.Params {
			for _, id := range param.Names {
				if sym := p.ctx.Env.Insert(ast.NewParamSymbol(id.Name, param.Mod, param.Type)); sym != nil {
					p.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  "duplicate parameter declaration" + id.Name,
						Range:    p.ctx.Source.Span(p.ctx.FileName, id.StartOffset, id.EndOffset),
					})
				}
			}
		}
	}

	return head
}

// FormalParameters = '(' [ FPSection { [';'] FPSection } ] ')' [ ':' ReturnType ]
func (p *Parser) parseFormalParameters() (fp *ast.FormalParams) {
	pos := p.pos
	fp = &ast.FormalParams{StartOffset: pos}

	p.match(token.LPAREN)

	if p.tok == token.VAR || p.tok == token.IN || p.tok == token.IDENTIFIER {
		fp.Params = append(fp.Params, p.parseFPSection())
		for p.tok == token.SEMICOLON {
			p.next()

			fp.Params = append(fp.Params, p.parseFPSection())
		}
	}

	fp.EndOffset = p.end
	p.match(token.RPAREN)

	if p.tok == token.COLON {
		p.next()
		fp.RetType = p.parseType()
		fp.EndOffset = fp.RetType.End()
	}

	return
}

// FPSection = [ VAR | IN ] ident { [','] ident } ':' FormalType
func (p *Parser) parseFPSection() (param *ast.FPSection) {
	pos := p.pos
	param = &ast.FPSection{StartOffset: pos}

	if p.tok == token.VAR || p.tok == token.IN {
		param.Mod = p.tok
		p.next()
	}

	param.Names = append(param.Names, p.parseIdentifierDef())
	for p.tok == token.COMMA {
		p.next()
		param.Names = append(param.Names, p.parseIdentifierDef())
	}

	p.match(token.COLON)

	param.Type = p.parseType()
	param.EndOffset = param.Type.End()

	return
}

func (p *Parser) parseReceiver() (rcv *ast.Receiver) {
	pos := p.pos
	rcv = &ast.Receiver{StartOffset: pos}

	p.match(token.LPAREN)
	if p.tok == token.VAR || p.tok == token.IN {
		rcv.Mod = p.tok
		p.next()
	}

	rcv.Var = p.parseIdent()
	p.match(token.COLON)
	q := p.parseQualifiedIdent()
	rcv.Type = ast.NewNamedType(q, q.StartOffset, q.EndOffset)

	rcv.EndOffset = p.end
	p.match(token.RPAREN)

	return
}

// ProcedureBody = DeclarationSequence [ BEGIN StatementSequence | ReturnStatement [ ';' ] ]
func (p *Parser) parseProcBody() (body *ast.ProcedureBody) {
	var (
		declSeq []ast.Declaration
		stmtSeq []ast.Statement
		bodyPos int
		bodyEnd int
	)

	declSeq = p.parseDeclarationSeq()

	if p.tok == token.BEGIN {
		p.next()
		stmtSeq = p.parseStatementSeq()

		if p.tok == token.SEMICOLON {
			p.next()
		}
	}

	if len(declSeq) > 0 && len(stmtSeq) == 0 {
		bodyPos = declSeq[0].Pos()
		bodyEnd = declSeq[len(declSeq)-1].End()
	} else if len(declSeq) == 0 && len(stmtSeq) > 0 {
		bodyPos = stmtSeq[0].Pos()
		bodyEnd = stmtSeq[len(stmtSeq)-1].End()
	} else if len(declSeq) > 0 && len(stmtSeq) > 0 {
		bodyPos = declSeq[0].Pos()
		bodyEnd = stmtSeq[len(stmtSeq)-1].End()
	} else {
		return
	}

	body = &ast.ProcedureBody{
		StartOffset: bodyPos,
		EndOffset:   bodyEnd,
		DeclSeq:     declSeq,
		StmtSeq:     stmtSeq,
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
		name = p.lexeme
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
		pos := p.pos
		dsg := p.parseDesignator()
		switch p.tok {
		case token.BECOMES:
			p.next()
			assign := &ast.AssignmentStmt{LValue: dsg, RValue: p.parseExpression(), StartOffset: pos}
			assign.EndOffset = assign.RValue.End()
			stmt = assign
		case token.RPAREN:
			stmt = &ast.ProcedureCall{Callee: dsg, ActualParams: p.list, StartOffset: pos, EndOffset: p.end}
			p.list = make([]ast.Expression, 0)
			p.next()
		default:
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("%s is not a valid statement", dsg),
				Range:    p.ctx.Source.Span(p.ctx.FileName, dsg.StartOffset, dsg.EndOffset),
			})
			p.advance(stmtStart)
			stmt = &ast.BadStmt{StartOffset: pos, EndOffset: p.pos}
		}
	default:
		p.errorExpected("statement")
		p.advance(stmtStart)
		stmt = &ast.BadStmt{StartOffset: p.pos, EndOffset: p.end}
	}

	return
}

func (p *Parser) parseLoopStmt() (stmt *ast.LoopStmt) {
	stmt = &ast.LoopStmt{StartOffset: p.pos}

	p.match(token.LOOP)
	stmt.StmtSeq = p.parseStatementSeq()

	stmt.EndOffset = p.end
	p.match(token.END)

	return
}

func (p *Parser) parseRepeatStmt() (stmt *ast.RepeatStmt) {
	stmt = &ast.RepeatStmt{StartOffset: p.pos}

	p.match(token.REPEAT)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.UNTIL)
	stmt.BoolExpr = p.parseExpression()

	stmt.EndOffset = stmt.BoolExpr.End()

	return
}

func (p *Parser) parseWhileStmt() (stmt *ast.WhileStmt) {
	stmt = &ast.WhileStmt{StartOffset: p.pos}

	p.match(token.WHILE)
	stmt.BoolExpr = p.parseExpression()
	p.match(token.DO)
	stmt.StmtSeq = p.parseStatementSeq()

	for p.tok == token.ELSIF {
		p.next()

		elsif := &ast.ElseIfBranch{BoolExpr: p.parseExpression()}
		p.match(token.DO)
		elsif.ThenPath = p.parseStatementSeq()

		stmt.ElsIfs = append(stmt.ElsIfs, elsif)
	}

	stmt.EndOffset = p.end
	p.match(token.END)

	return
}

func (p *Parser) parseReturnStmt() (stmt *ast.ReturnStmt) {
	stmt = &ast.ReturnStmt{StartOffset: p.pos, EndOffset: p.end}

	p.match(token.RETURN)
	if p.exprStart() {
		stmt.Value = p.parseExpression()
		stmt.EndOffset = stmt.Value.End()
	}

	return
}

// IfStatement = IF expression THEN StatementSequence {ElsifStatement} [ElseStatement] END
func (p *Parser) parseIfStmt() (stmt *ast.IfStmt) {
	stmt = &ast.IfStmt{StartOffset: p.pos}

	p.match(token.IF)
	stmt.BoolExpr = p.parseExpression()
	p.match(token.THEN)
	stmt.ThenPath = p.parseStatementSeq()

	for p.tok == token.ELSIF {
		p.next()

		elsif := &ast.ElseIfBranch{BoolExpr: p.parseExpression()}
		p.match(token.THEN)
		elsif.ThenPath = p.parseStatementSeq()

		stmt.ElseIfBranches = append(stmt.ElseIfBranches, elsif)
	}

	if p.tok == token.ELSE {
		p.next()
		stmt.ElsePath = p.parseStatementSeq()
	}

	stmt.EndOffset = p.end
	p.match(token.END)

	return
}

// CaseStatement = CASE expression OF ['|'] Case { '|' Case } [ ELSE StatementSequence ] END
func (p *Parser) parseCaseStmt() (stmt *ast.CaseStmt) {
	stmt = &ast.CaseStmt{StartOffset: p.pos}

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

	stmt.EndOffset = p.end
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

	if len(c.CaseLabelList) > 0 && len(c.StmtSeq) > 0 {
		c.StartOffset = c.CaseLabelList[0].Low.Pos()
		c.EndOffset = c.StmtSeq[len(c.StmtSeq)-1].End()
	} else if len(c.CaseLabelList) > 0 && len(c.StmtSeq) == 0 {
		c.StartOffset = c.CaseLabelList[0].Low.Pos()
		c.EndOffset = c.CaseLabelList[len(c.CaseLabelList)-1].High.End()
	}

	return c
}

func (p *Parser) parseLabelRange() *ast.LabelRange {
	pos := p.pos
	end := p.end
	r := &ast.LabelRange{StartOffset: pos, EndOffset: end, Low: p.parseExpression()}
	if p.tok == token.RANGE {
		p.next()
		r.High = p.parseExpression()
		r.EndOffset = r.High.End()
	}

	return r
}

// ForStatement = FOR ident ':=' expression TO expression [BY ConstExpression] DO StatementSequence END
func (p *Parser) parseForStmt() (stmt *ast.ForStmt) {
	stmt = &ast.ForStmt{StartOffset: p.pos}

	p.match(token.FOR)
	stmt.CtlVar = p.parseIdentifierDef()
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
	stmt.EndOffset = p.end
	p.match(token.END)

	return
}

// ExitStatement = EXIT
func (p *Parser) parseExitStmt() *ast.ExitStmt {
	stmt := &ast.ExitStmt{StartOffset: p.pos, EndOffset: p.end}
	p.match(token.EXIT)

	return stmt
}

// WithStatement = WITH ['|'] Guard DO StatementSequence { '|' Guard DO StatementSequence} [ ELSE StatementSequence ] END
// Guard         = qualident ':' qualident
func (p *Parser) parseWithStmt() *ast.WithStmt {
	stmt := &ast.WithStmt{StartOffset: p.pos}

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

	stmt.EndOffset = p.end
	p.match(token.END)

	return stmt
}

func (p *Parser) parseGuard() *ast.Guard {
	pos := p.pos
	grd := &ast.Guard{Expr: p.parseQualifiedIdent(), StartOffset: pos}
	p.match(token.COLON)
	grd.Type = p.parseQualifiedIdent()

	p.match(token.DO)
	grd.StmtSeq = p.parseStatementSeq()
	if len(grd.StmtSeq) > 0 {
		lastStmt := grd.StmtSeq[len(grd.StmtSeq)-1]
		grd.EndOffset = lastStmt.End()
	}

	return grd
}
