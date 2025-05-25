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
	pos := p.rng.Start
	ms := &ast.MetaSection{Pos: pos}

	if p.tok == token.TYPE || p.tok == token.CONST {
		ms.Mode = p.tok
		p.next()
	}

	ms.Rng = &report.Range{Start: pos, End: p.rng.End}
	ms.Ids = append(ms.Ids, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
		if p.tok == token.COMMA {
			p.next()
		}

		ms.Rng.End = p.rng.End
		ms.Ids = append(ms.Ids, p.parseIdent())
	}

	if p.tok == token.COLON {
		p.next()
		q := p.parseQualifiedIdent()
		ms.TyConst = ast.NewNamedType(q, q.Pos, q.Rng)
		ms.Rng.End = q.Rng.End
	}

	return ms
}

func (p *Parser) parseModule() *ast.Module {
	pos := p.rng.Start
	mod := ast.NewModule(pos)

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
			pos := p.rng.Start
			p.errorExpected("import or declaration")
			p.advance(declStart)
			mod.DeclSeq = append(mod.DeclSeq, &ast.BadDecl{Pos: pos, Rng: &report.Range{
				Start: pos,
				End:   p.rng.Start}})
		}
	}

	if p.tok == token.BEGIN {
		p.next()
		mod.StmtSeq = p.parseStatementSeq()
	}

	p.match(token.END)
	mod.Rng = &report.Range{Start: pos, End: p.rng.End}
	mod.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		mod.Rng.End = p.rng.End
		p.next()
	}

	return mod
}

// definition   = DEFINITION ident [';']  [ ImportList ] DeclarationSequence2 END ident ['.']
func (p *Parser) parseDefinition() *ast.Definition {
	pos := p.rng.Start
	def := ast.NewDefinition(pos)

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
	def.Rng = &report.Range{Start: pos, End: p.rng.End}
	def.EName = p.parseIdent()
	if p.tok == token.PERIOD {
		def.Rng.End = p.rng.End
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
					Pos:      imp.Pos,
					Range:    imp.Rng,
				})
			}
		} else {
			if sym := p.env.Insert(ast.NewModuleSymbol(imp.Name)); sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate module import",
					Pos:      imp.Pos,
					Range:    imp.Rng,
				})
			}
		}
	}

	return list
}

// import = [ ident ':=' ] ImportPath ident [ MetaActuals ]
// ImportPath = { ident '.' }
func (p *Parser) parseImport() *ast.Import {
	pos := p.rng.Start
	imp := &ast.Import{Pos: pos}

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
	imp.Rng = &report.Range{Start: pos, End: p.rng.End}

	if p.tok == token.LPAREN {
		p.next()
		imp.Meta = append(imp.Meta, p.parseExpression())

		for p.tok == token.COMMA {
			p.next()
			imp.Meta = append(imp.Meta, p.parseExpression())
		}

		imp.Rng.End = p.rng.End
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
			pos := p.rng.Start
			p.match(token.VAR)
			for p.tok == token.IDENTIFIER {
				decl := p.parseVariableDecl()
				decl.Pos = pos
				decl.Rng.Start = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			pos := p.rng.Start
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				decl := p.parseTypeDecl()
				decl.Pos = pos
				decl.Rng.Start = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			pos := p.rng.Start
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				decl := p.parseConstantDecl()
				decl.Pos = pos
				decl.Rng.Start = pos

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
			pos := p.rng.Start
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{Pos: pos, Rng: &report.Range{
				Start: pos,
				End:   p.rng.Start,
			}})
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
			pos := p.rng.Start
			p.match(token.VAR)
			for p.tok == token.IDENTIFIER {
				decl := p.parseVariableDecl()
				decl.Pos = pos
				decl.Rng.Start = pos

				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.TYPE:
			pos := p.rng.Start
			p.match(token.TYPE)
			for p.tok == token.IDENTIFIER {
				decl := p.parseTypeDecl()
				decl.Pos = pos
				decl.Rng.Start = pos
				seq = append(seq, decl)
				if p.tok == token.SEMICOLON {
					p.next()
				}
			}
		case token.CONST:
			pos := p.rng.Start
			p.match(token.CONST)
			for p.tok == token.IDENTIFIER {
				decl := p.parseConstantDecl()
				decl.Pos = pos
				decl.Rng.Start = pos
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
			pos := p.rng.Start
			p.errorExpected("declaration")
			p.advance(declStart)
			seq = append(seq, &ast.BadDecl{Pos: pos, Rng: &report.Range{
				Start: pos,
				End:   p.rng.Start,
			}})
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
			Pos:      name.Pos,
			Range:    name.Rng,
		})
	}

	return &ast.TypeDecl{
		Name:        name,
		DenotedType: Typ,
		Rng:         &report.Range{End: Typ.Range().End}}
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
			Pos:      name.Pos,
			Range:    name.Range(),
		})
	}

	return &ast.ConstantDecl{
		Name:  name,
		Value: value,
		Rng:   &report.Range{End: value.Range().End}}
}

// VariableDeclaration = IdentList ':' type
func (p *Parser) parseVariableDecl() *ast.VariableDecl {
	decl := &ast.VariableDecl{IdentList: p.parseIdentList()}
	p.match(token.COLON)
	decl.Type = p.parseType()

	decl.Rng = &report.Range{End: decl.Type.Range().End}

	// add the variables to the environment
	for _, id := range decl.IdentList {
		sym := p.env.Insert(ast.NewVariableSymbol(id.Name, id.Props, decl.Type))
		if sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate variable declaration " + id.Name,
				Pos:      id.Pos,
				Range:    id.Rng,
			})
		}
	}

	return decl
}

func (p *Parser) parseType() (ty ast.Type) {
	switch p.tok {
	case token.INTEGER, token.BOOLEAN, token.CHAR, token.REAL, token.LONGREAL, token.INT8,
		token.INT16, token.INT32, token.INT64, token.WCHAR, token.BYTE, token.SHORTINT, token.LONGINT, token.SET:
		ty = ast.NewBasicType(p.lit, p.rng.Start, p.rng)
		p.next()
	case token.IDENTIFIER:
		q := p.parseQualifiedIdent()
		ty = ast.NewNamedType(q, q.Pos, q.Rng)
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
		pos := p.rng.Start
		p.errorExpected("type")
		p.advance(declStart)
		return &ast.BadType{
			pos,
			&report.Range{
				Start: pos,
				End:   p.rng.End,
			},
		}
	}

	return
}

func (p *Parser) parseEnumType() *ast.EnumType {
	pos := p.rng.Start
	enum := &ast.EnumType{Pos: pos}
	p.next()

	enum.Variants = append(enum.Variants, p.parseIdent())
	for p.tok == token.COMMA || p.tok == token.IDENTIFIER {
		if p.tok == token.COMMA {
			p.next()
		}
		enum.Variants = append(enum.Variants, p.parseIdent())
	}

	enum.Rng = &report.Range{Start: pos, End: p.rng.End}
	p.match(token.RPAREN)

	return enum
}

func (p *Parser) parsePtrType() *ast.PointerType {
	pos := p.rng.Start

	ptr := &ast.PointerType{Pos: pos}
	tok := p.tok

	p.next()
	if tok == token.POINTER {
		p.match(token.TO)
	}

	ptr.Base = p.parseType()
	ptr.Rng = &report.Range{Start: pos, End: ptr.Base.Range().End}

	return ptr
}

func (p *Parser) parseProcedureType() *ast.ProcedureType {
	pos := p.rng.Start
	proc := &ast.ProcedureType{Pos: pos, Rng: p.rng}
	p.match(token.PROCEDURE)

	if p.tok == token.LPAREN {
		proc.FP = p.parseFormalParameters()
		if proc.FP != nil && len(proc.FP.Params) != 0 {
			last := proc.FP.Params[len(proc.FP.Params)-1]

			if proc.FP.RetType != nil {
				proc.Rng.End = proc.FP.RetType.Range().End
			} else {
				proc.Rng.End = last.Rng.End
			}
		}
	}

	return proc
}

func (p *Parser) parseArrayType() *ast.ArrayType {
	pos := p.rng.Start
	array := &ast.ArrayType{Pos: pos}

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

	array.Rng = &report.Range{Start: pos, End: array.ElemType.Range().End}
	return array
}

func (p *Parser) parseLengthList() *ast.LenList {
	pos := p.rng.Start
	list := &ast.LenList{Modifier: token.ILLEGAL, Pos: pos}

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
	list.Rng = &report.Range{Start: pos, End: last.Range().End}

	return list
}

func (p *Parser) parseRecordType() *ast.RecordType {
	pos := p.rng.Start

	rec := &ast.RecordType{Pos: pos}
	p.match(token.RECORD)

	baseEnv := p.parseRecordBase(rec)
	rec.Env = ast.NewRecordEnv(baseEnv, p.env)

	if p.tok == token.IDENTIFIER {
		rec.Fields = p.parseRecordFields()
	}

	rec.Rng = &report.Range{Start: pos, End: p.rng.End}
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
	rec.Base = ast.NewNamedType(q, q.Pos, q.Rng)
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
				Pos:      baseType.Name.Pos,
				Range:    baseType.Name.Rng,
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
					Pos:      id.Pos,
					Range:    id.Rng,
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
		relExpr := &ast.BinaryExpr{Left: expr, Op: p.tok, Pos: expr.Position()}
		p.next()

		relExpr.Right = p.parseSimpleExpression()
		relExpr.Rng = &report.Range{
			Start: relExpr.Left.Range().Start,
			End:   relExpr.Right.Range().End,
		}
		expr = relExpr
	}

	return expr
}

// SimpleExpression = ['+' | '-'] term { AddOperator term }
func (p *Parser) parseSimpleExpression() (expr ast.Expression) {
	var sign = token.ILLEGAL

	pos := p.rng.Start
	if p.tok == token.MINUS || p.tok == token.PLUS {
		sign = p.tok
		p.next()
	}

	expr = p.parseTerm()
	if sign != token.ILLEGAL {
		expr = &ast.UnaryExpr{Op: sign, Operand: expr, Pos: pos, Rng: &report.Range{
			Start: pos,
			End:   expr.Range().End,
		}}
	}

	for p.addOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok, Pos: pos}
		p.next()

		bin.Right = p.parseTerm()
		bin.Rng = &report.Range{
			Start: pos,
			End:   bin.Right.Range().End,
		}
		expr = bin
	}

	return
}

// term = factor {MulOperator factor}
func (p *Parser) parseTerm() (expr ast.Expression) {
	expr = p.parseFactor()

	for p.mulOp() {
		bin := &ast.BinaryExpr{Left: expr, Op: p.tok, Pos: expr.Position()}
		p.next()

		bin.Right = p.parseFactor()
		bin.Rng = &report.Range{
			Start: bin.Left.Range().Start,
			End:   bin.Right.Range().End,
		}
		expr = bin
	}

	return
}

// factor = literal | designator [ActualParameters] | '(' expression ')' | '~' factor
func (p *Parser) parseFactor() ast.Expression {
	switch p.tok {
	case token.NOT:
		pos := p.rng.Start
		p.next()

		operand := p.parseFactor()
		return &ast.UnaryExpr{
			Op:      token.NOT,
			Operand: operand,
			Pos:     pos,
			Rng: &report.Range{
				Start: pos,
				End:   operand.Range().End,
			},
		}

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

		if sym := env.Lookup(dsg.QIdent.Name); sym != nil && sym.Kind() == ast.ProcedureSymbolKind && p.tok == token.RPAREN {
			call := &ast.FunctionCall{
				Callee:       dsg,
				ActualParams: p.list,
				Pos:          dsg.Pos,
				Rng: &report.Range{
					Start: dsg.Pos,
					End:   p.rng.End,
				},
			}
			p.list = nil
			p.next()
			return call
		}
		return dsg

	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.NIL, token.TRUE, token.FALSE, token.LBRACE:
		return p.parseLiteral()

	default:
		p.err.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  p.lit,
			Pos:      p.rng.Start,
			Range:    p.rng,
		})
		pos := p.rng.Start
		p.advance(exprEnd)
		return &ast.BadExpr{Pos: pos, Rng: p.rng}
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
	dsg := &ast.Designator{QIdent: qident, Pos: qident.Pos, Rng: qident.Rng}

	for p.tok == token.PERIOD || p.tok == token.LBRACK || p.tok == token.CARET || p.tok == token.LPAREN {
		switch p.tok {
		case token.PERIOD:
			pos := p.rng.Start
			p.next()

			dot := &ast.DotOp{Pos: pos, Rng: &report.Range{Start: pos, End: p.rng.End}}
			dot.Field = p.parseIdent()
			dsg.Select = append(dsg.Select, dot)
			dsg.Rng = &report.Range{Start: dsg.Pos, End: dot.Range().End}
		case token.LBRACK:
			pos := p.rng.Start
			p.next()
			list := p.parseExprList()
			end := p.rng.End
			p.match(token.RBRACK)
			dsg.Select = append(dsg.Select, &ast.IndexOp{
				List: list,
				Pos:  pos,
				Rng:  &report.Range{Start: pos, End: end}})

			dsg.Rng = &report.Range{Start: dsg.Pos, End: end}
		case token.CARET:
			pos := p.rng.Start
			p.next()
			dsg.Select = append(dsg.Select, &ast.PtrDeref{
				Pos: pos,
				Rng: &report.Range{Start: pos, End: p.rng.End}})
			dsg.Rng = &report.Range{Start: dsg.Pos, End: p.rng.End}
		case token.LPAREN:
			pos := p.rng.Start
			p.next()
			if !p.parseTypeGuard(dsg, pos) {
				return dsg
			}
		}
	}

	return dsg
}

func (p *Parser) parseTypeGuard(dsg *ast.Designator, pos *report.Position) bool {
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

	dsg.Select = append(dsg.Select, &ast.TypeGuard{
		Ty:  d.QIdent,
		Pos: pos,
		Rng: &report.Range{Start: pos, End: p.rng.End},
	})
	dsg.Rng = &report.Range{Start: dsg.Pos, End: p.rng.End}
	p.match(token.RPAREN)
	return true
}

// literal = number | string | hexstring | hexchar | NIL | TRUE | FALSE | set
func (p *Parser) parseLiteral() (lit ast.Expression) {
	switch p.tok {
	case token.INT_LIT, token.INT32_LIT, token.INT64_LIT, token.REAL_LIT, token.LONGREAL_LIT,
		token.STR_LIT, token.HEX_STR_LIT, token.CHAR_LIT, token.TRUE, token.FALSE:

		lit = &ast.BasicLit{Kind: p.tok, Val: p.lit, Pos: p.rng.Start, Rng: p.rng}
		p.next()
	case token.LBRACE:
		pos := p.rng.Start

		p.next()
		set := &ast.Set{}

		if p.exprStart() {
			set.Elem = append(set.Elem, p.parseSetElem())
			for p.tok == token.COMMA {
				p.next()
				set.Elem = append(set.Elem, p.parseSetElem())
			}
		}

		end := p.rng.End
		p.match(token.RBRACE)

		set.Pos = pos
		set.Rng = &report.Range{Start: pos, End: end}

		lit = set
	case token.NIL:
		lit = &ast.Nil{Pos: p.rng.Start, Rng: p.rng}
		p.next()
	default:
		p.errorExpected("literal or set")
		pos := p.rng.Start
		p.advance(exprStart)
		lit = &ast.BadExpr{Pos: pos, Rng: p.rng}
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
		Beg: beg,
		End: end,
		Pos: beg.Position(),
		Rng: &report.Range{Start: beg.Range().Start, End: end.Range().End},
	}
}

// qualident = [ ident '.' ] ident
func (p *Parser) parseQualifiedIdent() *ast.QualifiedIdent {
	id := &ast.QualifiedIdent{Pos: p.rng.Start, Rng: p.rng}

	// Parse the first identifier
	id.Prefix = p.parseIdent()

	if sym := p.env.Lookup(id.Prefix); sym != nil && sym.Kind() == ast.ModuleSymbolKind && p.tok == token.PERIOD {
		p.next()
		id.Rng.End = p.rng.End // Update the end position
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
	r := &report.Range{Start: p.rng.Start, End: p.rng.End}

	id := &ast.IdentifierDef{
		Name: p.parseIdent(),
		Pos:  r.Start,
		Rng:  r,
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
		r.End = p.rng.End
		p.next()
	}

	return id
}

// ProcedureDeclaration = ProcedureHeading [ ';' ] ProcedureBody END [ ident ]
func (p *Parser) parseProcedureDecl() (proc *ast.ProcedureDecl) {
	pos := p.rng.Start
	proc = &ast.ProcedureDecl{Pos: pos}

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

	proc.Rng = &report.Range{Start: pos, End: p.rng.End}
	p.match(token.END)
	if p.tok == token.IDENTIFIER {
		proc.Rng.End = p.rng.End
		proc.EndName = p.parseIdent()
	}

	return
}

// ProcedureHeading = ( PROCEDURE | PROC ) [Receiver] identdef [ FormalParameters ]
// Receiver = '(' [VAR|IN] ident ':' ident ')'
func (p *Parser) parseProcHeading() (head *ast.ProcedureHeading) {
	pos := p.rng.Start
	head = &ast.ProcedureHeading{Pos: pos}

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
	head.Rng = &report.Range{Start: pos, End: head.Name.Rng.End}

	if p.tok == token.LPAREN {
		head.FP = p.parseFormalParameters()
		if head.FP != nil {
			head.Rng.End = head.FP.Rng.End
		}
	}

	// check that head.Rcv.Type has been declared as a record type and retrieve its env
	// insert the receiver into the procedure's environment and the rest of the procedure's
	// parameters into the receiver's environment
	if head.Rcv != nil {
		rcvType := head.Rcv.Type.String()
		sym := p.env.Lookup(rcvType)
		if sym == nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("reciever type '%s' not declared", rcvType),
				Pos:      head.Rcv.Pos,
				Range:    head.Rcv.Rng,
			})
			p.advance(declStart)
			return head
		}

		typeSymbol, ok := sym.(*ast.TypeSymbol)
		if !ok || typeSymbol == nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("reciever type '%s' not declared as a type", rcvType),
				Pos:      head.Rcv.Pos,
				Range:    head.Rcv.Rng,
			})
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
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("receiver '%s' must be a record type", rcvType),
				Pos:      head.Rcv.Pos,
				Range:    head.Rcv.Rng,
			})
			p.advance(declStart)
			return head
		}

		// add the receiver to the procedure's parent environment
		if sym := p.env.Insert(ast.NewParamSymbol(head.Rcv.Var, head.Rcv.Mod, head.Rcv.Type)); sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate parameter declaration",
				Pos:      head.Rcv.Pos,
				Range:    head.Rcv.Rng,
			})
		}

		// add the procedure to the receiver's environment
		if sym := recordType.Env.Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props)); sym != nil {
			p.err.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  "duplicate procedure declaration",
				Pos:      head.Name.Pos,
				Range:    head.Name.Rng,
			})
		}
	}

	// add the formal parameters into the receiver's environment
	for _, param := range head.FP.Params {
		for _, id := range param.Names {
			if sym := p.env.Insert(ast.NewParamSymbol(id, param.Mod, param.Type)); sym != nil {
				p.err.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "duplicate parameter declaration" + id,
					Pos:      param.Pos,
					Range:    param.Rng,
				})
			}
		}
	}

	return head
}

// FormalParameters = '(' [ FPSection { [';'] FPSection } ] ')' [ ':' ReturnType ]
func (p *Parser) parseFormalParameters() (fp *ast.FormalParams) {
	pos := p.rng.Start
	fp = &ast.FormalParams{Pos: pos}

	p.match(token.LPAREN)

	if p.tok == token.VAR || p.tok == token.IN || p.tok == token.IDENTIFIER {
		fp.Params = append(fp.Params, p.parseFPSection())
		for p.tok == token.SEMICOLON {
			p.next()

			fp.Params = append(fp.Params, p.parseFPSection())
		}
	}

	fp.Rng = &report.Range{Start: pos, End: p.rng.End}
	p.match(token.RPAREN)

	if p.tok == token.COLON {
		p.next()
		fp.RetType = p.parseType()
		fp.Rng = &report.Range{Start: pos, End: fp.RetType.Range().End}
	}

	return
}

// FPSection = [ VAR | IN ] ident { [','] ident } ':' FormalType
func (p *Parser) parseFPSection() (param *ast.FPSection) {
	pos := p.rng.Start
	param = &ast.FPSection{Pos: pos}

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
	param.Rng = &report.Range{Start: pos, End: param.Type.Range().End}

	return
}

func (p *Parser) parseReceiver() (rcv *ast.Receiver) {
	pos := p.rng.Start
	rcv = &ast.Receiver{Pos: pos}

	p.match(token.LPAREN)
	if p.tok == token.VAR || p.tok == token.IN {
		rcv.Mod = p.tok
		p.next()
	}

	rcv.Var = p.parseIdent()
	p.match(token.COLON)
	q := p.parseQualifiedIdent()
	rcv.Type = ast.NewNamedType(q, q.Pos, q.Rng)

	rcv.Rng = &report.Range{Start: pos, End: p.rng.End}
	p.match(token.RPAREN)

	return
}

// ProcedureBody = DeclarationSequence [ BEGIN StatementSequence | ReturnStatement [ ';' ] ]
func (p *Parser) parseProcBody() (body *ast.ProcedureBody) {
	var (
		declSeq   []ast.Declaration
		stmtSeq   []ast.Statement
		bodyPos   *report.Position
		bodyRange *report.Range
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
		bodyPos = declSeq[0].Position()
		bodyRange = &report.Range{
			Start: bodyPos,
			End:   declSeq[len(declSeq)-1].Range().End,
		}
	} else if len(declSeq) == 0 && len(stmtSeq) > 0 {
		bodyPos = stmtSeq[0].Position()
		bodyRange = &report.Range{
			Start: bodyPos,
			End:   stmtSeq[len(stmtSeq)-1].Range().End,
		}
	} else if len(declSeq) > 0 && len(stmtSeq) > 0 {
		bodyPos = declSeq[0].Position()
		bodyRange = &report.Range{
			Start: bodyPos,
			End:   stmtSeq[len(stmtSeq)-1].Range().End,
		}
	} else {
		return
	}

	body = &ast.ProcedureBody{
		Pos:     bodyPos,
		Rng:     bodyRange,
		DeclSeq: declSeq,
		StmtSeq: stmtSeq,
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
		pos := p.rng.Start
		dsg := p.parseDesignator()
		switch p.tok {
		case token.BECOMES:
			p.next()
			assign := &ast.AssignmentStmt{LValue: dsg, RValue: p.parseExpression(), Pos: pos}
			assign.Rng = &report.Range{Start: pos, End: assign.RValue.Range().End}
			stmt = assign
		case token.RPAREN:
			stmt = &ast.ProcedureCall{
				Callee:       dsg,
				ActualParams: p.list,
				Pos:          pos,
				Rng:          &report.Range{Start: pos, End: p.rng.End},
			}
			p.list = make([]ast.Expression, 0)
			p.next()
		default:
			stmt = &ast.BadStmt{Pos: pos, Rng: p.rng}
			p.errorExpected(":= or (")
			p.advance(stmtStart)
		}
	default:
		p.errorExpected("statement")
		p.advance(stmtStart)
		stmt = &ast.BadStmt{Pos: p.rng.Start, Rng: p.rng}
	}

	return
}

func (p *Parser) parseLoopStmt() (stmt *ast.LoopStmt) {
	stmt = &ast.LoopStmt{Pos: p.rng.Start}

	p.match(token.LOOP)
	stmt.StmtSeq = p.parseStatementSeq()

	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
	p.match(token.END)

	return
}

func (p *Parser) parseRepeatStmt() (stmt *ast.RepeatStmt) {
	stmt = &ast.RepeatStmt{Pos: p.rng.Start}

	p.match(token.REPEAT)
	stmt.StmtSeq = p.parseStatementSeq()
	p.match(token.UNTIL)
	stmt.BoolExpr = p.parseExpression()

	stmt.Rng = &report.Range{Start: stmt.Pos, End: stmt.BoolExpr.Range().End}

	return
}

func (p *Parser) parseWhileStmt() (stmt *ast.WhileStmt) {
	stmt = &ast.WhileStmt{Pos: p.rng.Start}

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

	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
	p.match(token.END)

	return
}

func (p *Parser) parseReturnStmt() (stmt *ast.ReturnStmt) {
	stmt = &ast.ReturnStmt{Pos: p.rng.Start, Rng: p.rng}

	p.match(token.RETURN)
	if p.exprStart() {
		stmt.Value = p.parseExpression()
		stmt.Rng.End = stmt.Value.Range().End
	}

	return
}

// IfStatement = IF expression THEN StatementSequence {ElsifStatement} [ElseStatement] END
func (p *Parser) parseIfStmt() (stmt *ast.IfStmt) {
	stmt = &ast.IfStmt{Pos: p.rng.Start}

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

	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
	p.match(token.END)

	return
}

// CaseStatement = CASE expression OF ['|'] Case { '|' Case } [ ELSE StatementSequence ] END
func (p *Parser) parseCaseStmt() (stmt *ast.CaseStmt) {
	stmt = &ast.CaseStmt{Pos: p.rng.Start}

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

	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
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

	if len(c.CaseLabelList) != 0 && len(c.StmtSeq) != 0 {
		c.Pos = c.CaseLabelList[0].Begin.Position()

		lastStmt := c.StmtSeq[len(c.StmtSeq)-1]
		c.Rng = &report.Range{Start: c.Pos, End: lastStmt.Range().End}
	}

	return c
}

func (p *Parser) parseLabelRange() *ast.LabelRange {
	pos := p.rng.Start
	rng := p.rng
	r := &ast.LabelRange{Pos: pos, Rng: rng, Begin: p.parseExpression()}
	if p.tok == token.RANGE {
		p.next()
		r.End = p.parseExpression()
		r.Rng.End = r.End.Range().End
	}

	return r
}

// ForStatement = FOR ident ':=' expression TO expression [BY ConstExpression] DO StatementSequence END
func (p *Parser) parseForStmt() (stmt *ast.ForStmt) {
	stmt = &ast.ForStmt{Pos: p.rng.Start}

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
	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
	p.match(token.END)

	return
}

// ExitStatement = EXIT
func (p *Parser) parseExitStmt() *ast.ExitStmt {
	stmt := &ast.ExitStmt{Pos: p.rng.Start, Rng: p.rng}
	p.match(token.EXIT)

	return stmt
}

// WithStatement = WITH ['|'] Guard DO StatementSequence { '|' Guard DO StatementSequence} [ ELSE StatementSequence ] END
// Guard         = qualident ':' qualident
func (p *Parser) parseWithStmt() *ast.WithStmt {
	stmt := &ast.WithStmt{Pos: p.rng.Start}

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

	stmt.Rng = &report.Range{Start: stmt.Pos, End: p.rng.End}
	p.match(token.END)

	return stmt
}

func (p *Parser) parseGuard() *ast.Guard {
	pos := p.rng.Start
	grd := &ast.Guard{Expr: p.parseQualifiedIdent(), Pos: pos}
	p.match(token.COLON)
	grd.Type = p.parseQualifiedIdent()

	p.match(token.DO)
	grd.StmtSeq = p.parseStatementSeq()
	if len(grd.StmtSeq) != 0 {
		lastStmt := grd.StmtSeq[len(grd.StmtSeq)-1]
		grd.Rng = &report.Range{Start: grd.Pos, End: lastStmt.Range().End}
	}

	return grd
}
