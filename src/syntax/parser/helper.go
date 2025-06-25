package parser

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

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

func (p *Parser) populateEnvs(head *ast.ProcedureHeading, kind ast.ProcedureKind) {
	procType := &ast.ProcedureType{FP: &ast.FormalParams{}}

	if head.FP != nil {
		procType.FP = head.FP

		for _, param := range head.FP.Params {
			for _, id := range param.Names {
				if sym := p.ctx.Env.Insert(ast.NewParamSymbol(id.Name, param.Kind, param.Type)); sym != nil {
					p.ctx.Reporter.Report(report.Diagnostic{
						Severity: report.Error,
						Message:  fmt.Sprintf("duplicate parameter declaration: '%s'", id.Name),
						Range:    p.ctx.Source.Span(p.ctx.FileName, id.StartOffset, id.EndOffset),
					})
				}
			}
		}
	}

	switch kind {
	case ast.ProperProcedureKind, ast.FunctionProcedureKind:
		sym := p.ctx.Env.Parent().Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, procType, p.ctx.Env, kind))
		if sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate procedure declaration: '%v'" + head.Name.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}
	case ast.TypeBoundProcedureKind:
		procType.IsTypeBound = true
		rcvType := p.underlyingRcvType(head.Rcv.Type)

		var (
			ok  bool
			rec *ast.RecordType
		)

		switch head.Rcv.Kind {
		case token.VAR, token.IN:
			rec, ok = rcvType.(*ast.RecordType)
			if !ok {
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "VAR/IN receiver type must be a record type",
					Range:    p.ctx.Source.Span(p.ctx.FileName, rcvType.Pos(), rcvType.End()),
				})

				return
			}
		default:
			rec, ok = p.isPointerToRecord(rcvType)
			if !ok {
				p.ctx.Reporter.Report(report.Diagnostic{
					Severity: report.Error,
					Message:  "value receiver type must be a pointer to record type",
					Range:    p.ctx.Source.Span(p.ctx.FileName, rcvType.Pos(), rcvType.End()),
				})

				return
			}
		}

		// add the procedure to the receiver's environment
		proc := ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, procType, p.ctx.Env, kind)
		if sym := rec.Env.Insert(proc); sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate procedure declaration for '%s'", head.Name.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}

		// add the receiver to the procedure's environment
		if sym := p.ctx.Env.Insert(ast.NewParamSymbol(head.Rcv.Name.Name, head.Rcv.Kind, head.Rcv.Type)); sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate parameter declaration for '%s'", head.Rcv.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
		}

		name := head.Rcv.Type.String() + "." + head.Name.Name
		sym := p.ctx.Env.Parent().Insert(ast.NewProcedureSymbol(name, head.Name.Props, procType, p.ctx.Env, kind))
		if sym != nil {
			p.ctx.Reporter.Report(report.Diagnostic{
				Severity: report.Error,
				Message:  fmt.Sprintf("duplicate type-bound procedure declaration for '%s'", head.Name.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}
	}
}

func (p *Parser) underlyingRcvType(ty ast.Type) ast.Type {
	var rcvType ast.Type

	Named, ok := ty.(*ast.NamedType)
	if !ok {
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("receiver type must be Named Type, got: '%v'", ty),
			Range:    p.ctx.Source.Span(p.ctx.FileName, ty.Pos(), ty.End()),
		})

		return rcvType
	}

	env := p.ctx.Env.Parent()
	if Named.Name.Prefix != "" {
		env = p.ctx.Envs[Named.Name.Prefix]
	}

	sym := env.Lookup(Named.Name.Name)
	if sym == nil {
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("receiver type '%s' not declared", Named.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, Named.Name.StartOffset, Named.EndOffset),
		})

		return rcvType
	}

	typeSymbol, ok := sym.(*ast.TypeSymbol)
	if !ok || typeSymbol == nil {
		p.ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  fmt.Sprintf("reciever type '%s' not declared as a type", rcvType),
			Range:    p.ctx.Source.Span(p.ctx.FileName, Named.Name.StartOffset, Named.EndOffset),
		})

		return rcvType
	}

	return sym.TypeNode()
}

func (p *Parser) isPointerToRecord(ty ast.Type) (*ast.RecordType, bool) {
	ptr, ok := ty.(*ast.PointerType)
	if !ok {
		return nil, false
	}

	rec, ok := ptr.Base.(*ast.RecordType)
	return rec, ok
}
