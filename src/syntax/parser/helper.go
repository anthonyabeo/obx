package parser

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// stmtStart lists tokens that genuinely begin a statement.
// Used only as a loop-continuation predicate in parseStatementSeq.
// Do NOT add block terminators (END, ELSE, ELSIF, UNTIL) here – they would
// cause the loop to re-enter and spin forever.
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

// stmtRecover is the sync set used by advance() in statement-level error
// recovery. It is stmtStart plus the tokens that legally follow a statement
// sequence so recovery never skips past an enclosing block terminator.
var stmtRecover = map[token.Kind]bool{
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
	token.END:        true,
	token.ELSE:       true,
	token.ELSIF:      true,
	token.UNTIL:      true,
	token.BAR:        true,
}

// declStart lists tokens that genuinely begin a declaration.
// Used only as a loop-continuation predicate in parseDeclarationSeq.
// Do NOT add BEGIN or END here – they would cause the loop to re-enter.
var declStart = map[token.Kind]bool{
	token.IMPORT:    true,
	token.CONST:     true,
	token.TYPE:      true,
	token.VAR:       true,
	token.PROC:      true,
	token.PROCEDURE: true,
}

// declRecover is the sync set used by advance() in declaration-level error
// recovery. It is declStart plus BEGIN and END so recovery halts before the
// module/procedure body rather than skipping past it.
var declRecover = map[token.Kind]bool{
	token.IMPORT:    true,
	token.CONST:     true,
	token.TYPE:      true,
	token.VAR:       true,
	token.PROC:      true,
	token.PROCEDURE: true,
	token.BEGIN:     true,
	token.END:       true,
}

// typeEnd is the sync set used when recovering inside a type expression.
// It is deliberately narrow so recovery stays within the current declaration
// instead of jumping all the way to the next CONST/VAR/PROC keyword.
var typeEnd = map[token.Kind]bool{
	token.SEMICOLON: true,
	token.END:       true,
	token.RPAREN:    true,
	token.RBRACK:    true,
	token.OF:        true,
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

func (p *Parser) copyParamsForProcedureType(FP *ast.FormalParams) *ast.FormalParams {
	FormalParams := &ast.FormalParams{Params: make([]*ast.FPSection, 0)}

	for _, param := range FP.Params {
		section := &ast.FPSection{Kind: param.Kind, Type: param.Type}
		for _, id := range param.Names {
			section.Names = append(section.Names, &ast.Identifier{
				Name:  "_",
				Props: id.Props,
			})
		}

		FormalParams.Params = append(FormalParams.Params, section)
	}
	FormalParams.RetType = FP.RetType

	return FormalParams
}

// pendingBinding holds everything needed to insert a type-bound procedure into
// its receiver record's method table once all module declarations are in scope.
type pendingBinding struct {
	head    *ast.ProcedureHeading
	sym     *ast.ProcedureSymbol
	procEnv *ast.LexicalScope // the procedure's own (now-popped) lexical scope
}

func (p *Parser) populateEnvs(head *ast.ProcedureHeading, kind ast.ProcedureKind) {
	procType := &ast.ProcedureType{FP: &ast.FormalParams{}}

	if head.FP != nil {
		procType.FP = p.copyParamsForProcedureType(head.FP)

		for _, param := range head.FP.Params {
			for _, id := range param.Names {
				if sym := p.ctx.Env.Define(ast.NewParamSymbol(id.Name, param.Kind, param.Type)); sym != nil {
					p.ctx.Reporter.Report(diag.Diagnostic{
						Severity: diag.Error,
						Message:  fmt.Sprintf("duplicate parameter declaration: '%s'", id.Name),
						Range:    p.ctx.Source.Span(p.ctx.FileName, id.StartOffset, id.EndOffset),
					})
				}
			}
		}
	}

	switch kind {
	case ast.ProperProcedureKind, ast.FunctionProcedureKind:
		curscope := p.ctx.Env.CurrentScope()
		sym := curscope.Parent().Insert(ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, procType, curscope, kind))
		if sym != nil {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("duplicate procedure declaration: '%s'", head.Name.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Name.StartOffset, head.Name.EndOffset),
			})
		}

	case ast.TypeBoundProcedureKind:
		procType.IsTypeBound = true
		curscope := p.ctx.Env.CurrentScope()

		proc := ast.NewProcedureSymbol(head.Name.Name, head.Name.Props, procType, curscope, kind)

		// Register the receiver as a parameter in the procedure scope.
		if sym := p.ctx.Env.Define(ast.NewParamSymbol(head.Rcv.Name.Name, head.Rcv.Kind, head.Rcv.Type)); sym != nil {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("duplicate parameter declaration for '%s'", head.Rcv.Name),
				Range:    p.ctx.Source.Span(p.ctx.FileName, head.Rcv.StartOffset, head.Rcv.EndOffset),
			})
		}

		// Defer the RecordScope binding: the receiver type may not be declared
		// yet because declarations can appear in any order.
		// The method symbol lives exclusively in RecordScope.methods — it is NOT
		// inserted into the flat module LexicalScope under a mangled name.
		p.pendingMethodBindings = append(p.pendingMethodBindings, pendingBinding{
			head:    head,
			sym:     proc,
			procEnv: curscope,
		})
	}
}

// bindTypeBoundMethod resolves the receiver type and inserts the method symbol
// into the record's dedicated method table.  Called from resolvePendingMethodBindings
// once all module declarations have been parsed.
func (p *Parser) bindTypeBoundMethod(b pendingBinding) bool {
	named, ok := b.head.Rcv.Type.(*ast.NamedType)
	if !ok {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver type must be a named type, got: '%v'", b.head.Rcv.Type),
			Range:    p.ctx.Source.Span(p.ctx.FileName, b.head.Rcv.StartOffset, b.head.Rcv.EndOffset),
		})
		return false
	}

	// At this call-site we are back at module scope (all procedure scopes popped).
	var searchScope *ast.LexicalScope
	if named.Name.Prefix != "" {
		searchScope = p.ctx.Env.ModuleScope(named.Name.Prefix)
		if searchScope == nil {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("cannot find module '%s'", named.Name.Prefix),
				Range:    p.ctx.Source.Span(p.ctx.FileName, named.Name.StartOffset, named.Name.EndOffset),
			})
			return false
		}
	} else {
		searchScope = p.ctx.Env.CurrentScope() // module scope
	}

	sym := searchScope.Lookup(named.Name.Name)
	if sym == nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver type '%s' not declared", named.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, named.Name.StartOffset, named.Name.EndOffset),
		})
		return false
	}
	typeSymbol, ok2 := sym.(*ast.TypeSymbol)
	if !ok2 {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver '%s' is not a type declaration", named.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, named.Name.StartOffset, named.Name.EndOffset),
		})
		return false
	}

	rcvType := typeSymbol.AstType()
	var rec *ast.RecordType
	switch b.head.Rcv.Kind {
	case token.VAR, token.IN:
		rec, ok = rcvType.(*ast.RecordType)
		if !ok {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  "VAR/IN receiver type must be a record type",
				Range:    p.ctx.Source.Span(p.ctx.FileName, rcvType.Pos(), rcvType.End()),
			})
			return false
		}
	default:
		rec, ok = p.isPointerToRecord(rcvType)
		if !ok {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  "value receiver type must be a pointer to record type",
				Range:    p.ctx.Source.Span(p.ctx.FileName, rcvType.Pos(), rcvType.End()),
			})
			return false
		}
	}

	// The method's logical parent is a named scope for the receiver type, so
	// Mangle() produces "Module$TypeName$MethodName" rather than the ambiguous
	// "Module$MethodName" that results from using the flat module scope directly.
	moduleScope := b.procEnv.Parent()
	typeScope := ast.NewLexicalScope(moduleScope, named.Name.Name)
	if dup := rec.Env.InsertMethod(b.sym, typeScope); dup != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("duplicate type-bound procedure '%s'", b.head.Name.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, b.head.Name.StartOffset, b.head.Name.EndOffset),
		})
	}
	return true
}

// resolvePendingMethodBindings must be called after the full declaration
// sequence of a module/definition has been parsed so that every receiver type
// is guaranteed to be in scope.
func (p *Parser) resolvePendingMethodBindings() {
	for _, b := range p.pendingMethodBindings {
		p.bindTypeBoundMethod(b)
	}
	p.pendingMethodBindings = nil
}

// resolveLocalPendingMethodBindings resolves the bindings that were accumulated
// starting at index `from` and trims them from the slice.  Call this from
// parseProcedureDecl, before the procedure scope is popped, so that types
// declared locally inside the procedure body (e.g. a record type that serves
// as a receiver for a nested type-bound proc) are still in scope.
func (p *Parser) resolveLocalPendingMethodBindings(from int) {
	for _, b := range p.pendingMethodBindings[from:] {
		p.bindTypeBoundMethod(b)
	}
	p.pendingMethodBindings = p.pendingMethodBindings[:from]
}

func (p *Parser) underlyingRcvType(ty ast.Type) ast.Type {
	var rcvType ast.Type

	Named, ok := ty.(*ast.NamedType)
	if !ok {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver type must be Named Type, got: '%v'", ty),
			Range:    p.ctx.Source.Span(p.ctx.FileName, ty.Pos(), ty.End()),
		})

		return rcvType
	}

	env := p.ctx.Env.CurrentScope().Parent()
	if Named.Name.Prefix != "" {
		env = p.ctx.Env.ModuleScope(Named.Name.Prefix)
	}

	sym := env.Lookup(Named.Name.Name)
	if sym == nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver type '%s' not declared", Named.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, Named.Name.StartOffset, Named.EndOffset),
		})

		return rcvType
	}

	typeSymbol, ok := sym.(*ast.TypeSymbol)
	if !ok || typeSymbol == nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("receiver type '%s' is not a type declaration", Named.Name),
			Range:    p.ctx.Source.Span(p.ctx.FileName, Named.Name.StartOffset, Named.EndOffset),
		})

		return rcvType
	}

	return sym.AstType()
}

func (p *Parser) isPointerToRecord(ty ast.Type) (*ast.RecordType, bool) {
	ptr, ok := ty.(*ast.PointerType)
	if !ok {
		return nil, false
	}

	base := ptr.Base
	for {
		switch t := base.(type) {
		case *ast.RecordType:
			return t, true
		case *ast.NamedType:
			sym := p.ctx.Env.Lookup(t.Name.Name)
			if sym == nil {
				return nil, false
			}

			base = sym.AstType()
		case *ast.PointerType:
			base = t.Base
		default:
			return nil, false
		}
	}
}

// isTypeGuardCall reports whether args applied to dsg constitutes a TypeGuard:
//   - exactly one argument
//   - the argument is a NamedType resolving to RecordType or PointerType{Base: RecordType}
//   - the root of the designator is a variable or parameter symbol
//
// Types can also appear as regular call arguments (e.g. NEW(T)), so this check
// additionally requires the designator root to be a variable/param — not a
// procedure — to distinguish them.
func (p *Parser) isTypeGuardCall(dsg *ast.Designator, args []ast.Expression) bool {
	if len(args) != 1 {
		return false
	}
	named, ok := args[0].(*ast.NamedType)
	if !ok {
		return false
	}

	env := p.ctx.Env.CurrentScope()
	if named.Name.Prefix != "" {
		env = p.ctx.Env.ModuleScope(named.Name.Prefix)
		if env == nil {
			return false
		}
	}
	typeSym := env.Lookup(named.Name.Name)
	if typeSym == nil || typeSym.Kind() != ast.TypeSymbolKind {
		return false
	}
	switch t := typeSym.(*ast.TypeSymbol).AstType().(type) {
	case *ast.RecordType:
		// ok
	case *ast.PointerType:
		if _, ok := t.Base.(*ast.RecordType); !ok {
			return false
		}
	default:
		return false
	}

	varSym := p.ctx.Env.Lookup(dsg.QIdent.Name)
	if varSym == nil {
		return false
	}
	return varSym.Kind() == ast.VariableSymbolKind || varSym.Kind() == ast.ParamSymbolKind
}

func (p *Parser) IsCallable(sym ast.Symbol) bool {
	if sym == nil {
		return false
	}

	switch sym.Kind() {
	case ast.ProcedureSymbolKind:
		return true
	case ast.VariableSymbolKind, ast.ConstantSymbolKind, ast.FieldSymbolKind, ast.ParamSymbolKind:
		typeNode := sym.AstType()
		for {
			switch ty := typeNode.(type) {
			case *ast.ProcedureType:
				return true
			case *ast.PointerType:
				typeNode = ty.Base
			case *ast.NamedType:
				sym = p.ctx.Env.Lookup(ty.Name.Name)
				if sym == nil {
					return false
				}
				typeNode = sym.AstType()
			default:
				return false
			}
		}
	default:
		return false
	}
}
