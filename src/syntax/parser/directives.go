package parser

// directives.go — compile-time source directives (Section 13).
//
// Grammar:
//   Directive  = '<*' ( IfDir | ElsifDir | ElseDir | EndDir | AssertDir ) '*>'
//   IfDir      = 'IF'     ConstExpression 'THEN'
//   ElsifDir   = 'ELSIF'  ConstExpression 'THEN'
//   ElseDir    = 'ELSE'
//   EndDir     = 'END'
//
// Names in ConstExpression are resolved from ctx.Directives first; if the
// name is not found there, compile-time constants visible in the current
// lexical scope are checked as a fallback (Option B: CONST-backed conditions).
//
// Token-stream protocol
// ─────────────────────
// All helpers read via dNext / p.sc.NextToken() directly, bypassing next()
// so no diagnostics are emitted for dead-branch tokens.
//
// skipToNextBranch return conventions:
//   ELSIF — <* elsif consumed; *> NOT consumed; condition tokens follow.
//   ELSE  — <* else *>  fully consumed; body follows.
//   END   — <* end  *>  fully consumed; chain finished.
//   EOF   — error already reported.

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── raw-token helper ─────────────────────────────────────────────────────────

func (p *Parser) dNext() token.Token {
	for {
		t := p.sc.NextToken()
		if t.Kind == token.NEWLINE {
			continue
		}
		return t
	}
}

// consumeDirectiveEnd reads until DIRECTIVE_END (*>) or EOF.
// Call only when *> has NOT yet been read from the scanner.
func (p *Parser) consumeDirectiveEnd() {
	for {
		t := p.sc.NextToken()
		if t.Kind == token.DIRECTIVE_END || t.Kind == token.EOF {
			return
		}
	}
}

// ── branch skipping ──────────────────────────────────────────────────────────

// skipToNextBranch skips tokens until the next branch delimiter at depth 0.
// See file-level comment for return conventions.
func (p *Parser) skipToNextBranch() token.Kind {
	depth := 0
	for {
		t := p.sc.NextToken()
		if t.Kind == token.EOF {
			p.ctx.Reporter.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  "unterminated <* IF *> directive: reached EOF while skipping branch",
				Range:    p.ctx.Source.Span(p.fileName, t.Pos, t.End),
			})
			p.tok = token.EOF
			return token.EOF
		}
		if t.Kind != token.DIRECTIVE_START {
			continue
		}
		kw := p.sc.NextToken()
		switch kw.Kind {
		case token.IF:
			depth++
			p.consumeDirectiveEnd()
		case token.END:
			if depth == 0 {
				p.consumeDirectiveEnd()
				return token.END
			}
			depth--
			p.consumeDirectiveEnd()
		case token.ELSIF:
			if depth == 0 {
				return token.ELSIF // *> NOT consumed
			}
			p.consumeDirectiveEnd()
		case token.ELSE:
			if depth == 0 {
				p.consumeDirectiveEnd()
				return token.ELSE
			}
			p.consumeDirectiveEnd()
		default:
			p.consumeDirectiveEnd()
		}
	}
}

// skipAllRemainingArms skips until <* END *> at depth 0, consuming it.
// Used by handleClosingDirective to discard unused arms after a taken branch.
func (p *Parser) skipAllRemainingArms() {
	depth := 0
	for {
		t := p.sc.NextToken()
		if t.Kind == token.EOF {
			return
		}
		if t.Kind != token.DIRECTIVE_START {
			continue
		}
		kw := p.sc.NextToken()
		switch kw.Kind {
		case token.IF:
			depth++
			p.consumeDirectiveEnd()
		case token.END:
			if depth == 0 {
				p.consumeDirectiveEnd()
				return
			}
			depth--
			p.consumeDirectiveEnd()
		default:
			p.consumeDirectiveEnd()
		}
	}
}

// ── directive expression evaluator ───────────────────────────────────────────

type dirExprState struct {
	p   *Parser
	cur token.Token
}

func (s *dirExprState) advance() token.Token {
	s.cur = s.p.dNext()
	return s.cur
}

func newDirExprState(p *Parser) *dirExprState {
	s := &dirExprState{p: p}
	s.advance()
	return s
}

// resolveDirectiveIdent looks up name in ctx.Directives first.
// If not found there, it falls back to compile-time CONST symbols visible in
// the current lexical scope (Option B: CONST-backed directive conditions).
func (p *Parser) resolveDirectiveIdent(name string) (any, bool) {
	if v, ok := p.ctx.Directives[name]; ok {
		return v, true
	}
	// Option B: check the lexical environment for a compile-time constant.
	sym := p.ctx.Env.Lookup(name)
	if sym == nil || sym.Kind() != ast.ConstantSymbolKind {
		return nil, false
	}
	constSym := sym.(*ast.ConstantSymbol)
	return evalConstExprValue(constSym.Value)
}

// evalConstExprValue extracts a directive-evaluator-compatible primitive value
// (bool, int64, or float64) from a simple AST constant expression.
// Only BasicLit nodes are handled; complex expressions return (nil, false).
func evalConstExprValue(expr ast.Expression) (any, bool) {
	lit, ok := expr.(*ast.BasicLit)
	if !ok {
		return nil, false
	}
	switch lit.Kind {
	case token.TRUE:
		return true, true
	case token.FALSE:
		return false, true
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		v, err := strconv.ParseInt(lit.Val, 10, 64)
		if err != nil {
			return nil, false
		}
		return v, true
	case token.REAL_LIT, token.LONGREAL_LIT:
		v, err := strconv.ParseFloat(lit.Val, 64)
		if err != nil {
			return nil, false
		}
		return v, true
	}
	return nil, false
}

func evalDirExpr(s *dirExprState) (any, error) { return evalDirSimple(s) }

func evalDirSimple(s *dirExprState) (any, error) {
	sign := token.ILLEGAL
	if s.cur.Kind == token.PLUS || s.cur.Kind == token.MINUS {
		sign = s.cur.Kind
		s.advance()
	}
	lhs, err := evalDirTerm(s)
	if err != nil {
		return nil, err
	}
	if sign == token.MINUS {
		lhs = dirNegate(lhs)
	}
	for dirIsAddOp(s.cur.Kind) {
		op := s.cur.Kind
		s.advance()
		rhs, err := evalDirTerm(s)
		if err != nil {
			return nil, err
		}
		lhs = dirBinOp(op, lhs, rhs)
	}
	if dirIsRelOp(s.cur.Kind) {
		op := s.cur.Kind
		s.advance()
		rhs, err := evalDirSimple(s)
		if err != nil {
			return nil, err
		}
		lhs = dirBinOp(op, lhs, rhs)
	}
	return lhs, nil
}

func evalDirTerm(s *dirExprState) (any, error) {
	lhs, err := evalDirFactor(s)
	if err != nil {
		return nil, err
	}
	for dirIsMulOp(s.cur.Kind) {
		op := s.cur.Kind
		s.advance()
		rhs, err := evalDirFactor(s)
		if err != nil {
			return nil, err
		}
		lhs = dirBinOp(op, lhs, rhs)
	}
	return lhs, nil
}

func evalDirFactor(s *dirExprState) (any, error) {
	tok := s.cur
	switch tok.Kind {
	case token.NOT:
		s.advance()
		v, err := evalDirFactor(s)
		if err != nil {
			return nil, err
		}
		b, ok := v.(bool)
		if !ok {
			return nil, fmt.Errorf("'~' requires a boolean operand")
		}
		return !b, nil
	case token.LPAREN:
		s.advance()
		v, err := evalDirExpr(s)
		if err != nil {
			return nil, err
		}
		if s.cur.Kind != token.RPAREN {
			return nil, fmt.Errorf("expected ')' after expression, got %q", s.cur.Lexeme)
		}
		s.advance()
		return v, nil
	case token.TRUE:
		s.advance()
		return true, nil
	case token.FALSE:
		s.advance()
		return false, nil
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		v, err := strconv.ParseInt(tok.Lexeme, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("bad integer literal %q", tok.Lexeme)
		}
		s.advance()
		return v, nil
	case token.REAL_LIT, token.LONGREAL_LIT:
		v, err := strconv.ParseFloat(tok.Lexeme, 64)
		if err != nil {
			return nil, fmt.Errorf("bad real literal %q", tok.Lexeme)
		}
		s.advance()
		return v, nil
	case token.IDENTIFIER:
		name := tok.Lexeme
		s.advance()
		v, ok := s.p.resolveDirectiveIdent(name)
		if !ok {
			return nil, fmt.Errorf("undefined directive identifier %q", name)
		}
		return v, nil
	default:
		return nil, fmt.Errorf("unexpected token %q in directive expression", tok.Lexeme)
	}
}

func dirIsAddOp(k token.Kind) bool {
	return k == token.PLUS || k == token.MINUS || k == token.OR
}
func dirIsMulOp(k token.Kind) bool {
	return k == token.STAR || k == token.QUOT || k == token.DIV || k == token.MOD || k == token.AND
}
func dirIsRelOp(k token.Kind) bool {
	return k == token.EQUAL || k == token.NEQ ||
		k == token.LESS || k == token.LEQ ||
		k == token.GREAT || k == token.GEQ
}
func dirNegate(v any) any {
	switch x := v.(type) {
	case int64:
		return -x
	case float64:
		return -x
	}
	return v
}
func dirBinOp(op token.Kind, l, r any) any {
	switch lv := l.(type) {
	case int64:
		if rv, ok := r.(int64); ok {
			switch op {
			case token.PLUS:
				return lv + rv
			case token.MINUS:
				return lv - rv
			case token.STAR:
				return lv * rv
			case token.QUOT, token.DIV:
				if rv != 0 {
					return lv / rv
				}
			case token.MOD:
				if rv != 0 {
					return lv % rv
				}
			case token.EQUAL:
				return lv == rv
			case token.NEQ:
				return lv != rv
			case token.LESS:
				return lv < rv
			case token.LEQ:
				return lv <= rv
			case token.GREAT:
				return lv > rv
			case token.GEQ:
				return lv >= rv
			}
		}
	case float64:
		if rv, ok := r.(float64); ok {
			switch op {
			case token.PLUS:
				return lv + rv
			case token.MINUS:
				return lv - rv
			case token.STAR:
				return lv * rv
			case token.QUOT:
				if rv != 0 {
					return lv / rv
				}
			case token.EQUAL:
				return lv == rv
			case token.NEQ:
				return lv != rv
			case token.LESS:
				return lv < rv
			case token.LEQ:
				return lv <= rv
			case token.GREAT:
				return lv > rv
			case token.GEQ:
				return lv >= rv
			}
		}
	case bool:
		if rv, ok := r.(bool); ok {
			switch op {
			case token.AND:
				return lv && rv
			case token.OR:
				return lv || rv
			case token.EQUAL:
				return lv == rv
			case token.NEQ:
				return lv != rv
			}
		}
	}
	return nil
}

func dirIsTruthy(v any) (bool, error) {
	switch x := v.(type) {
	case bool:
		return x, nil
	case int64:
		return x != 0, nil
	case float64:
		return x != 0, nil
	}
	return false, fmt.Errorf("directive expression did not evaluate to a boolean value")
}

// ── top-level dispatcher ─────────────────────────────────────────────────────

func (p *Parser) parseDirective() {
	kw := p.dNext()
	switch kw.Kind {
	case token.IF:
		p.parseIfDirective()
	case token.ELSIF, token.ELSE, token.END:
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("unexpected directive keyword %q outside <* IF *>", strings.ToUpper(kw.Lexeme)),
			Range:    p.ctx.Source.Span(p.fileName, kw.Pos, kw.End),
		})
		p.consumeDirectiveEnd()
	default:
		// ASSERT is not a language keyword; the scanner emits it as IDENTIFIER.
		if kw.Kind == token.IDENTIFIER && strings.EqualFold(kw.Lexeme, "assert") {
			p.parseAssertDirective()
			return
		}
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("unknown directive keyword %q", kw.Lexeme),
			Range:    p.ctx.Source.Span(p.fileName, kw.Pos, kw.End),
		})
		p.consumeDirectiveEnd()
	}
}

// ── <* IF *> chain ───────────────────────────────────────────────────────────

// parseIfDirective: "if" consumed; *> not consumed; condition+THEN follow.
func (p *Parser) parseIfDirective() {
	s := newDirExprState(p)
	val, err := evalDirExpr(s)
	if err != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "directive IF: " + err.Error(),
			Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
		})
		p.consumeDirectiveEnd()
		return
	}
	if s.cur.Kind != token.THEN {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("directive IF: expected 'THEN', got %q", s.cur.Lexeme),
			Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
		})
		p.consumeDirectiveEnd()
		return
	}
	p.consumeDirectiveEnd() // consume THEN + *>

	cond, err := dirIsTruthy(val)
	if err != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "directive IF: " + err.Error(),
		})
	}

	if cond {
		p.directiveDepth++
		return
	}
	arm := p.skipToNextBranch()
	p.evaluateRemainingArms(arm)
}

// evaluateRemainingArms handles ELSIF/ELSE/END after a non-taken IF or ELSIF.
func (p *Parser) evaluateRemainingArms(firstArm token.Kind) {
	arm := firstArm
	for {
		switch arm {
		case token.ELSIF:
			// <* elsif consumed; *> NOT consumed; condition+THEN follow.
			s := newDirExprState(p)
			val, err := evalDirExpr(s)
			if err != nil {
				p.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  "directive ELSIF: " + err.Error(),
					Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
				})
				p.consumeDirectiveEnd()
				return
			}
			if s.cur.Kind != token.THEN {
				p.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  fmt.Sprintf("directive ELSIF: expected 'THEN', got %q", s.cur.Lexeme),
					Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
				})
				p.consumeDirectiveEnd()
				return
			}
			p.consumeDirectiveEnd() // consume THEN + *>
			cond, _ := dirIsTruthy(val)
			if cond {
				p.directiveDepth++
				return
			}
			arm = p.skipToNextBranch()

		case token.ELSE:
			// <* else *> fully consumed; body follows.
			p.directiveDepth++
			return

		default: // END or EOF
			return
		}
	}
}

// ── handleClosingDirective ────────────────────────────────────────────────────

// handleClosingDirective is called from next() when DIRECTIVE_START arrives
// and directiveDepth > 0.
func (p *Parser) handleClosingDirective() {
	kw := p.dNext()
	switch kw.Kind {
	case token.ELSIF:
		// Taken branch ended. Skip ELSIF condition+*> and remaining arms.
		p.consumeDirectiveEnd()  // skip condition…THEN *>
		p.skipAllRemainingArms() // skip body and <* END *>
		p.directiveDepth--

	case token.ELSE:
		// Taken branch ended at <* ELSE *>.
		p.consumeDirectiveEnd()  // consume *>
		p.skipAllRemainingArms() // skip ELSE body and <* END *>
		p.directiveDepth--

	case token.END:
		p.consumeDirectiveEnd() // consume *>
		p.directiveDepth--

	case token.IF:
		// Nested directive inside taken branch.
		p.parseIfDirective()

	default:
		// ASSERT is not a language keyword; the scanner emits it as IDENTIFIER.
		if kw.Kind == token.IDENTIFIER && strings.EqualFold(kw.Lexeme, "assert") {
			p.parseAssertDirective()
			return
		}
		p.consumeDirectiveEnd()
	}
}

// ── <* ASSERT expr, "msg" *> ─────────────────────────────────────────────────

// parseAssertDirective: "assert" consumed; *> not yet consumed.
// After consuming the message string via s.advance(), s.cur == DIRECTIVE_END.
// Do NOT call consumeDirectiveEnd() — the *> is already in s.cur.
func (p *Parser) parseAssertDirective() {
	errPos := p.pos
	s := newDirExprState(p)
	val, err := evalDirExpr(s)
	if err != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "directive ASSERT: " + err.Error(),
			Range:    p.ctx.Source.Span(p.fileName, errPos, s.cur.End),
		})
		for s.cur.Kind != token.DIRECTIVE_END && s.cur.Kind != token.EOF {
			s.advance()
		}
		return
	}

	if s.cur.Kind != token.COMMA {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("directive ASSERT: expected ',' after expression, got %q", s.cur.Lexeme),
			Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
		})
		for s.cur.Kind != token.DIRECTIVE_END && s.cur.Kind != token.EOF {
			s.advance()
		}
		return
	}
	s.advance() // consume comma; s.cur = string literal

	if s.cur.Kind != token.STR_LIT {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("directive ASSERT: expected string message after ',', got %q", s.cur.Lexeme),
			Range:    p.ctx.Source.Span(p.fileName, s.cur.Pos, s.cur.End),
		})
		for s.cur.Kind != token.DIRECTIVE_END && s.cur.Kind != token.EOF {
			s.advance()
		}
		return
	}
	msg := s.cur.Lexeme
	msgPos := s.cur.Pos
	msgEnd := s.cur.End
	s.advance() // consume string; s.cur should now be *> (DIRECTIVE_END)
	// *> is in s.cur — do NOT call consumeDirectiveEnd().

	cond, err2 := dirIsTruthy(val)
	if err2 != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "directive ASSERT: " + err2.Error(),
			Range:    p.ctx.Source.Span(p.fileName, errPos, msgEnd),
		})
		return
	}
	if !cond {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("compile-time assertion failed: %s", msg),
			Range:    p.ctx.Source.Span(p.fileName, msgPos, msgEnd),
		})
	}
}
