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
	"strings"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── raw-token helper ─────────────────────────────────────────────────────────

func (p *Parser) dNext() token.Token {
	return directive.DNext(p.sc)
}

// consumeDirectiveEnd reads until DIRECTIVE_END (*>) or EOF.
// Call only when *> has NOT yet been read from the scanner.
func (p *Parser) consumeDirectiveEnd() {
	directive.ConsumeDirectiveEnd(p.sc)
}

// ── branch skipping ──────────────────────────────────────────────────────────

// skipToNextBranch skips tokens until the next branch delimiter at depth 0.
// See file-level comment for return conventions.
func (p *Parser) skipToNextBranch() token.Kind {
	arm, t := directive.SkipToNextBranch(p.sc)
	if arm == token.EOF {
		// report with source span like previous behavior
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "unterminated <* IF *> directive: reached EOF while skipping branch",
			Range:    p.ctx.Source.Span(p.fileName, t.Pos, t.End),
		})
		p.tok = token.EOF
		return token.EOF
	}
	return arm
}

// skipAllRemainingArms skips until <* END *> at depth 0, consuming it.
// Used by handleClosingDirective to discard unused arms after a taken branch.
func (p *Parser) skipAllRemainingArms() {
	directive.SkipAllRemainingArms(p.sc)
}

// Directive expression evaluation and skipping are delegated to package
// src/syntax/directive. Parser keeps directiveDepth and diagnostics framing.

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
	resolver := directive.ResolverFromContext(p.ctx)
	es := directive.NewEvalState(p.sc, resolver)
	val, cur, err := es.EvalExpr()
	if err != nil {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  "directive IF: " + err.Error(),
			Range:    p.ctx.Source.Span(p.fileName, cur.Pos, cur.End),
		})
		directive.ConsumeDirectiveEnd(p.sc)
		return
	}
	if cur.Kind != token.THEN {
		p.ctx.Reporter.Report(diag.Diagnostic{
			Severity: diag.Error,
			Message:  fmt.Sprintf("directive IF: expected 'THEN', got %q", cur.Lexeme),
			Range:    p.ctx.Source.Span(p.fileName, cur.Pos, cur.End),
		})
		directive.ConsumeDirectiveEnd(p.sc)
		return
	}
	directive.ConsumeDirectiveEnd(p.sc) // consume THEN + *>

	cond, err := directive.DirIsTruthy(val)
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
			resolver := directive.ResolverFromContext(p.ctx)
			es := directive.NewEvalState(p.sc, resolver)
			val, cur, err := es.EvalExpr()
			if err != nil {
				p.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  "directive ELSIF: " + err.Error(),
					Range:    p.ctx.Source.Span(p.fileName, cur.Pos, cur.End),
				})
				directive.ConsumeDirectiveEnd(p.sc)
				return
			}
			if cur.Kind != token.THEN {
				p.ctx.Reporter.Report(diag.Diagnostic{
					Severity: diag.Error,
					Message:  fmt.Sprintf("directive ELSIF: expected 'THEN', got %q", cur.Lexeme),
					Range:    p.ctx.Source.Span(p.fileName, cur.Pos, cur.End),
				})
				directive.ConsumeDirectiveEnd(p.sc)
				return
			}
			directive.ConsumeDirectiveEnd(p.sc) // consume THEN + *>
			cond, _ := directive.DirIsTruthy(val)
			if cond {
				p.directiveDepth++
				return
			}
			arm, _ = directive.SkipToNextBranch(p.sc)

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
		directive.ConsumeDirectiveEnd(p.sc)  // skip condition…THEN *>
		directive.SkipAllRemainingArms(p.sc) // skip body and <* END *>
		p.directiveDepth--

	case token.ELSE:
		// Taken branch ended at <* ELSE *>.
		directive.ConsumeDirectiveEnd(p.sc)  // consume *>
		directive.SkipAllRemainingArms(p.sc) // skip ELSE body and <* END *>
		p.directiveDepth--

	case token.END:
		directive.ConsumeDirectiveEnd(p.sc) // consume *>
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
		directive.ConsumeDirectiveEnd(p.sc)
	}
}

// ── <* ASSERT expr, "msg" *> ─────────────────────────────────────────────────

// parseAssertDirective: "assert" consumed; *> not yet consumed.
// After consuming the message string via s.advance(), s.cur == DIRECTIVE_END.
// Do NOT call consumeDirectiveEnd() — the *> is already in s.cur.
func (p *Parser) parseAssertDirective() {
	es := directive.NewEvalState(p.sc, directive.ResolverFromContext(p.ctx))
	directive.HandleAssert(es, p.ctx.Reporter, p.pos)
}
