package directive

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// HandleAssert evaluates an ASSERT directive using the EvalState. It reports
// diagnostics via rep. errPos is the parser position used in original code
// to build ranges; this helper does not construct ranges (caller may).
func HandleAssert(es *EvalState, rep diag.Reporter, errPos int) {
	val, cur, err := es.EvalExpr()
	if err != nil {
		if rep != nil {
			rep.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  "directive ASSERT: " + err.Error(),
			})
		}
		for cur.Kind != token.DIRECTIVE_END && cur.Kind != token.EOF {
			es.advance()
			cur = es.cur
		}
		return
	}

	if cur.Kind != token.COMMA {
		if rep != nil {
			rep.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("directive ASSERT: expected ',' after expression, got %q", cur.Lexeme),
			})
		}
		for cur.Kind != token.DIRECTIVE_END && cur.Kind != token.EOF {
			es.advance()
			cur = es.cur
		}
		return
	}
	es.advance() // consume comma; cur = string literal

	if es.cur.Kind != token.STR_LIT {
		if rep != nil {
			rep.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("directive ASSERT: expected string message after ',', got %q", es.cur.Lexeme),
			})
		}
		for es.cur.Kind != token.DIRECTIVE_END && es.cur.Kind != token.EOF {
			es.advance()
		}
		return
	}
	msg := es.cur.Lexeme
	// consume string; next should be DIRECTIVE_END
	es.advance()

	cond, err2 := DirIsTruthy(val)
	if err2 != nil {
		if rep != nil {
			rep.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  "directive ASSERT: " + err2.Error(),
			})
		}
		return
	}
	if !cond {
		if rep != nil {
			rep.Report(diag.Diagnostic{
				Severity: diag.Error,
				Message:  fmt.Sprintf("compile-time assertion failed: %s", msg),
			})
		}
	}
}
