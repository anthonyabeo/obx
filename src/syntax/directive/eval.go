package directive

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func (es *EvalState) advance() token.Token {
	// skip NEWLINE tokens like the parser did
	for {
		t := es.Supplier.NextToken()
		if t.Kind == token.NEWLINE {
			continue
		}
		es.cur = t
		return t
	}
}

// EvalExpr evaluates a directive const-expression starting from the current
// token (es.cur already initialized) and returns the resulting value and the
// last token read (the token following the expression, e.g. THEN or COMMA).
func (es *EvalState) EvalExpr() (any, token.Token, error) {
	v, err := evalDirSimple(es)
	return v, es.cur, err
}

func evalDirSimple(s *EvalState) (any, error) {
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

func evalDirTerm(s *EvalState) (any, error) {
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

func evalDirFactor(s *EvalState) (any, error) {
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
		v, err := evalDirSimple(s)
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
		if s.Resolver == nil {
			return nil, fmt.Errorf("undefined directive identifier %q", name)
		}
		v, ok := s.Resolver(name)
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

// DirIsTruthy exported truthiness
func DirIsTruthy(v any) (bool, error) {
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

// EvalConstExprValue extracts primitive value from an AST BasicLit.
func EvalConstExprValue(expr ast.Expression) (any, bool) {
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
