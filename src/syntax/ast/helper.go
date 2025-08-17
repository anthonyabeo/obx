package ast

import (
	"strconv"

	"github.com/anthonyabeo/obx/src/syntax/token"
)

func IsConstExpr(expr Expression) bool {
	switch e := expr.(type) {
	case *BasicLit:
		return true

	case *Set:
		return true

	case *Identifier:
		if sym, ok := e.Symbol.(*ConstantSymbol); ok {
			return sym.Value != nil // Constant must have a known value
		}
		return false

	case *UnaryExpr:
		return IsConstExpr(e.Operand)

	case *BinaryExpr:
		return IsConstExpr(e.Left) && IsConstExpr(e.Right)

	case *Designator:
		if sym, ok := e.Symbol.(*ConstantSymbol); ok {
			return sym.Value != nil // Constant must have a known value
		}
		return false

	case *QualifiedIdent:
		if sym, ok := e.Symbol.(*ConstantSymbol); ok {
			return sym.Value != nil // Constant must have a known value
		}
		return false

	default:
		return false
	}
}

func IsZeroConstExpr(expr Expression) bool {
	switch e := expr.(type) {
	case *BasicLit:
		switch e.Kind {
		case token.INTEGER, token.INT64, token.INT32, token.INT8, token.INT16,
			token.SHORTINT, token.LONGINT, token.BYTE:
			return e.Val == "0"
		case token.REAL, token.LONGREAL:
			return e.Val == "0.0"
		case token.BOOLEAN:
			return e.Val == "false"
		case token.CHAR, token.WCHAR:
			return e.Val == ""
		}
		return false

	case *Identifier:
		if sym, ok := e.Symbol.(*ConstantSymbol); ok && sym.Value != nil {
			return isZeroValue(sym.Value)
		}
		return false

	case *UnaryExpr:
		if e.Op == token.MINUS {
			// e.g., -0 is still 0
			return IsZeroConstExpr(e.Operand)
		}
		return false

	case *BinaryExpr:
		// Optional: constant folding here
		val := EvalConstExpr(expr)
		return isZeroValue(val)

	default:
		return false
	}
}

func isZeroValue(v any) bool {
	switch val := v.(type) {
	case int, int64:
		return val == 0
	case float64:
		return val == 0.0
	case bool:
		return val == false
	default:
		return false
	}
}

func EvalConstExpr(expr Expression) any {
	switch e := expr.(type) {
	case *BasicLit:
		return evalBasicLiteral(e)
	case *UnaryExpr:
		return evalUnaryExpr(e)
	case *BinaryExpr:
		return evalBinaryExpr(e)
	case *Designator:
		if sym, ok := e.Symbol.(*ConstantSymbol); ok {
			return EvalConstExpr(sym.Value) // Return the constant value
		}
		return nil // Not a constant expression
	default:
		return nil // Not a constant expression
	}
}

func evalBasicLiteral(lit *BasicLit) any {
	switch lit.Kind {
	case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT64_LIT, token.INT32_LIT:
		val, _ := strconv.ParseInt(lit.Val, 10, 64)
		return val
	case token.REAL_LIT, token.LONGREAL_LIT:
		val, _ := strconv.ParseFloat(lit.Val, 64)
		return val
	case token.CHAR_LIT:
		return []rune(lit.Val)[0]
	case token.STR_LIT:
		if len(lit.Val) == 1 {
			return rune(lit.Val[0])
		}
		return lit.Val
	case token.TRUE:
		return lit.Val == "true"
	case token.FALSE:
		return lit.Val == "false"
	default:
		return nil
	}
}

func evalUnaryExpr(e *UnaryExpr) any {
	v := EvalConstExpr(e.Operand)
	if v == nil {
		return nil
	}

	switch e.Op {
	case token.MINUS:
		switch val := v.(type) {
		case int64:
			return -val
		case float64:
			return -val
		}
	case token.PLUS:
		return v // Unary + is a no-op
	case token.NOT:
		if b, ok := v.(bool); ok {
			return !b
		}
	}
	return nil
}

func evalBinaryExpr(e *BinaryExpr) any {
	lhs := EvalConstExpr(e.Left)
	rhs := EvalConstExpr(e.Right)
	if lhs == nil || rhs == nil {
		return nil
	}

	switch l := lhs.(type) {
	case int64:
		r, ok := rhs.(int64)
		if !ok {
			return nil
		}
		return evalIntOp(e.Op, l, r)
	case float64:
		r, ok := rhs.(float64)
		if !ok {
			return nil
		}
		return evalFloatOp(e.Op, l, r)
	case bool:
		r, ok := rhs.(bool)
		if !ok {
			return nil
		}
		return evalBoolOp(e.Op, l, r)
	case string:
		r, ok := rhs.(string)
		if !ok {
			return nil
		}
		if e.Op == token.PLUS {
			return l + r
		}
	}
	return nil
}

func evalIntOp(op token.Kind, a, b int64) any {
	switch op {
	case token.PLUS:
		return a + b
	case token.MINUS:
		return a - b
	case token.STAR:
		return a * b
	case token.QUOT:
		if b == 0 {
			return nil
		}
		return a / b
	case token.EQUAL:
		return a == b
	case token.NEQ:
		return a != b
	case token.LESS:
		return a < b
	case token.LEQ:
		return a <= b
	case token.GREAT:
		return a > b
	case token.GEQ:
		return a >= b
	}
	return nil
}

func evalFloatOp(op token.Kind, a, b float64) any {
	switch op {
	case token.PLUS:
		return a + b
	case token.MINUS:
		return a - b
	case token.STAR:
		return a * b
	case token.QUOT:
		if b == 0 {
			return nil
		}
		return a / b
	case token.EQUAL:
		return a == b
	case token.NEQ:
		return a != b
	case token.LESS:
		return a < b
	case token.LEQ:
		return a <= b
	case token.GREAT:
		return a > b
	case token.GEQ:
		return a >= b
	}
	return nil
}

func evalBoolOp(op token.Kind, a, b bool) any {
	switch op {
	case token.PLUS:
		return a && b
	case token.OR:
		return a || b
	case token.EQUAL:
		return a == b
	case token.NEQ:
		return a != b
	}
	return nil
}
