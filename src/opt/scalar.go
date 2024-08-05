package opt

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

// ConstantFolding
// --------------------------------------------------------------
func ConstantFolding(assign *tacil.Assign) {
	switch e := assign.Value.(type) {
	case *tacil.BinaryOp:
		left, xOk := e.X.(*tacil.ConstantInt)
		right, yOk := e.Y.(*tacil.ConstantInt)
		if xOk && yOk {
			assign.Value = performBinaryOp(e.Op, left, right)
		}
	default:

	}
}

func performBinaryOp(op tacil.Opcode, left, right *tacil.ConstantInt) tacil.Expr {
	var result uint64

	switch op {
	case tacil.Add:
		result = left.Value + right.Value
	case tacil.Sub:
		result = left.Value - right.Value
	case tacil.Mul:
		result = left.Value * right.Value
	case tacil.Div:
		if right.Value == 0 {
			panic(fmt.Sprintf("divide by zero error"))
		}

		result = left.Value / right.Value
	}

	return tacil.NewConstantInt(left.Ty, result, result < 0)
}

// AlgebraicSimplifyReAssociate
// --------------------------------------------------------------
func AlgebraicSimplifyReAssociate(assign *tacil.Assign) {
	switch e := assign.Value.(type) {
	case *tacil.BinaryOp:
		_, leftBool := e.X.Type().(tacil.Int1)
		_, rightBool := e.Y.Type().(tacil.Int1)

		if leftBool && rightBool {
			assign.Dst = simplifyBool(e)
		}

	}
}

func simplifyBool(e *tacil.BinaryOp) (r tacil.Expr) {
	left, leftBoolConst := e.X.(*tacil.ConstantBool)
	right, rightBoolConst := e.Y.(*tacil.ConstantBool)
	if leftBoolConst && rightBoolConst {
		switch e.Op {
		case tacil.Or:
			r = tacil.NewConstBool(left.Value | right.Value)
		case tacil.And:
			r = tacil.NewConstBool(left.Value & right.Value)
		}
	}

	if leftBoolConst && !rightBoolConst {
		switch e.Op {
		case tacil.Or:
			if left.Value == 1 {
				r = tacil.NewConstBool(1)
			} else {
				r = right
			}
		case tacil.And:
			if left.Value == 1 {
				r = right
			} else {
				r = tacil.NewConstBool(0)
			}
		}
	}

	if !leftBoolConst && rightBoolConst {
		switch e.Op {
		case tacil.Or:
			if right.Value == 1 {
				r = tacil.NewConstBool(1)
			} else {
				r = left
			}
		case tacil.And:
			if right.Value == 1 {
				r = left
			} else {
				r = tacil.NewConstBool(0)
			}
		}
	}

	return
}
