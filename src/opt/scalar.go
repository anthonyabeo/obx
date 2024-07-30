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

// AlgebraicSimReAssoc
// --------------------------------------------------------------
