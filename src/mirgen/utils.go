package mirgen

import (
	"testing"

	"github.com/anthonyabeo/obx/src/meer"
)

func testLabel(t *testing.T, rcv, exp string) {
	if rcv != exp {
		t.Errorf("Expected Label with name '%s', got '%s' instead", exp, rcv)
	}
}

func testAssign(t *testing.T, inst *meer.AssignInst, expDst, expValue string) {
	if inst.Value.String() != expValue {
		t.Errorf("Expected value in assignment to be '%s', got '%s' instead",
			expValue, inst.Value.String())
	}

	if inst.Dst.String() != expDst {
		t.Errorf("Expected destination in assignment to be '%s', got '%s' instead",
			expDst, inst.Dst.String())
	}
}

func testCondBr(t *testing.T, inst *meer.CondBrInst, pred meer.Opcode, expTrueLabel, expFalseLabel, cond string) {
	if inst.Op != pred {
		t.Errorf("expected logical operator to be %s, Got %s", pred, inst.Op)
	}

	if inst.Cond.String() != cond {
		t.Errorf("expected logical condition to be '%s', Got '%s'", cond, inst.Cond.String())
	}

	testLabel(t, inst.IfTrue.String(), expTrueLabel)
	testLabel(t, inst.IfFalse.String(), expFalseLabel)
}

func testBinaryOp(t *testing.T, expr *meer.BinaryOp, left, right string, op meer.Opcode) {
	if expr.Op != op {
		t.Errorf("expected operator of binary operation to be '%s', Got '%s'", op, expr.Op)
	}

	if expr.X.String() != left {
		t.Errorf("expected LHS of binary operation to be '%s', Got '%s'", left, expr.X.String())
	}

	if expr.Y.String() != right {
		t.Errorf("expected RHS of binary operation to be '%s', Got '%s'", right, expr.Y.String())
	}
}
