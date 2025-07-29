package mir

import (
	"fmt"
)

type Inst interface {
	inst()
	String() string
}

type (
	AssignInst struct {
		Target Operand
		Value  Operand
	}

	JumpInst struct {
		Target string
	}

	CondBrInst struct {
		Cond       Operand
		TrueLabel  string
		FalseLabel string
	}

	ReturnInst struct {
		Result Operand // nil if procedure returns nothing
	}
)

func (*AssignInst) inst() {}
func (*JumpInst) inst()   {}
func (*CondBrInst) inst() {}
func (*ReturnInst) inst() {}

func (i *AssignInst) String() string { return fmt.Sprintf("%v = %v", i.Target, i.Value) }
func (i *JumpInst) String() string   { return fmt.Sprintf("jmp %s", i.Target) }
func (i *CondBrInst) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", i.Cond, i.TrueLabel, i.FalseLabel)
}
func (i *ReturnInst) String() string {
	ret := "ret"
	if i.Result != nil {
		ret = fmt.Sprintf("ret %v", i.Result)
	}

	return ret
}
