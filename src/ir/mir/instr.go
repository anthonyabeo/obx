package mir

import (
	"fmt"
	"strings"
)

type Inst interface {
	isInstr()
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

	LabelInst struct {
		Name Label
	}

	ReturnInst struct {
		Result Operand // nil if procedure returns nothing
	}

	ExitInst struct {
		LoopLabel LabelInst
	}

	ProcCallInst struct {
		Callee Operand
		Args   []Operand
	}
)

func (*AssignInst) isInstr()   {}
func (*ProcCallInst) isInstr() {}
func (*JumpInst) isInstr()     {}
func (*CondBrInst) isInstr()   {}
func (*LabelInst) isInstr()    {}
func (*ReturnInst) isInstr()   {}
func (*ExitInst) isInstr()     {}

func (i *AssignInst) String() string { return fmt.Sprintf("%v = %v", i.Target, i.Value) }
func (i *ProcCallInst) String() string {
	var args []string
	for _, arg := range i.Args {
		args = append(args, arg.String())
	}
	return fmt.Sprintf("call %v(%s)", i.Callee, strings.Join(args, ","))
}
func (i *JumpInst) String() string { return fmt.Sprintf("jmp %s", i.Target) }
func (i *CondBrInst) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", i.Cond, i.TrueLabel, i.FalseLabel)
}
func (i *LabelInst) String() string { return fmt.Sprintf("%v:", i.Name) }
func (i *ReturnInst) String() string {
	ret := "ret"
	if i.Result != nil {
		ret = fmt.Sprintf("ret %v", i.Result)
	}

	return ret
}
func (i *ExitInst) String() string { return "exit" }
