package mir

import (
	"fmt"
	"strings"
)

type Instr interface {
	isInstr()
	String() string
}

type (
	AssignInstr struct {
		Target Value
		Value  Value
	}

	JumpInstr struct {
		Target string
	}

	CondBrInstr struct {
		Cond       Value
		TrueLabel  string
		FalseLabel string
	}

	LabelInstr struct {
		Label string
	}

	ReturnInstr struct {
		Result Value // nil if procedure returns nothing
	}

	ExitInstr struct {
		LoopLabel LabelInstr
	}

	ProcCallInstr struct {
		Callee Value
		Args   []Value
	}
)

func (*AssignInstr) isInstr()   {}
func (*ProcCallInstr) isInstr() {}
func (*JumpInstr) isInstr()     {}
func (*CondBrInstr) isInstr()   {}
func (*LabelInstr) isInstr()    {}
func (*ReturnInstr) isInstr()   {}
func (*ExitInstr) isInstr()     {}

func (i *AssignInstr) String() string { return fmt.Sprintf("%v = %v", i.Target, i.Value) }
func (i *ProcCallInstr) String() string {
	var args []string
	for _, arg := range i.Args {
		args = append(args, arg.String())
	}
	return fmt.Sprintf("call %v(%s)", i.Callee, strings.Join(args, ","))
}
func (i *JumpInstr) String() string { return fmt.Sprintf("jmp %s", i.Target) }
func (i *CondBrInstr) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", i.Cond, i.TrueLabel, i.FalseLabel)
}
func (i *LabelInstr) String() string { return i.Label }
func (i *ReturnInstr) String() string {
	ret := "ret"
	if i.Result != nil {
		ret = fmt.Sprintf("ret %v", i.Result)
	}

	return ret
}
func (i *ExitInstr) String() string { return "exit" }
