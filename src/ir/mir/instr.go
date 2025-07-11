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
		Label string
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

	BinaryInst struct {
		Dst   Operand
		Op    string // "+", "-", "*", "/", etc.
		Left  Operand
		Right Operand
	}

	UnaryInst struct {
		Dst  Operand
		Op   string
		Expr Operand
	}

	FuncCallInst struct {
		Dst  Operand
		Func Operand
		Args []Operand
	}

	CmpInst struct {
		Dst  Operand
		Op   string
		X, Y Operand
	}
)

func (*AssignInst) isInstr()   {}
func (*ProcCallInst) isInstr() {}
func (*JumpInst) isInstr()     {}
func (*CondBrInst) isInstr()   {}
func (*LabelInst) isInstr()    {}
func (*ReturnInst) isInstr()   {}
func (*ExitInst) isInstr()     {}
func (*BinaryInst) isInstr()   {}
func (*UnaryInst) isInstr()    {}
func (*FuncCallInst) isInstr() {}
func (*CmpInst) isInstr()      {}

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
func (i *LabelInst) String() string { return i.Label }
func (i *ReturnInst) String() string {
	ret := "ret"
	if i.Result != nil {
		ret = fmt.Sprintf("ret %v", i.Result)
	}

	return ret
}
func (i *ExitInst) String() string { return "exit" }
func (v *BinaryInst) String() string {
	return fmt.Sprintf("%s = %s %s %s", v.Dst, v.Left, v.Op, v.Right)
}
func (v *UnaryInst) String() string { return fmt.Sprintf("%s = %s %s", v.Dst, v.Op, v.Expr) }
func (v *FuncCallInst) String() string {
	var args []string
	for _, op := range v.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("%s = call %s(%s)", v.Dst, v.Func, strings.Join(args, ", "))
}
func (i *CmpInst) String() string { return fmt.Sprintf("cmp %s %s, %s", i.Op, i.X, i.Y) }
