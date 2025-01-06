package meer

import (
	"fmt"
	"strings"
)

type Instruction interface {
	instr()
	fmt.Stringer
}

// Label ...
// ------------------
type Label struct {
	Op   Opcode
	Name string
}

func NewLabel(name string) *Label {
	if name == "" {
		// TODO generate new name for label
	}

	return &Label{Op: Lbl, Name: name}
}

func (*Label) instr()           {}
func (l *Label) String() string { return fmt.Sprintf("%%%s", l.Name) }

// AssignInst
// --------------------------------
type AssignInst struct {
	Op    Opcode
	Dst   Expression
	Value Expression
}

func CreateAssign(val, dst Expression) *AssignInst {
	return &AssignInst{
		Op:    Assign,
		Dst:   dst,
		Value: val,
	}
}

func (*AssignInst) instr()           {}
func (a *AssignInst) String() string { return fmt.Sprintf("%s := %s", a.Dst, a.Value) }

// ReturnInst
// -----------------------------
type ReturnInst struct {
	Op    Opcode
	Value Expression
}

func CreateRet(v Expression) *ReturnInst {
	return &ReturnInst{
		Op:    Ret,
		Value: v,
	}
}

func (*ReturnInst) instr()           {}
func (r *ReturnInst) String() string { return fmt.Sprintf("ret %s", r.Value) }

// Jump
// --------------------------------
type Jump struct {
	Op  Opcode
	Dst *Label
}

func CreateJmp(dst *Label) *Jump {
	return &Jump{
		Op:  Jmp,
		Dst: dst,
	}
}

func (*Jump) instr()           {}
func (j *Jump) String() string { return fmt.Sprintf("jmp label %%%s", j.Dst.Name) }

// CondBrInst
// ----------
type CondBrInst struct {
	Op      Opcode
	Cond    Expression
	IfTrue  *Label
	IfFalse *Label
}

func CreateCondBrInst(cond Expression, ifThen, ifElse *Label) *CondBrInst {
	return &CondBrInst{
		Op:      Br,
		Cond:    cond,
		IfTrue:  ifThen,
		IfFalse: ifElse,
	}
}

func (*CondBrInst) instr() {}
func (c *CondBrInst) String() string {
	return fmt.Sprintf("br %s, label %s, label %s", c.Cond, c.IfTrue, c.IfFalse)
}

// ProcCallInstr ...
// --------------------
type ProcCallInstr struct {
	Op     Opcode
	Callee Expression
	Args   []Expression
}

func CreateProcCall(callee Expression, args []Expression) *ProcCallInstr {
	return &ProcCallInstr{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (*ProcCallInstr) instr() {}
func (c *ProcCallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", c.Callee, strings.Join(args, ", "))
}
