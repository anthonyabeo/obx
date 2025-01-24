package meer

import (
	"fmt"
	"strings"
)

type Instruction interface {
	instr()
	OpCode() Opcode
	fmt.Stringer
}

// Label ...
// ------------------
type Label struct {
	Op      Opcode
	Name    string
	BlockID BasicBlockID
}

func NewLabel(name string) *Label {
	if name == "" {
		// TODO generate new name for label
	}

	return &Label{Op: Lbl, Name: name}
}

func (*Label) instr()           {}
func (l *Label) OpCode() Opcode { return l.Op }
func (l *Label) String() string { return fmt.Sprintf("%s", l.Name) }

// AssignInst
// --------------------------------
type AssignInst struct {
	Op    Opcode
	Dst   NamedOperand
	Value Expression
}

func CreateAssign(val Expression, dst NamedOperand) *AssignInst {
	return &AssignInst{
		Op:    Assign,
		Dst:   dst,
		Value: val,
	}
}

func (*AssignInst) instr()           {}
func (a *AssignInst) OpCode() Opcode { return a.Op }
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
func (r *ReturnInst) OpCode() Opcode { return r.Op }
func (r *ReturnInst) String() string { return fmt.Sprintf("ret %s", r.Value) }

// JumpInst
// --------------------------------
type JumpInst struct {
	Op  Opcode
	Dst *Label
}

func CreateJmp(dst *Label) *JumpInst {
	return &JumpInst{
		Op:  Jmp,
		Dst: dst,
	}
}

func (*JumpInst) instr()           {}
func (j *JumpInst) OpCode() Opcode { return j.Op }
func (j *JumpInst) String() string { return fmt.Sprintf("jmp label %%%s", j.Dst.Name) }

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

func (*CondBrInst) instr()           {}
func (c *CondBrInst) OpCode() Opcode { return c.Op }
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

func (*ProcCallInstr) instr()           {}
func (c *ProcCallInstr) OpCode() Opcode { return c.Op }
func (c *ProcCallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.String())
	}

	return fmt.Sprintf("call %s(%s)", c.Callee, strings.Join(args, ", "))
}
