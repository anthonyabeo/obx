package tacil

import (
	"fmt"
	"strings"
)

// Assign
// --------------------------------
type Assign struct {
	Dst   Expr
	Value Expr
}

func CreateAssign(val, dst Expr) *Assign {
	return &Assign{
		Dst:   dst,
		Value: val,
	}
}

func (Assign) stmt()            {}
func (a Assign) String() string { return fmt.Sprintf("%s := %s", a.Dst, a.Value) }

// Return
// -----------------------------
type Return struct {
	Op    Opcode
	Value Expr
}

func CreateRet(v Expr) *Return {
	return &Return{
		Op:    Ret,
		Value: v,
	}
}

func (r Return) stmt()          {}
func (r Return) String() string { return fmt.Sprintf("ret %s", r.Value) }

// Jump
// --------------------------------
type Jump struct {
	Op  Opcode
	Dst *BasicBlock
}

func CreateJmp(dst *BasicBlock) *Jump {
	return &Jump{
		Op:  Jmp,
		Dst: dst,
	}
}

func (Jump) stmt()            {}
func (j Jump) String() string { return fmt.Sprintf("jmp label %%%s", j.Dst.name) }

// CondBr
// ----------
type CondBr struct {
	Op      Opcode
	Cond    Expr
	IfTrue  *BasicBlock
	IfFalse *BasicBlock
}

func CreateCondBr(cond Expr, ifThen, ifElse *BasicBlock) *CondBr {
	return &CondBr{
		Op:      Br,
		Cond:    cond,
		IfTrue:  ifThen,
		IfFalse: ifElse,
	}
}

func (CondBr) stmt() {}
func (c CondBr) String() string {
	return fmt.Sprintf("br %s, label %%%s, label %%%s", c.Cond.Name(), c.IfTrue.name, c.IfFalse.name)
}

// ProcCallInstr ...
// --------------------
type ProcCallInstr struct {
	Op     Opcode
	Callee Expr
	Args   []Expr
}

func CreateProcCall(callee Expr, args []Expr) *ProcCallInstr {
	return &ProcCallInstr{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (c ProcCallInstr) stmt() {}
func (c ProcCallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.Name())
	}

	return fmt.Sprintf("call %s(%s)", c.Callee.Name(), strings.Join(args, ", "))
}
