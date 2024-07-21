package tacil

import (
	"fmt"
	"strings"
)

// Assign
// --------------------------------
type Assign struct {
	Dst    Expr
	Value  Expr
	parent *BasicBlock
}

func CreateAssign(val, dst Expr) *Assign {
	return &Assign{
		Dst:   dst,
		Value: val,
	}
}

func (*Assign) stmt()                          {}
func (a *Assign) Parent() *BasicBlock          { return a.parent }
func (a *Assign) SetParent(parent *BasicBlock) { a.parent = parent }
func (a *Assign) String() string               { return fmt.Sprintf("%s := %s", a.Dst, a.Value) }

// Return
// -----------------------------
type Return struct {
	Op    Opcode
	Value Expr

	parent *BasicBlock
}

func CreateRet(v Expr) *Return {
	return &Return{
		Op:    Ret,
		Value: v,
	}
}

func (*Return) stmt()                          {}
func (r *Return) Parent() *BasicBlock          { return r.parent }
func (r *Return) SetParent(parent *BasicBlock) { r.parent = parent }
func (r *Return) String() string               { return fmt.Sprintf("ret %s", r.Value) }

// Jump
// --------------------------------
type Jump struct {
	Op  Opcode
	Dst *BasicBlock

	parent *BasicBlock
}

func CreateJmp(dst *BasicBlock) *Jump {
	return &Jump{
		Op:  Jmp,
		Dst: dst,
	}
}

func (*Jump) stmt()                          {}
func (j *Jump) Parent() *BasicBlock          { return j.parent }
func (j *Jump) SetParent(parent *BasicBlock) { j.parent = parent }
func (j *Jump) String() string               { return fmt.Sprintf("jmp label %%%s", j.Dst.name) }

// CondBr
// ----------
type CondBr struct {
	Op      Opcode
	Cond    Expr
	IfTrue  *BasicBlock
	IfFalse *BasicBlock

	parent *BasicBlock
}

func CreateCondBr(cond Expr, ifThen, ifElse *BasicBlock) *CondBr {
	return &CondBr{
		Op:      Br,
		Cond:    cond,
		IfTrue:  ifThen,
		IfFalse: ifElse,
	}
}

func (*CondBr) stmt()                          {}
func (c *CondBr) Parent() *BasicBlock          { return c.parent }
func (c *CondBr) SetParent(parent *BasicBlock) { c.parent = parent }
func (c *CondBr) String() string {
	return fmt.Sprintf("br %s, label %%%s, label %%%s", c.Cond.Name(), c.IfTrue.name, c.IfFalse.name)
}

// ProcCallInstr ...
// --------------------
type ProcCallInstr struct {
	Op     Opcode
	Callee Expr
	Args   []Expr

	parent *BasicBlock
}

func CreateProcCall(callee Expr, args []Expr) *ProcCallInstr {
	return &ProcCallInstr{
		Op:     Call,
		Callee: callee,
		Args:   args,
	}
}

func (*ProcCallInstr) stmt()                          {}
func (c *ProcCallInstr) Parent() *BasicBlock          { return c.parent }
func (c *ProcCallInstr) SetParent(parent *BasicBlock) { c.parent = parent }
func (c *ProcCallInstr) String() string {
	var args []string
	for _, op := range c.Args {
		args = append(args, op.Name())
	}

	return fmt.Sprintf("call %s(%s)", c.Callee.Name(), strings.Join(args, ", "))
}

// Store
// -----------------------
type Store struct {
	Op    Opcode
	Value Expr
	Addr  Expr

	parent *BasicBlock
}

func CreateStore(addr, value Expr) *Store {
	return &Store{
		Op:    Str,
		Value: value,
		Addr:  addr,
	}
}

func (*Store) stmt()                          {}
func (s *Store) Parent() *BasicBlock          { return s.parent }
func (s *Store) SetParent(parent *BasicBlock) { s.parent = parent }
func (s *Store) String() string               { return fmt.Sprintf("%s %s %s", s.Op, s.Value, s.Addr) }
