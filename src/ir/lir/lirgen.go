package lir

import (
	"fmt"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

type Generator struct {
	nextTemp  int
	lir       *Program
	currBlock *Block
}

func NewGenerator() *Generator {
	return &Generator{}
}

func (g *Generator) Generate(m *mir.Program) *Program {
	prog := &Program{}

	for _, mod := range m.Modules {
		prog.Modules = append(prog.Modules, g.lowerModule(mod))
	}

	return prog
}

func (g *Generator) lowerModule(mod *mir.Module) *Module {
	lirMod := &Module{Name: mod.Name}

	// Lower global variables
	var decl *GlobalDecl
	for _, glob := range mod.Globals {
		switch glob := glob.(type) {
		case *mir.VarDecl:
			decl = &GlobalDecl{Name: glob.Name, Type: g.lowerType(glob.Type)}
		case *mir.ConstDecl:
			decl = &GlobalDecl{Name: glob.Name /*Type: g.lowerType(g.Type),*/, Init: g.lowerOperand(glob.Value)}
		case *mir.TypeDecl:
		}
		lirMod.Globals = append(lirMod.Globals, decl)
	}

	// Lower each procedure
	for _, mirProc := range mod.Procedures {
		lirProc := g.lowerProcedure(mirProc)
		lirMod.Procs = append(lirMod.Procs, lirProc)
	}

	// Lower init if present
	if mod.Init != nil {
		lirInit := g.lowerProcedure(mod.Init)
		lirInit.Name = "init"
		lirMod.Procs = append(lirMod.Procs, lirInit)
	}

	return lirMod
}

func (g *Generator) lowerProcedure(fn *mir.Procedure) *Procedure {
	proc := &Procedure{
		Name:   fn.Name,
		Params: make([]*Param, 0, len(fn.Params)),
	}

	// Lower parameters
	for _, param := range fn.Params {
		reg := &Param{Name: param.Name, Type: g.lowerType(param.Type)}
		proc.Params = append(proc.Params, reg)
	}

	// lower return type
	if fn.Result != nil {
		proc.Ret = g.lowerType(fn.Result)
	}

	// Lower locals
	var local *Local
	for _, decl := range fn.Locals {
		switch decl := decl.(type) {
		case *mir.VarDecl:
			local = &Local{Name: decl.Name, Type: g.lowerType(decl.Type)}
		case *mir.ConstDecl:
			local = &Local{Name: decl.Name}
		}
		proc.Locals = append(proc.Locals, local)

	}

	// Convert blocks
	for _, mirBlock := range fn.Blocks {
		proc.Blocks = append(proc.Blocks, g.lowerBlock(mirBlock))
	}

	return proc
}

func (g *Generator) lowerBlock(b *mir.Block) *Block {
	block := &Block{Name: b.Label}

	tmp := g.currBlock
	g.currBlock = block
	defer func() { g.currBlock = tmp }()

	for _, inst := range b.Instrs {
		g.lowerInst(inst)
	}

	return block
}

func (g *Generator) lowerAssign(inst *mir.AssignInst) {
	dst := g.lowerOperand(inst.Target)
	val := g.lowerOperand(inst.Value)
	g.emit(&MovInst{Src: val, Dst: dst})
}

func (g *Generator) lowerBinary(inst *mir.BinaryInst) {
	dst := g.lowerOperand(inst.Dst)
	left := g.lowerOperand(inst.Left)
	right := g.lowerOperand(inst.Right)
	g.emit(&BinaryInst{Op: g.lowerOp(inst.Op), Dst: dst, Lhs: left, Rhs: right})
}

func (g *Generator) lowerJump(inst *mir.JumpInst) {
	g.emit(&JmpInst{Dst: &Label{Name: inst.Target}})
}

func (g *Generator) lowerCondBr(inst *mir.CondBrInst) {
	cond := g.lowerOperand(inst.Cond)
	g.emit(&CondBrInst{
		Cond:    cond,
		IfTrue:  &Label{Name: inst.TrueLabel},
		IfFalse: &Label{Name: inst.FalseLabel},
	})
}

func (g *Generator) lowerFuncCall(inst *mir.FuncCallInst) {
	args := make([]Operand, len(inst.Args))
	for i, arg := range inst.Args {
		args[i] = g.lowerOperand(arg)
	}

	dst := g.lowerOperand(inst.Dst)
	fn := g.lowerOperand(inst.Func)
	g.emit(&CallInst{Func: fn, Args: args, Dst: dst})
}

func (g *Generator) lowerReturnInst(inst *mir.ReturnInst) {
	var operand Operand
	if inst.Result != nil {
		operand = g.lowerOperand(inst.Result)
	}

	g.emit(&RetInst{Value: operand})
}

func (g *Generator) lowerCmpInst(inst *mir.CmpInst) {
	g.emit(&CmpInst{
		Dst: g.lowerOperand(inst.Dst),
		X:   g.lowerOperand(inst.X),
		Y:   g.lowerOperand(inst.Y),
		Op:  g.lowerOp(inst.Op),
	})
}

func (g *Generator) lowerProcCallInst(inst *mir.ProcCallInst) {
	var args []Operand
	for _, arg := range inst.Args {
		args = append(args, g.lowerOperand(arg))
	}

	g.emit(&CallInst{
		Func: g.lowerOperand(inst.Callee),
		Args: args,
	})
}

func (g *Generator) lowerOperand(op mir.Operand) Operand {
	switch x := op.(type) {
	case *mir.Variable:
		return &Var{Name: x.Name}
	case *mir.IntConst:
		return &IntegerConst{Value: uint64(x.Value), Signed: true}
	//case *mir.BoolConst:
	//case *mir.StringConst:
	//case *mir.Nil:
	default:
		panic(fmt.Sprintf("unsupported operand: %T", x))
	}
}

func (g *Generator) lowerInst(inst mir.Inst) {
	switch x := inst.(type) {
	case *mir.AssignInst:
		g.lowerAssign(x)
	case *mir.BinaryInst:
		g.lowerBinary(x)
	case *mir.JumpInst:
		g.lowerJump(x)
	case *mir.CondBrInst:
		g.lowerCondBr(x)
	case *mir.FuncCallInst:
		g.lowerFuncCall(x)
	case *mir.ReturnInst:
		g.lowerReturnInst(x)
	case *mir.CmpInst:
		g.lowerCmpInst(x)
	case *mir.ProcCallInst:
		g.lowerProcCallInst(x)
	
	default:
		panic(fmt.Sprintf("unsupported MIR instruction: %T", x))
	}
}

func (g *Generator) lowerType(t mir.Type) Type {
	switch ty := t.(type) {
	case *mir.IntegerType:
		return &IntegerType{Bits: ty.Bits, Signed: ty.Signed}
	case *mir.PointerType:
		return &PtrType{Base: g.lowerType(ty.Ref)}
	default:
		panic(fmt.Sprintf("unsupported MIR type: %T", t))
	}
}

func (g *Generator) lowerOp(op string) Op {
	switch op {
	case "+":
		return Add
	case "-":
		return Sub
	case "*":
		return Mul
	case "lt":
		return Lt
	case "leq":
		return Leq
	case "gt":
		return Gt
	case "geq":
		return Geq
	case "eq":
		return Eq
	default:
		panic(fmt.Sprintf("unsupported MIR op: %s", op))
	}
}

func (g *Generator) newTemp(t Type) *Register {
	name := fmt.Sprintf("t%d", g.nextTemp)
	g.nextTemp++
	return &Register{Name: name, Type: t}
}

func (g *Generator) emit(inst Inst) {
	g.currBlock.Instrs = append(g.currBlock.Instrs, inst)
}
