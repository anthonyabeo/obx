package lir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/anthonyabeo/obx/src/report"
)

type Generator struct {
	ctx *report.Context

	nextTemp  int
	lir       *Program
	currBlock *Block
}

func NewGenerator(ctx *report.Context) *Generator {
	return &Generator{ctx: ctx}
}

func (g *Generator) Generate(m *mir.Program) *Program {
	prog := &Program{}

	for _, mod := range m.Modules {
		prog.Modules = append(prog.Modules, g.genModule(mod))
	}

	return prog
}

func (g *Generator) genModule(mod *mir.Module) *Module {
	lirMod := &Module{Name: mod.Name}

	// Lower global variables
	var decl *GlobalDecl
	for _, glob := range mod.Globals {
		switch glob := glob.(type) {
		case *mir.VarDecl:
			decl = &GlobalDecl{Name: glob.Name, Type: g.lowerType(glob.Type)}
		case *mir.ConstDecl:
			decl = &GlobalDecl{Name: glob.Name /*Type: g.lowerType(g.Type),*/, Init: g.genOperand(glob.Value)}
		case *mir.TypeDecl:
		}
		lirMod.Globals = append(lirMod.Globals, decl)
	}

	// Lower each procedure
	for _, mirProc := range mod.Procedures {
		lirProc := g.genProcedure(mirProc)
		lirMod.Procs = append(lirMod.Procs, lirProc)
	}

	// Lower init if present
	if mod.Init != nil {
		lirInit := g.genProcedure(mod.Init)
		lirInit.Name = "init"
		lirMod.Procs = append(lirMod.Procs, lirInit)
	}

	return lirMod
}

func (g *Generator) genProcedure(fn *mir.ProcedureDecl) *Procedure {
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
		proc.Blocks = append(proc.Blocks, g.genBlock(mirBlock))
	}

	return proc
}

func (g *Generator) genInstructions(insts []mir.Inst) {
	for _, inst := range insts {
		switch i := inst.(type) {
		case *mir.AssignInst:
			g.genAssign(i)
		case *mir.ProcCallInst:
			g.genProcCallInst(i)
		case *mir.JumpInst:
			g.genJump(i)
		case *mir.CondBrInst:
			g.genCondBr(i)
		case *mir.ReturnInst:
			g.genReturnInst(i)
		case *mir.ExitInst:
			g.genExit(i)
		}
	}
}

func (g *Generator) genBlock(b *mir.Block) *Block {
	block := &Block{Name: b.Label}

	tmp := g.currBlock
	g.currBlock = block
	defer func() { g.currBlock = tmp }()

	g.genInstructions(b.Inst)

	return block
}

func (g *Generator) genAssign(inst *mir.AssignInst) {
	dst := g.genOperand(inst.Target)
	val := g.genOperand(inst.Value)
	g.emit(&MovInst{Src: val, Dst: dst})
}

func (g *Generator) genBinary(inst *mir.Binary) Operand {
	dst := g.newTemp(g.lowerType(inst.Type))
	left := g.genOperand(inst.Left)
	right := g.genOperand(inst.Right)

	g.emit(&BinaryInst{
		Op:  inst.Op,
		Dst: dst,
		Lhs: left,
		Rhs: right,
	})

	return dst
}

func (g *Generator) genUnary(inst *mir.Unary) Operand {
	dst := g.newTemp(g.lowerType(inst.Type))
	val := g.genOperand(inst.Expr)

	g.emit(&UnaryInst{
		Op:  inst.Op,
		Dst: dst,
		Val: val,
	})

	return dst
}

func (g *Generator) genJump(inst *mir.JumpInst) {
	g.emit(&JmpInst{Dst: &Label{Name: inst.Target}})
}

func (g *Generator) genCondBr(inst *mir.CondBrInst) {
	cond := g.genOperand(inst.Cond)
	g.emit(&CondBrInst{
		Cond:    cond,
		IfTrue:  &Label{Name: inst.TrueLabel},
		IfFalse: &Label{Name: inst.FalseLabel},
	})
}

func (g *Generator) genFuncCall(inst *mir.FuncCall) Operand {
	args := make([]Operand, len(inst.Args))
	for i, arg := range inst.Args {
		args[i] = g.genOperand(arg)
	}

	dst := g.newTemp(g.lowerType(inst.Type))
	fn := g.genOperand(inst.Func)
	g.emit(&CallInst{Func: fn, Args: args, Dst: dst})

	return dst
}

func (g *Generator) genReturnInst(inst *mir.ReturnInst) {
	var operand Operand
	if inst.Result != nil {
		operand = g.genOperand(inst.Result)
	}

	g.emit(&RetInst{Value: operand})
}

func (g *Generator) genCmpInst(inst *mir.Cmp) Operand {
	dst := g.newTemp(Int1Type)

	g.emit(&CmpInst{
		Dst: dst,
		Op:  inst.Op,
		X:   g.genOperand(inst.X),
		Y:   g.genOperand(inst.Y),
	})

	return dst
}

func (g *Generator) genProcCallInst(inst *mir.ProcCallInst) {
	var args []Operand
	for _, arg := range inst.Args {
		args = append(args, g.genOperand(arg))
	}

	g.emit(&CallInst{
		Func: g.genOperand(inst.Callee),
		Args: args,
	})
}

func (g *Generator) genExit(inst *mir.ExitInst) {
	g.emit(&JmpInst{Dst: &Label{Name: inst.LoopLabel.Name.Name}})
}

func (g *Generator) genOperand(op mir.Operand) Operand {
	switch x := op.(type) {
	case *mir.Variable:
		return &Var{Name: x.Name}
	case *mir.Constant:
		panic("not implemented")
	case *mir.Procedure:
		panic("not implemented")
	case *mir.FieldAccess:
		panic("not implemented")
	case *mir.IndexExpr:
		panic("not implemented")
	case *mir.DerefExpr:
		panic("not implemented")
	case *mir.TypeGuardExpr:
		panic("not implemented")
	case *mir.IntConst:
		return &IntegerConst{Value: uint64(x.Value), Signed: true}
	case *mir.BoolConst:
		panic("not implemented")
	case *mir.StringConst:
		panic("not implemented")
	case *mir.Nil:
		panic("not implemented")
	case *mir.Binary:
		return g.genBinary(x)
	case *mir.Unary:
		return g.genUnary(x)
	case *mir.FuncCall:
		return g.genFuncCall(x)
	case *mir.Cmp:
		return g.genCmpInst(x)
	default:
		panic(fmt.Sprintf("unsupported operand: %T", x))
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

func (g *Generator) newTemp(t Type) *Register {
	name := fmt.Sprintf("t%d", g.nextTemp)
	g.nextTemp++
	return &Register{Name: name, Type: t}
}

func (g *Generator) emit(inst Inst) {
	g.currBlock.Inst = append(g.currBlock.Inst, inst)
}
