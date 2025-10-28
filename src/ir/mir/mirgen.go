package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Generator struct {
	ctx         *report.Context
	program     *Program
	build       *Builder
	currentExit string
}

func NewGenerator(ctx *report.Context) *Generator {
	return &Generator{
		ctx:   ctx,
		build: NewBuilder(),
	}
}

func (g *Generator) Generate(prog *hir.Program) *Program {
	g.program = &Program{}

	for _, mod := range prog.Modules {
		g.program.Modules = append(g.program.Modules, g.genModule(mod))
	}

	return g.program
}

func (g *Generator) genModule(module *hir.Module) *Module {
	env := NewSymbolTable(nil)
	functions := make([]*Function, 0)
	globals := make(map[string]*Global)

	for _, decl := range module.Decls {
		switch d := decl.(type) {
		case *hir.Function:
			functions = append(functions, g.genFunction(d, NewSymbolTable(env)))
		case *hir.Constant:
			g := &Global{
				NameStr: d.Name,
				BName:   d.Name,
				Kind:    "CONST",
				Typ:     g.genType(d.Type),
				Value:   g.genValue(d.Value),
				Offset:  d.Offset,
				Size:    d.Size,
			}
			globals[d.Name] = g
			env.Define(g.NameStr, g)
		case *hir.Variable:
			g := &Global{
				NameStr: d.Name,
				BName:   d.Name,
				Kind:    "VAR",
				Typ:     g.genType(d.Type),
				Offset:  d.Offset,
				Size:    d.Size,
			}
			globals[d.Name] = g
			env.Define(g.NameStr, g)
		}
	}

	functions = append(functions, g.genFunction(module.Init, NewSymbolTable(env)))

	return &Module{
		Name:    module.Name,
		IsEntry: module.IsEntry,
		Globals: globals,
		Funcs:   functions,
		Env:     env,
	}
}

func (g *Generator) genFunction(h *hir.Function, env *SymbolTable) *Function {
	name := h.Mangled
	if h.Mangled == "" {
		name = h.Name
	}
	fn := NewFunction(name, h.IsExport, g.genType(h.Result), env)

	currentFn := g.build.Func
	defer func() { g.build.Func = currentFn }()

	g.build.Func = fn

	for _, param := range h.Params {
		switch param.Kind {
		case hir.ValueParam:
			t := &Param{
				ID:    param.Name,
				BName: param.Name,
				Typ:   g.genType(param.Typ),
			}

			fn.Params = append(fn.Params, t)
			fn.Env.Define(t.ID, t)
		case hir.InParam:
		case hir.VarParam:
		}
	}

	for _, local := range h.Locals {
		switch d := local.(type) {
		case *hir.Variable:
			lcl := &Local{
				ID:     d.Name,
				BName:  d.Name,
				Typ:    g.genType(d.Type),
				Offset: d.Offset,
				Size:   d.Size,
			}
			fn.Locals = append(fn.Locals, lcl)
			fn.Env.Define(lcl.ID, lcl)
		case *hir.Constant:
			c := &NamedConst{
				ID:         d.Name,
				ConstValue: g.genValue(d.Value),
				Typ:        g.genType(d.Type),
				Offset:     d.Offset,
				Size:       d.Size,
			}
			fn.Constants[d.Mangled] = c
			fn.Env.Define(c.ID, c)
		case *hir.Function:
		}
	}

	entry := NewBlock("entry")

	fn.Blocks[entry.ID] = entry
	fn.Entry = entry

	// mark "entry" as currently active block for inserting instructions
	g.build.SetBlock(entry)

	g.genCompoundStmt(h.Body)

	exit := NewBlock(fn.Name + "_exit")
	fn.Blocks[exit.ID] = exit
	fn.Exit = exit

	return fn
}

func (g *Generator) genAssignStmt(s *hir.AssignStmt) {
	target := g.genValue(s.Left)
	value := g.genValue(s.Right)

	if mem := isMem(target); mem != nil {
		g.build.CreateStore(mem, value)
		return
	}

	if mem := isMem(value); mem != nil {
		g.build.CreateLoad(target, value)
		return
	}

	// Emit the assignment instruction
	g.build.Emit(&MovInst{Target: target, Value: value})
}

func (g *Generator) genReturnStmt(s *hir.ReturnStmt) {
	var result Value
	if s.Result != nil {
		result = g.genValue(s.Result)
	}

	g.build.CreateReturn(result)
}

func (g *Generator) genCompoundStmt(s *hir.CompoundStmt) {
	for _, st := range s.Stmts {
		switch s := st.(type) {
		case *hir.AssignStmt:
			g.genAssignStmt(s)
		case *hir.ReturnStmt:
			g.genReturnStmt(s)
		case *hir.IfStmt:
			g.genIfStmt(s)
		case *hir.LoopStmt:
			g.genLoopStmt(s)
		case *hir.ExitStmt:
			g.genExitStmt(s)
		case *hir.CompoundStmt:
			g.genCompoundStmt(s)
		case *hir.FuncCall:
			g.genFuncCall(s)
		case *hir.CaseStmt:
		case *hir.WithStmt:
		default:
		}
	}
}

func (g *Generator) genFuncCall(s *hir.FuncCall) Value {
	var args []Value
	for idx, arg := range s.Args {
		v := g.genValue(arg)
		g.build.Emit(&Arg{Index: idx, Value: v})
		args = append(args, v)
	}

	var t Value
	if s.RetType != nil {
		t = g.build.NewTemp(s.RetType)
	}

	name := s.Func.Mangled
	if name == "" {
		name = s.Func.Name
	}

	g.build.Emit(&CallInst{
		Target: t,
		Callee: name,
		Args:   args,
	})

	g.build.Func.IsLeaf = false

	return t
}

func (g *Generator) genIfStmt(s *hir.IfStmt) {
	endLabel := g.build.NewLabel("if_end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: s.Cond, Body: s.Then},
	}, s.ElseIfs...)

	var trueLabel, falseLabel string

	for i, branch := range allConds {
		cond := g.genValue(branch.Cond)
		falseLabel = g.build.NewLabel(fmt.Sprintf("if_next_%d", i))
		trueLabel = g.build.NewLabel(fmt.Sprintf("if_true_%d", i))

		// set conditional branch as the terminator instruction for this block
		// and add it to the list of instructions in the block
		br := &CondBrInst{Cond: cond, TrueLabel: trueLabel, FalseLabel: falseLabel}
		g.build.SetTerm(br)
		g.build.Emit(br)

		// Emit the conditional label for the true path block
		nextBlk := g.build.NewBlock(trueLabel)
		g.build.SetBlock(nextBlk)

		g.genCompoundStmt(branch.Body)

		if !g.build.BlockTermSet() {
			jmp := &JumpInst{Target: endLabel}
			g.build.SetTerm(jmp)
			g.build.Emit(jmp)
		}

		// Emit the false path conditional label block
		nextBlk = g.build.NewBlock(falseLabel)
		g.build.SetBlock(nextBlk)
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		g.genCompoundStmt(s.Else)
	}
	//g.build.Emit(&JumpInst{Target: endLabel})
	if !g.build.BlockTermSet() {
		jmp := &JumpInst{Target: endLabel}
		g.build.SetTerm(jmp)
		g.build.Emit(jmp)
	}

	// Final end block
	endBlk := g.build.NewBlock(endLabel)
	g.build.SetBlock(endBlk)
}

func (g *Generator) genLoopStmt(s *hir.LoopStmt) {
	loopLabel := g.build.NewLabel(s.Label)
	exitLabel := g.build.NewLabel(s.Label + "_exit")

	// Register exit label for EXIT lowering
	prevExit := g.currentExit
	g.currentExit = exitLabel
	defer func() { g.currentExit = prevExit }()

	// Emit loop label
	loopBlk := g.build.NewBlock(loopLabel)
	g.build.SetBlock(loopBlk)

	// Emit loop body
	g.genCompoundStmt(s.Body)

	// Jump back to loop
	jmp := &JumpInst{Target: loopLabel}
	g.build.SetTerm(jmp)
	g.build.Emit(jmp)

	// Emit loop exit label
	exitBlk := g.build.NewBlock(exitLabel)
	g.build.SetBlock(exitBlk)
}

func (g *Generator) genExitStmt(*hir.ExitStmt) {
	if g.currentExit == "" {
		panic("EXIT used outside loop")
	}

	jmp := &JumpInst{Target: g.currentExit}
	g.build.SetTerm(jmp)
	g.build.Emit(jmp)
}

func (g *Generator) genBinaryExpr(b *hir.BinaryExpr) Value {
	lhs := g.genValue(b.Left)
	rhs := g.genValue(b.Right)
	op := g.genOp(b.Op)

	return g.build.CreateBinary(op, lhs, rhs, b.SemaType)
}

func (g *Generator) genUnaryExpr(u *hir.UnaryExpr) Value {
	var op InstrOp
	switch u.Op {
	case token.PLUS:
		op = ADD
	case token.MINUS:
		op = NEG
	case token.NOT:
		op = NOT
	}

	operand := g.genValue(u.Operand)

	return g.build.CreateUnary(op, operand, u.SemaType)
}

func (g *Generator) genConst(c *hir.Literal) Value {
	var v Value

	switch c.Kind {
	case token.BYTE_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 8)
		v = &IntegerLit{LitValue: value, Bits: 8, Signed: false, Typ: g.genType(c.SemaType)}
	case token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 64)

		var bits uint
		if c.Kind == token.INT8_LIT {
			bits = 8
		} else if c.Kind == token.INT16_LIT {
			bits = 16
		} else if c.Kind == token.INT32_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &IntegerLit{LitValue: value, Bits: bits, Signed: true, Typ: g.genType(c.SemaType)}
	case token.REAL_LIT, token.LONGREAL_LIT:
		value, _ := strconv.ParseFloat(c.Value, 64)

		var bits uint
		if c.Kind == token.REAL_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &FloatLit{LitValue: value, Bits: bits, Typ: g.genType(c.SemaType)}
	case token.CHAR_LIT, token.WCHAR_LIT:
		v = &CharLit{LitValue: []rune(c.Value), Typ: g.genType(c.SemaType) /* additional info? */}
	case token.HEX_STR_LIT, token.STR_LIT:
		name := "str_const_" + strconv.Itoa(len(g.build.Func.Constants))
		str := &StrLit{LitName: name, LitValue: c.Value, Typ: g.genType(c.SemaType)}

		g.build.Func.Constants[name] = str
		v = str
	case token.TRUE:
		v = True
	case token.FALSE:
		v = False
	}

	t := g.build.NewTemp(v.Type())
	g.build.Emit(&MovInst{Target: t, Value: v})

	return t
}

func (g *Generator) genIndexExpr(e *hir.IndexExpr) Value {
	// Generate the base array or pointer value
	arr := g.genValue(e.Array)
	arrayType := arr.Type().(*ArrayType)

	var indices []Value
	for _, idx := range e.Index {
		indices = append(indices, g.genValue(idx))
	}

	// 2) Compute offset in BYTES using strides
	strides := arrayType.Strides()
	if len(strides) != len(indices) {
		panic("LowerArrayIndexAddr: index count does not match array rank")
	}

	// acc = 0
	var acc Value
	acc = g.build.NewTemp(Int64Type)
	g.build.CreateAssign(acc, &IntegerLit{LitValue: 0})

	for k := 0; k < len(indices); k++ {
		// term = indices[k] * strides[k]
		tMul := g.build.CreateBinary(MUL, indices[k], &IntegerLit{LitValue: uint64(strides[k])}, Int64Type)

		// acc = acc + term
		acc = g.build.CreateBinary(ADD, acc, tMul, Int64Type)
	}

	// 3) addr = baseAddr + acc
	addr := g.build.CreateBinary(ADD, arr, acc, Int64Type)
	return &Mem{Base: addr}
}

func (g *Generator) genValue(e hir.Expr) Value {
	switch e := e.(type) {
	case *hir.Literal:
		return g.genConst(e)
	case *hir.BinaryExpr:
		return g.genBinaryExpr(e)
	case *hir.UnaryExpr:
		return g.genUnaryExpr(e)
	case *hir.FuncCall:
		return g.genFuncCall(e)
	case *hir.SetExpr:
	case *hir.RangeExpr:
	case *hir.VariableRef:
		if v, found := g.build.Func.Env.Lookup(e.Name); found {
			return v
		}

		panic(fmt.Sprintf("undefined variable: '%s'", e.Name))
	case *hir.ConstantRef:
		return g.build.Func.Constants[e.Mangled]
	case *hir.Param:
		if v, found := g.build.Func.Env.Lookup(e.Name); found {
			return v
		}
		panic(fmt.Sprintf("undefined parameter: '%s'", e.Name))
	case *hir.FunctionRef:
	case *hir.TypeRef:
	case *hir.FieldAccess:
	case *hir.IndexExpr:
		return g.genIndexExpr(e)
	case *hir.DerefExpr:
	case *hir.TypeGuardExpr:
	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}

	return nil
}
