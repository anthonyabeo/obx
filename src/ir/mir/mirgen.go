package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Generator struct {
	program     *Program
	build       *Builder
	currentExit string
}

func NewGenerator() *Generator {
	return &Generator{
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

	fxn := make([]*Function, 0)
	globals := make(map[string]*Global)

	for _, decl := range module.Decls {
		switch d := decl.(type) {
		case *hir.Function:
			fxn = append(fxn, g.genFunction(d))
		case *hir.Constant:
			globals[d.Name] = &Global{
				NameStr: d.Name,
				Kind:    "CONST",
				Typ:     g.genType(d.Type),
				Value:   g.genValue(d.Value),
				Offset:  d.Offset,
				Size:    d.Size,
			}
		case *hir.Variable:
			globals[d.Name] = &Global{
				NameStr: d.Name,
				Kind:    "VAR",
				Typ:     g.genType(d.Type),
				Offset:  d.Offset,
				Size:    d.Size,
			}
		}
	}

	fxn = append(fxn, g.genFunction(module.Init))

	return &Module{
		Name:    module.Name,
		IsEntry: module.IsEntry,
		Globals: globals,
		Funcs:   fxn,
	}
}

func (g *Generator) genFunction(h *hir.Function) *Function {
	fn := NewFunction(h.Name, g.genType(h.Result))

	currentFn := g.build.Func
	defer func() { g.build.Func = currentFn }()

	g.build.Func = fn

	for _, param := range h.Params {
		switch param.Kind {
		case hir.ValueParam, hir.InParam:
			fn.Params[param.Name] = &Temp{
				ID:      param.Name,
				SrcName: param.Name,
				Typ:     g.genType(param.Typ),
			}
		case hir.VarParam:
		}
	}

	for _, local := range h.Locals {
		switch d := local.(type) {
		case *hir.Variable:
			fn.Locals = append(fn.Locals, &Temp{
				ID:      d.Name,
				SrcName: d.Name,
				Typ:     g.genType(d.Type),
				Offset:  d.Offset,
				Size:    d.Size,
			})
		case *hir.Constant:
			c := &Const{
				ID:     d.Name,
				Value:  g.genValue(d.Value),
				Typ:    g.genType(d.Type),
				Offset: d.Offset,
				Size:   d.Size,
			}
			fn.Constants[d.Mangled] = c
		case *hir.Function:
		}
	}

	entry := NewBlock("entry")
	g.build.SetBlock(entry)
	fn.Blocks[entry.ID] = entry

	g.genCompoundStmt(h.Body)

	return fn
}

func (g *Generator) genAssignStmt(s *hir.AssignStmt) {
	target := g.genValue(s.Left)
	value := g.genValue(s.Right)

	// Emit the assignment instruction
	g.build.Emit(&AssignInst{Target: target, Value: value})
}

func (g *Generator) genReturnStmt(s *hir.ReturnStmt) {
	var result Value
	if s.Result != nil {
		result = g.genValue(s.Result)
	}
	g.build.Emit(&ReturnInst{Result: result})
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
	for _, arg := range s.Args {
		args = append(args, g.genValue(arg))
	}

	return g.build.CreateCallInst(s.Func.Name, args)
}

func (g *Generator) genIfStmt(s *hir.IfStmt) {
	endLabel := g.build.NewLabel("if.end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: s.Cond, Body: s.Then},
	}, s.ElseIfs...)

	var trueLabel, falseLabel string

	for i, branch := range allConds {
		cond := g.genValue(branch.Cond)
		falseLabel = g.build.NewLabel(fmt.Sprintf("if.next.%d", i))
		trueLabel = g.build.NewLabel(fmt.Sprintf("if.true.%d", i))

		g.build.Emit(&CondBrInst{
			Cond:       cond,
			TrueLabel:  trueLabel,
			FalseLabel: falseLabel,
		})

		// Emit the conditional label for the true path block
		nextBlk := g.build.NewBlock(trueLabel)
		g.build.SetBlock(nextBlk)

		g.genCompoundStmt(branch.Body)
		g.build.Emit(&JumpInst{Target: endLabel})

		// Emit the false path conditional label block
		nextBlk = g.build.NewBlock(falseLabel)
		g.build.SetBlock(nextBlk)
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		g.genCompoundStmt(s.Else)
	}
	g.build.Emit(&JumpInst{Target: endLabel})

	// Final end block
	endBlk := g.build.NewBlock(endLabel)
	g.build.SetBlock(endBlk)
}

func (g *Generator) genLoopStmt(s *hir.LoopStmt) {
	loopLabel := g.build.NewLabel(s.Label)
	exitLabel := g.build.NewLabel(s.Label + ".exit")

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
	g.build.Emit(&JumpInst{Target: loopLabel})

	// Emit loop exit label
	exitBlk := g.build.NewBlock(exitLabel)
	g.build.SetBlock(exitBlk)
}

func (g *Generator) genExitStmt(*hir.ExitStmt) {
	if g.currentExit == "" {
		panic("EXIT used outside loop")
	}
	g.build.Emit(&JumpInst{Target: g.currentExit})
}

func (g *Generator) genBinaryExpr(b *hir.BinaryExpr) Value {
	lhs := g.genValue(b.Left)
	rhs := g.genValue(b.Right)
	op := g.genOp(b.Op)

	return g.build.CreateBinary(op, lhs, rhs)
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

	return g.build.CreateUnary(op, operand)
}

func (g *Generator) genConst(c *hir.Literal) Value {
	var v Value

	switch c.Kind {
	case token.BYTE_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 8)
		v = &IntegerConst{Value: value, Bits: 8, Signed: false, Typ: g.genType(c.SemaType)}
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

		v = &IntegerConst{Value: value, Bits: bits, Signed: true, Typ: g.genType(c.SemaType)}
	case token.REAL_LIT, token.LONGREAL_LIT:
		value, _ := strconv.ParseFloat(c.Value, 64)

		var bits uint
		if c.Kind == token.REAL_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &FloatConst{Value: value, Bits: bits, Typ: g.genType(c.SemaType)}
	case token.CHAR_LIT, token.WCHAR_LIT:
		v = &CharConst{Value: []rune(c.Value), Typ: g.genType(c.SemaType) /* additional info? */}
	case token.HEX_STR_LIT, token.STR_LIT:
		v = &StrConst{Value: c.Value, Typ: g.genType(c.SemaType)}
	case token.TRUE:
		v = True
	case token.FALSE:
		v = False
	}

	return v
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
		return &Temp{
			ID:      e.Name,
			SrcName: e.Name,
			Offset:  e.Offset,
			Size:    e.Size,
			Typ:     g.genType(e.SemaType),
		}
	case *hir.ConstantRef:
		return g.build.Func.Constants[e.Mangled].Value
	case *hir.Param:
		return g.build.Func.Params[e.Name]
	case *hir.FunctionRef:
	case *hir.TypeRef:
	case *hir.FieldAccess:
	case *hir.IndexExpr:
	case *hir.DerefExpr:
	case *hir.TypeGuardExpr:
	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}

	return nil
}
