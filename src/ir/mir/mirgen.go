package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Generator struct {
	currentFn   *Function
	currentBlk  *Block
	program     *Program
	labelCount  int
	tempCount   int
	currentExit string
}

func NewGenerator() *Generator {
	return &Generator{}
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
	g.labelCount, g.tempCount = 0, 0

	fn := &Function{Name: h.Name, Result: g.genType(h.Result), Blocks: make(map[string]*Block)}

	for _, param := range h.Params {
		switch param.Kind {
		case hir.ValueParam, hir.InParam:
			fn.Params = append(fn.Params, &Temp{
				ID:      param.Name,
				SrcName: param.Name,
				Typ:     g.genType(param.Type),
			})
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
	g.currentBlk = entry
	fn.Blocks["entry"] = entry
	g.currentFn = fn

	g.genCompoundStmt(h.Body)

	return fn
}

func (g *Generator) genAssignStmt(s *hir.AssignStmt) {
	target := g.genValue(s.Left)
	value := g.genValue(s.Right)

	// Emit the assignment instruction
	g.emit(&AssignInst{Target: target, Value: value})
}

func (g *Generator) genReturnStmt(s *hir.ReturnStmt) {
	var result Value
	if s.Result != nil {
		result = g.genValue(s.Result)
	}
	g.emit(&ReturnInst{Result: result})
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
		case *hir.CaseStmt:
		case *hir.WithStmt:
		default:
		}
	}
}

func (g *Generator) genFuncCall(s *hir.FuncCall) Value {
	retType := g.genType(s.RetType)
	t := g.newTemp(retType)

	var args []Value
	for _, arg := range s.Args {
		args = append(args, g.genValue(arg))
	}

	g.emit(&CallInst{Target: t, Callee: s.Func.Name, Args: args})

	return t
}

func (g *Generator) genIfStmt(s *hir.IfStmt) {
	endLabel := g.newLabel("if.end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: s.Cond, Body: s.Then},
	}, s.ElseIfs...)

	var trueLabel, falseLabel string

	for i, branch := range allConds {
		cond := g.genValue(branch.Cond)
		falseLabel = g.newLabel(fmt.Sprintf("if.next_%d", i))
		trueLabel = g.newLabel(fmt.Sprintf("if.true_%d", i))

		g.emit(&CondBrInst{
			Cond:       cond,
			TrueLabel:  trueLabel,
			FalseLabel: falseLabel,
		})

		// Emit the conditional label for the true path block
		nextBlk := NewBlock(trueLabel)
		g.currentFn.Blocks[trueLabel] = nextBlk
		g.currentBlk = nextBlk

		g.genCompoundStmt(branch.Body)
		g.emit(&JumpInst{Target: endLabel})

		// Emit the false path conditional label block
		nextBlk = NewBlock(falseLabel)
		g.currentFn.Blocks[falseLabel] = nextBlk
		g.currentBlk = nextBlk
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		g.genCompoundStmt(s.Else)
	}
	g.emit(&JumpInst{Target: endLabel})

	// Final end block
	endBlk := NewBlock(endLabel)
	g.currentFn.Blocks[endLabel] = endBlk
	g.currentBlk = endBlk
}

func (g *Generator) genLoopStmt(s *hir.LoopStmt) {
	loopLabel := g.newLabel(s.Label)
	exitLabel := g.newLabel(s.Label + ".exit")

	// Register exit label for EXIT lowering
	prevExit := g.currentExit
	g.currentExit = exitLabel
	defer func() { g.currentExit = prevExit }()

	// Emit loop label
	loopBlk := NewBlock(loopLabel)
	g.currentFn.Blocks[loopLabel] = loopBlk
	g.currentBlk = loopBlk

	// Emit loop body
	g.genCompoundStmt(s.Body)

	// Jump back to loop
	g.emit(&JumpInst{Target: loopLabel})

	// Emit loop exit label
	exitBlk := NewBlock(exitLabel)
	g.currentFn.Blocks[exitLabel] = exitBlk
	g.currentBlk = exitBlk
}

func (g *Generator) genExitStmt(*hir.ExitStmt) {
	if g.currentExit == "" {
		panic("EXIT used outside loop")
	}
	g.emit(&JumpInst{Target: g.currentExit})
}

func (g *Generator) genBinaryExpr(b *hir.BinaryExpr) Value {
	t := g.newTemp(g.genType(b.SemaType))
	lhs := g.genValue(b.Left)
	rhs := g.genValue(b.Right)

	g.emit(&BinaryInst{Target: t, Op: g.genOp(b.Op), Left: lhs, Right: rhs})

	return t
}

func (g *Generator) genUnaryExpr(u *hir.UnaryExpr) Value {
	typ := g.genType(u.SemaType)
	t := g.newTemp(typ)

	var op InstrOp
	switch u.Op {
	case token.PLUS:
		op = ADD
	case token.MINUS:
		op = SUB
	case token.NOT:
		op = NOT
	}

	g.emit(&UnaryInst{
		Target:  t,
		Op:      op,
		Operand: g.genValue(u.Operand),
	})

	return t
}

func (g *Generator) genConst(c *hir.Literal) Value {
	t := g.newTemp(g.genType(c.SemaType))

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

	g.emit(&AssignInst{Target: t, Value: v})

	return t
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
		return g.currentFn.Constants[e.Mangled].Value
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
