package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/syntax/ast"
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
		m := &Module{Name: mod.Name}

		var decl Decl
		for _, glob := range mod.Decls {
			switch d := glob.(type) {
			case *hir.VariableDecl:
				decl = &VarDecl{Name: d.Name, Type: g.genType(d.Type)}
			case *hir.TypeDecl:
				decl = &TypeDecl{Name: d.Name, Type: g.genType(d.Type)}
			case *hir.ConstDecl:
				decl = &ConstDecl{Name: d.Name, Value: g.genOperand(d.Value)}
			}

			m.Decl = append(m.Decl, decl)
		}

		m.Init = g.genFunction(mod.Init)

		g.program.Modules = append(g.program.Modules, m)
	}

	return g.program
}

func (g *Generator) genFunction(h *hir.ProcedureDecl) *Function {
	g.labelCount, g.tempCount = 0, 0

	fn := &Function{Name: h.Name, Result: g.genType(h.Result)}

	for _, param := range h.Params {
		fn.Params = append(fn.Params, &Param{
			Name: param.Name,
			Type: g.genType(param.Type),
			Kind: param.Kind,
		})
	}

	for _, local := range h.Locals {
		switch d := local.(type) {
		case *hir.VariableDecl:
			fn.Locals = append(fn.Locals, Local{
				Name:   d.Name,
				Kind:   Var,
				Offset: d.Offset,
				Size:   d.Size,
			})
		case *hir.ConstDecl:
			fn.Locals = append(fn.Locals, Local{
				Name:   d.Name,
				Kind:   Const,
				Offset: d.Offset,
				Size:   d.Size,
			})
		}
	}

	entry := &Block{Label: "entry"}
	g.currentBlk = entry
	fn.Blocks = []*Block{entry}
	g.currentFn = fn

	g.genCompoundStmt(h.Body)

	return fn
}

func (g *Generator) genAssignStmt(s *hir.AssignStmt) {
	target := g.genOperand(s.Left)
	value := g.genOperand(s.Right)

	// Emit the assignment instruction
	g.emit(&AssignInst{Target: target, Value: value})
}

func (g *Generator) genReturnStmt(s *hir.ReturnStmt) {
	var result Operand
	if s.Result != nil {
		result = g.genOperand(s.Result)
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

func (g *Generator) genFuncCall(s *hir.FuncCall) Operand {
	retType := g.genType(s.RetType)
	t := g.newTemp(retType)

	callee := g.genOperand(s.Proc)

	var args []Operand
	for _, arg := range s.Args {
		args = append(args, g.genOperand(arg))
	}

	g.emit(&AssignInst{Target: t, Value: &FuncCall{
		Callee:  callee,
		Args:    args,
		RetType: retType,
	}})

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
		cond := g.genOperand(branch.Cond)
		falseLabel = g.newLabel(fmt.Sprintf("if.next_%d", i))
		trueLabel = g.newLabel(fmt.Sprintf("if.true_%d", i))

		g.emit(&CondBrInst{
			Cond:       cond,
			TrueLabel:  trueLabel,
			FalseLabel: falseLabel,
		})

		// Emit the conditional label for the true path block
		nextBlk := &Block{Label: trueLabel}
		g.currentFn.Blocks = append(g.currentFn.Blocks, nextBlk)
		g.currentBlk = nextBlk

		g.genCompoundStmt(branch.Body)
		g.emit(&JumpInst{Target: endLabel})

		// Emit the false path conditional label block
		nextBlk = &Block{Label: falseLabel}
		g.currentFn.Blocks = append(g.currentFn.Blocks, nextBlk)
		g.currentBlk = nextBlk
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		g.genCompoundStmt(s.Else)
	}
	g.emit(&JumpInst{Target: endLabel})

	// Final end block
	endBlk := &Block{Label: endLabel}
	g.currentFn.Blocks = append(g.currentFn.Blocks, endBlk)
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
	loopBlk := &Block{Label: loopLabel}
	g.currentFn.Blocks = append(g.currentFn.Blocks, loopBlk)
	g.currentBlk = loopBlk

	// Emit loop body
	g.genCompoundStmt(s.Body)

	// Jump back to loop
	g.emit(&JumpInst{Target: loopLabel})

	// Emit loop exit label
	exitBlk := &Block{Label: exitLabel}
	g.currentFn.Blocks = append(g.currentFn.Blocks, exitBlk)
	g.currentBlk = exitBlk
}

func (g *Generator) genExitStmt(*hir.ExitStmt) {
	if g.currentExit == "" {
		panic("EXIT used outside loop")
	}
	g.emit(&JumpInst{Target: g.currentExit})
}

func (g *Generator) genBinaryExpr(b *hir.BinaryExpr) Operand {
	t := g.newTemp(g.genType(b.Ty))
	lhs := g.genOperand(b.Left)
	rhs := g.genOperand(b.Right)
	bin := g.genBinaryOp(b.Op, lhs, rhs)

	g.emit(&AssignInst{Target: t, Value: bin})

	return t
}

func (g *Generator) genUnaryExpr(u *hir.UnaryExpr) Operand {
	typ := g.genType(u.Ty)
	t := g.newTemp(typ)

	var lhs Operand

	switch u.Op {
	case token.MINUS:
		if isIntType(typ) {
			lhs = &IntegerConst{Value: 0, Signed: true, Typ: typ}
		} else if isFloatType(typ) {
			lhs = &FloatConst{Value: 0.0, Bits: 64, Typ: typ}
		}

		rhs := g.genOperand(u.Operand)
		g.emit(&AssignInst{Target: t, Value: g.genBinaryOp(token.MINUS, lhs, rhs)})
	case token.NOT:
		rhs := g.genOperand(u.Operand)

		g.emit(&AssignInst{Target: t, Value: &Binary{
			Op:    ir.Xor,
			Left:  True,
			Right: rhs,
			Typ:   Int1Type,
		}})
	case token.PLUS:
	default:

	}

	return t
}

func (g *Generator) genIdent(n *hir.Ident) Operand {
	switch n.Kind {
	case ast.VariableSymbolKind:
		return &Variable{
			Name:       n.Name,
			Typ:        g.genType(n.Ty),
			Size:       n.Size,
			Offset:     n.Offset,
			IsExport:   n.Props == ast.Exported,
			IsReadOnly: n.Props == ast.ReadOnly,
		}
	default:
		panic(fmt.Sprintf("unknown symbol kind '%v'", n.Kind))
	}
}

func (g *Generator) genConst(c *hir.Literal) Operand {
	t := g.newTemp(g.genType(c.Ty))

	var v Operand

	switch c.Kind {
	case token.BYTE_LIT:
		value, _ := strconv.ParseUint(c.Value, 10, 8)
		v = &IntegerConst{Value: value, Bits: 8, Signed: false, Typ: g.genType(c.Ty)}
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

		v = &IntegerConst{Value: value, Bits: bits, Signed: true, Typ: g.genType(c.Ty)}
	case token.REAL_LIT, token.LONGREAL_LIT:
		value, _ := strconv.ParseFloat(c.Value, 64)

		var bits uint
		if c.Kind == token.REAL_LIT {
			bits = 32
		} else {
			bits = 64
		}

		v = &FloatConst{Value: value, Bits: bits, Typ: g.genType(c.Ty)}
	case token.CHAR_LIT, token.WCHAR_LIT:

		v = &CharConst{Value: []rune(c.Value), Typ: g.genType(c.Ty) /* additional info? */}
	case token.HEX_STR_LIT, token.STR_LIT:
		v = &StrConst{Value: c.Value, Typ: g.genType(c.Ty)}
	case token.TRUE:
		v = True
	case token.FALSE:
		v = False
	}

	g.emit(&AssignInst{Target: t, Value: v})

	return t
}

func (g *Generator) genOperand(e hir.Expr) Operand {
	switch e := e.(type) {
	case *hir.Ident:
		return g.genIdent(e)
	case *hir.Literal:
		return g.genConst(e)
	case *hir.BinaryExpr:
		return g.genBinaryExpr(e)
	case *hir.UnaryExpr:
		return g.genUnaryExpr(e)
	case *hir.FuncCall:
		return g.genFuncCall(e)
	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}

	return nil
}
