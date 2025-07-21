package mir

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Generator struct {
	currentProc *ProcedureDecl
	currentBlk  *Block
	program     *Program
	labelCount  int
	tempCount   int
	currentExit string
}

func NewGenerator() *Generator {
	return &Generator{}
}

func (g *Generator) Generate(prog *hir.Module) *Program {
	g.program = &Program{}

	m := &Module{Name: prog.Name}
	g.program.Modules = append(g.program.Modules, m)

	for _, proc := range prog.Procedures {
		m.Procedures = append(m.Procedures, g.genProc(proc))
	}

	var decl Decl
	for _, glob := range prog.Globals {
		switch d := glob.(type) {
		case *hir.VarDecl:
			decl = &VarDecl{Name: d.Name, Type: g.genMIRType(d.Type)}
		case *hir.TypeDecl:
			decl = &TypeDecl{Name: d.Name, Type: g.genMIRType(d.Type)}
		case *hir.ConstDecl:
			decl = &ConstDecl{Name: d.Name, Value: g.genExpr(d.Value)}
		}

		m.Globals = append(m.Globals, decl)
	}

	return g.program
}

func (g *Generator) genProc(h *hir.ProcedureDecl) *ProcedureDecl {
	g.labelCount, g.tempCount = 0, 0

	p := &ProcedureDecl{
		Name:   h.Name,
		Result: g.genMIRType(h.Result),
	}

	for _, param := range h.Params {
		p.Params = append(p.Params, &Param{
			Name: param.Name,
			Type: g.genMIRType(param.Type),
			Kind: param.Kind,
		})
	}

	var decl Decl
	for _, local := range h.Locals {
		switch d := local.(type) {
		case *hir.VarDecl:
			decl = &VarDecl{Name: d.Name, Type: g.genMIRType(d.Type)}
		case *hir.TypeDecl:
			decl = &TypeDecl{Name: d.Name, Type: g.genMIRType(d.Type)}
		case *hir.ConstDecl:

		}
		p.Locals = append(p.Locals, decl)
	}

	entry := &Block{Label: "entry"}
	g.currentBlk = entry
	p.Blocks = []*Block{entry}
	g.currentProc = p

	g.genCompoundStmt(h.Body)

	return p
}

func (g *Generator) genAssignStmt(s *hir.AssignStmt) {
	target := g.genExpr(s.Left)
	value := g.genExpr(s.Right)

	// Emit the assignment instruction
	g.emit(&AssignInst{Target: target, Value: value})
}

func (g *Generator) genReturnStmt(s *hir.ReturnStmt) {
	var result Operand
	if s.Result != nil {
		result = g.genExpr(s.Result)
	}
	g.emit(&ReturnInst{Result: result})
}

func (g *Generator) genCompoundStmt(s *hir.CompoundStmt) {
	for _, st := range s.Stmts {
		switch s := st.(type) {
		case *hir.AssignStmt:
			g.genAssignStmt(s)
		case *hir.CallStmt:
			g.genCallStmt(s)
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

func (g *Generator) genCallStmt(s *hir.CallStmt) {
	callee := g.genExpr(s.Proc)

	var args []Operand
	for _, arg := range s.Args {
		args = append(args, g.genExpr(arg))
	}

	g.emit(&ProcCallInst{Callee: callee, Args: args})
}

func (g *Generator) genIfStmt(s *hir.IfStmt) {
	endLabel := g.newLabel("if.end")

	// Track all conditional branches (initial + elsif)
	allConds := append([]*hir.ElseIfBranch{
		{Cond: s.Cond, Body: s.Then},
	}, s.ElseIfs...)

	var nextLabel string

	for i, branch := range allConds {
		cond := g.genExpr(branch.Cond)
		nextLabel = g.newLabel(fmt.Sprintf("if.next_%d", i))

		g.emit(&CondBrInst{
			Cond:       cond,
			FalseLabel: nextLabel,
		})

		g.genCompoundStmt(branch.Body)
		g.emit(&JumpInst{Target: endLabel})

		// Emit the next conditional label block
		nextBlk := &Block{Label: nextLabel}
		g.currentProc.Blocks = append(g.currentProc.Blocks, nextBlk)
		g.currentBlk = nextBlk
	}

	// ELSE branch or fallthrough
	if s.Else != nil {
		g.genCompoundStmt(s.Else)
	}

	// Final end block
	endBlk := &Block{Label: endLabel}
	g.currentProc.Blocks = append(g.currentProc.Blocks, endBlk)
	g.currentBlk = endBlk
}

func (g *Generator) genLoopStmt(s *hir.LoopStmt) {
	loopLabel := g.newLabel("loop")
	exitLabel := g.newLabel("exit")

	// Register exit label for EXIT lowering
	prevExit := g.currentExit
	g.currentExit = exitLabel
	defer func() { g.currentExit = prevExit }()

	// Emit loop label
	loopBlk := &Block{Label: loopLabel}
	g.currentProc.Blocks = append(g.currentProc.Blocks, loopBlk)
	g.currentBlk = loopBlk

	// Emit loop body
	g.genCompoundStmt(s.Body)

	// Jump back to loop
	g.emit(&JumpInst{Target: loopLabel})

	// Emit loop exit label
	exitBlk := &Block{Label: exitLabel}
	g.currentProc.Blocks = append(g.currentProc.Blocks, exitBlk)
	g.currentBlk = exitBlk
}

func (g *Generator) genExitStmt(s *hir.ExitStmt) {
	if g.currentExit == "" {
		panic("EXIT used outside loop")
	}
	g.emit(&JumpInst{Target: g.currentExit})
}

func (g *Generator) genExprs(exprs []hir.Expr) []Operand {
	var ops []Operand
	for _, e := range exprs {
		ops = append(ops, g.genExpr(e))
	}
	return ops
}

func (g *Generator) genExpr(e hir.Expr) Operand {
	switch e := e.(type) {
	case *hir.Literal:
		switch e.Kind {
		case token.BYTE_LIT, token.INT8_LIT, token.INT16_LIT, token.INT32_LIT, token.INT64_LIT:
			value, err := strconv.ParseInt(e.Value, 10, 64)
			if err != nil {
				panic("invalid literal value: " + e.Value)
			}
			return &IntConst{Value: value}
		}
	case *hir.BinaryExpr:
		return &Binary{
			Op:    e.Op,
			Left:  g.genExpr(e.Left),
			Right: g.genExpr(e.Right),
		}
	case *hir.UnaryExpr:
		return &Unary{
			Op:   e.Op,
			Expr: g.genExpr(e.Operand),
		}
	case *hir.FuncCallExpr:
		var args []Operand
		for _, arg := range e.Args {
			args = append(args, g.genExpr(arg))
		}

		return &FuncCall{
			Func: g.genExpr(e.Proc),
			Args: args,
		}
	case *hir.FieldAccess:
		return &FieldAccess{
			Record: g.genExpr(e.Record),
			Field:  e.Field,
		}
	case *hir.IndexExpr:
		return &IndexExpr{
			Array: g.genExpr(e.Array),
			Index: g.genExprs(e.Index),
		}
	case *hir.DerefExpr:
		return &DerefExpr{
			Pointer: g.genExpr(e.Pointer),
		}
	case *hir.TypeGuardExpr:
	case *hir.SetExpr:
	case *hir.RangeExpr:
	case *hir.Variable:
		return &Variable{
			Name:     e.Name,
			UniqName: e.MangledName,
			Type:     g.genMIRType(e.Type()),
			Props:    e.Props,
		}
	case *hir.Constant:
		return &Constant{
			Name:     e.Name,
			UniqName: e.MangledName,
			Type:     g.genMIRType(e.Type()),
			Props:    e.Props,
		}
	case *hir.Procedure:
		return &Procedure{
			Name:     e.Name,
			UniqName: e.MangledName,
			Ty:       g.genMIRType(e.Type()),
			Props:    e.Props,
		}

	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}

	return nil
}
