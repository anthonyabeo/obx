package mir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/hir"
)

type Generator struct {
	currentProc *Procedure
	currentBlk  *Block
	program     *Program
	labelCount  int
	tempCount   int
	currentExit string
}

func (g *Generator) Generate(prog *hir.Module) *Program {
	g.program = &Program{}

	m := &Module{Name: prog.Name}
	g.program.Modules = append(g.program.Modules, m)

	for _, proc := range prog.Procedures {
		m.Procedures = append(m.Procedures, g.lowerProc(proc))
	}

	var decl Decl
	for _, glob := range prog.Globals {
		switch d := glob.(type) {
		case *hir.VarDecl:
			decl = &VarDecl{Name: d.Name, Type: toMIRType(d.Type)}
		case *hir.TypeDecl:
			decl = &TypeDecl{Name: d.Name, Type: toMIRType(d.Type)}
		case *hir.ConstDecl:
			//decl = &ConstDecl{Name: d.Name, Value: d.Value}
		}

		m.Globals = append(m.Globals, decl)
	}

	return g.program
}

func (g *Generator) lowerProc(h *hir.Procedure) *Procedure {
	g.labelCount, g.tempCount = 0, 0

	p := &Procedure{
		Name:   h.Name,
		Result: toMIRType(h.Result),
	}

	for _, param := range h.Params {
		p.Params = append(p.Params, &Param{
			Name: param.Name,
			Type: toMIRType(param.Type),
			Kind: param.Kind,
		})
	}

	var decl Decl
	for _, local := range h.Locals {
		switch d := local.(type) {
		case *hir.VarDecl:
			decl = &VarDecl{Name: d.Name, Type: toMIRType(d.Type)}
		case *hir.TypeDecl:
			decl = &TypeDecl{Name: d.Name, Type: toMIRType(d.Type)}
		case *hir.ConstDecl:

		}
		p.Locals = append(p.Locals, decl)
	}

	entry := &Block{Label: "entry"}
	g.currentBlk = entry
	p.Blocks = []*Block{entry}
	g.currentProc = p

	g.lowerStmt(h.Body)

	return p
}

func (g *Generator) lowerStmt(s hir.Stmt) {
	switch s := s.(type) {
	case *hir.AssignStmt:
		g.emit(&AssignInstr{Target: g.lowerExpr(s.Lhs), Value: g.lowerExpr(s.Rhs)})

	case *hir.ReturnStmt:
		var result Value
		if s.Result != nil {
			result = g.lowerExpr(s.Result)
		}
		g.emit(&ReturnInstr{Result: result})

	case *hir.CompoundStmt:
		for _, st := range s.Stmts {
			g.lowerStmt(st)
		}
	case *hir.IfStmt:
		endLabel := g.newLabel("ifend")

		// Track all conditional branches (initial + elsif)
		allConds := append([]*hir.ElseIfBranch{
			{Cond: s.Cond, Body: s.Then},
		}, s.ElseIfs...)

		var nextLabel string

		for i, branch := range allConds {
			cond := g.lowerExpr(branch.Cond)
			nextLabel = g.newLabel(fmt.Sprintf("ifnext_%d", i))

			g.emit(&CondBrInstr{
				Cond:       cond,
				FalseLabel: nextLabel,
			})

			g.lowerStmt(branch.Body)
			g.emit(&JumpInstr{Target: endLabel})

			// Emit the next conditional label block
			nextBlk := &Block{Label: nextLabel}
			g.currentProc.Blocks = append(g.currentProc.Blocks, nextBlk)
			g.currentBlk = nextBlk
		}

		// ELSE branch or fallthrough
		if s.Else != nil {
			g.lowerStmt(s.Else)
		}

		// Final end block
		endBlk := &Block{Label: endLabel}
		g.currentProc.Blocks = append(g.currentProc.Blocks, endBlk)
		g.currentBlk = endBlk
	case *hir.LoopStmt:
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
		g.lowerStmt(s.Body)

		// Jump back to loop
		g.emit(&JumpInstr{Target: loopLabel})

		// Emit loop exit label
		exitBlk := &Block{Label: exitLabel}
		g.currentProc.Blocks = append(g.currentProc.Blocks, exitBlk)
		g.currentBlk = exitBlk
	case *hir.ExitStmt:
		if g.currentExit == "" {
			panic("EXIT used outside loop")
		}
		g.emit(&JumpInstr{Target: g.currentExit})

	// Add more stmt types here...

	default:
		panic("unhandled stmt: " + fmt.Sprintf("%T", s))
	}
}

func (g *Generator) lowerExpr(e hir.Expr) Value {
	switch e := e.(type) {
	case *hir.IntConst:
		return &IntConst{Value: e.Value}

	case *hir.VarExpr:
		return &Variable{Name: e.Name}

	case *hir.BinaryExpr:
		left := g.lowerExpr(e.Left)
		right := g.lowerExpr(e.Right)

		return &Binary{Op: string(e.Op), Left: left, Right: right}

	// Handle call, unary, set const, etc.

	default:
		panic("unhandled expr: " + fmt.Sprintf("%T", e))
	}
}
