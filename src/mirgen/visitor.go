package mirgen

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Visitor struct {
	PrgUnit *meer.ProgramUnit

	env    scope.Scope
	scopes map[string]scope.Scope

	loopExitTarget *meer.Label
}

func NewVisitor(scopes map[string]scope.Scope) *Visitor {
	return &Visitor{scopes: scopes}
}

func (v *Visitor) Translate(ob *ast.Oberon, order []string) *meer.Program {
	program := meer.NewProgram()

	for _, name := range order {
		unit := ob.Units()[name]
		unit.Accept(v)
		program.Units[name] = v.PrgUnit
	}

	return program
}

func (v *Visitor) VisitOberon(oberon *ast.Oberon) {}

func (v *Visitor) VisitModule(m *ast.Module) {
	v.env = v.scopes[m.BName.Name]
	v.PrgUnit = meer.NewProgramUnit(m.BName.Name)

	for _, decl := range m.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range m.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitDefinition(definition *ast.Definition) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	sym := v.env.Lookup(id.Name)
	if sym == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.MirExpr = &meer.Ident{Id: id.Name}
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	var instr meer.Expression

	switch expr.Op {
	case token.PLUS:
		instr = meer.CreateBinaryOp(meer.Add, expr.Left.MirValue(), expr.Right.MirValue())
	case token.EQUAL:
		instr = meer.CreateCmpInst(meer.Eq, expr.Left.MirValue(), expr.Right.MirValue())
	case token.LESS:
		instr = meer.CreateCmpInst(meer.Lt, expr.Left.MirValue(), expr.Right.MirValue())
	case token.LEQ:
		instr = meer.CreateCmpInst(meer.Le, expr.Left.MirValue(), expr.Right.MirValue())
	case token.GEQ:
		instr = meer.CreateCmpInst(meer.Ge, expr.Left.MirValue(), expr.Right.MirValue())
	case token.GREAT:
		instr = meer.CreateCmpInst(meer.Gt, expr.Left.MirValue(), expr.Right.MirValue())
	case token.NEQ:
		instr = meer.CreateCmpInst(meer.Ne, expr.Left.MirValue(), expr.Right.MirValue())
	}

	expr.MirExpr = instr
}

func (v *Visitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(v)
	if d.Selector == nil {
		d.MirExpr = d.QualifiedIdent.MirValue()
		return
	}

	switch d.Selector.(type) {
	case *ast.DotOp:
	case *ast.IndexOp:
	case *ast.PtrDref:
	case *ast.TypeGuard:
	}
}

func (v *Visitor) VisitFuncCall(call *ast.FuncCall) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	expr.X.Accept(v)

	switch expr.Op {
	case token.MINUS:
		expr.MirExpr = meer.CreateUnaryOp(meer.Sub, expr.X.MirValue())
	case token.NOT:
		expr.MirExpr = meer.CreateUnaryOp(meer.Not, expr.X.MirValue())
	case token.PLUS:
		expr.MirExpr = expr.X.MirValue()
	default:
		panic(fmt.Sprintf("[internal] invalid unary operator '%s'", expr.Op))
	}
}

func (v *Visitor) VisitQualifiedIdent(ident *ast.QualifiedIdent) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitSet(set *ast.Set) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitBasicLit(lit *ast.BasicLit) {
	val, err := strconv.ParseUint(lit.Val, 10, 64)
	if err != nil {
		panic(fmt.Sprintf("[internal] unable to parse %s", lit.Val))
	}

	lit.MirExpr = &meer.IntegerConst{Value: val}
}

func (v *Visitor) VisitExprRange(exprRange *ast.ExprRange) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	var (
		LblElse  *meer.Label
		LblElsif *meer.Label
	)

	stmt.BoolExpr.Accept(v)

	LblThen := meer.NewLabel("if.then")
	LblCont := meer.NewLabel("if.cont")

	// if-then only. No else or elsif paths
	if len(stmt.ElsePath) == 0 && len(stmt.ElseIfBranches) == 0 {
		cond := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), LblThen, LblCont)
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)
	}

	// if-then-elif-else. elif and else paths exist. Create labels for both paths.
	// Set the false path of conditional branch to the elif label
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) > 0 {
		LblElse = meer.NewLabel("if.else")
		LblElsif = meer.NewLabel("elsif")

		cond := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), LblThen, LblElsif)
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)
	}

	// no elsif path. but else path exist
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) == 0 {
		LblElse = meer.NewLabel("if.else")
		cond := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), LblThen, LblElse)
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)

	}

	// at least one elsif path, but no else path
	if len(stmt.ElseIfBranches) > 0 && len(stmt.ElsePath) == 0 {
		LblElsif = meer.NewLabel("elsif")
		cond := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), LblThen, LblElsif)
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)
	}

	// emit code for the 'True' path
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblThen)
	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(LblCont))

	// emit code for the 'elsif' branches
	if len(stmt.ElseIfBranches) > 0 {
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblElsif)

		for i, elif := range stmt.ElseIfBranches {
			LblElifThen := meer.NewLabel(fmt.Sprintf("elif.then.%d", i))
			LblElifElse := meer.NewLabel(fmt.Sprintf("elif.else.%d", i))

			elif.BoolExpr.Accept(v)
			cond := meer.CreateCondBrInst(elif.BoolExpr.MirValue(), LblElifThen, LblElifElse)
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)

			// emit code for the ith, elif branch unconditionally branch to 'cont' BasicBlock
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(LblCont))

			LblElsif = LblElifElse
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblElsif)

			// we've reached the last elif-branch and there is no else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) == 0 {
				v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(LblCont))
			}

			// we've reached the last elif-branch and there exists an else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) > 0 {
				v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(LblElse))
			}
		}
	}

	// emit code for the 'False' path if it exists
	if len(stmt.ElsePath) > 0 {
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblElse)
		for _, s := range stmt.ElsePath {
			s.Accept(v)
		}
		v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(LblCont))
	}

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblCont)
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	assign := meer.CreateAssign(stmt.RValue.MirValue(), stmt.LValue.MirValue())
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, assign)
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	var value meer.Expression
	if stmt.Value != nil {
		stmt.Value.Accept(v)
		value = stmt.Value.MirValue()
	}

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateRet(value))
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var args []meer.Expression

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.MirValue())
	}

	callee := meer.CreateIdent(call.Callee.String())
	proc := meer.CreateProcCall(callee, args)

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, proc)

}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	Body := meer.NewLabel("repeat.body")
	Cont := meer.NewLabel("cont")

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Body)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	stmt.BoolExpr.Accept(v)
	CondBr := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), Body, Cont)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, CondBr)

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Cont)
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	Loop := meer.NewLabel("loop")
	IfThen := meer.NewLabel("if.then")
	IfElse := meer.NewLabel("if.else")
	ContBB := meer.NewLabel("cont")

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Loop)

	stmt.BoolExpr.Accept(v)
	CondBr := meer.CreateCondBrInst(stmt.BoolExpr.MirValue(), IfThen, IfElse)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, CondBr)

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, IfThen)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	Jmp := meer.CreateJmp(Loop)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Jmp)

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, IfElse)
	if len(stmt.ElsIfs) > 0 {
		for i, elif := range stmt.ElsIfs {
			ElifThen := meer.NewLabel(fmt.Sprintf("elif.then.%d", i))
			ElifElse := meer.NewLabel(fmt.Sprintf("elif.else.%d", i))

			elif.BoolExpr.Accept(v)
			CondBr = meer.CreateCondBrInst(elif.BoolExpr.MirValue(), ElifThen, ElifElse)
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, CondBr)

			v.PrgUnit.Inst = append(v.PrgUnit.Inst, ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			Jmp = meer.CreateJmp(Loop)
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, Jmp)

			IfElse = ElifElse
			v.PrgUnit.Inst = append(v.PrgUnit.Inst, IfElse)

		}
	}

	Jmp = meer.CreateJmp(ContBB)

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Jmp, ContBB)
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	Loop := meer.NewLabel("loop")
	Next := meer.NewLabel("next")
	v.loopExitTarget = Next

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Loop)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(Loop))

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, Next)
}

func (v *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	LblBody := meer.NewLabel("body")
	LblCont := meer.NewLabel("cont")

	sym := v.env.Lookup(stmt.CtlVar.Name)
	if sym == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", stmt.CtlVar.Name))
	}

	CtlVar := meer.CreateIdent(stmt.CtlVar.Name)

	stmt.InitVal.Accept(v)
	stmt.FinalVal.Accept(v)
	FinalV := stmt.FinalVal.MirValue()

	assign := meer.CreateAssign(stmt.InitVal.MirValue(), CtlVar)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, assign)

	CondV := meer.CreateCmpInst(meer.Lt, CtlVar, FinalV)
	cond := meer.CreateCondBrInst(CondV, LblBody, LblCont)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)

	// IR-Codegen for loop body
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblBody)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	// update control variable
	var inc meer.Expression
	if stmt.By != nil {
		stmt.By.Accept(v)
		inc = stmt.By.MirValue()
	} else {
		inc = &meer.IntegerConst{Value: 1}
	}

	Val := meer.CreateBinaryOp(meer.Add, CtlVar, inc)
	assign = meer.CreateAssign(Val, CtlVar)

	CondV = meer.CreateCmpInst(meer.Lt, CtlVar, FinalV)
	cond = meer.CreateCondBrInst(CondV, LblBody, LblCont)
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, cond)

	// point the builder which block to go to next
	v.PrgUnit.Inst = append(v.PrgUnit.Inst, LblCont)
}

func (v *Visitor) VisitExitStmt(stmt *ast.ExitStmt) {
	if v.loopExitTarget == nil {
		panic("[internal] some loop statement does not contain an exit statement")
	}

	v.PrgUnit.Inst = append(v.PrgUnit.Inst, meer.CreateJmp(v.loopExitTarget))
}

func (v *Visitor) VisitWithStmt(stmt *ast.WithStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitImport(i *ast.Import) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	//decl.Type.Accept(v)
	for _, id := range decl.IdentList {
		id.Accept(v)
	}
}

func (v *Visitor) VisitConstDecl(decl *ast.ConstDecl) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitTypeDecl(decl *ast.TypeDecl) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcHead(head *ast.ProcHead) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitBasicType(basicType *ast.BasicType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitArrayType(arrayType *ast.ArrayType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitPointerType(pointerType *ast.PointerType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcType(procType *ast.ProcType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitRecordType(recordType *ast.RecordType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitEnumType(enumType *ast.EnumType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitNamedType(namedType *ast.NamedType) {
	//TODO implement me
	panic("implement me")
}
