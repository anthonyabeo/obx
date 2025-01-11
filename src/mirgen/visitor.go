package mirgen

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Visitor struct {
	builder *meer.Builder

	env    scope.Scope
	scopes map[string]scope.Scope

	loopExitTarget *meer.Label
}

func NewVisitor(scopes map[string]scope.Scope) *Visitor {
	return &Visitor{scopes: scopes, builder: meer.NewBuilder()}
}

func (v *Visitor) Translate(ob *ast.Oberon, order []string) *meer.Program {
	program := meer.NewProgram()

	for _, name := range order {
		unit := ob.Units()[name]
		unit.Accept(v)

		pUnit := meer.NewProgramUnit(name)
		pUnit.Inst = append(pUnit.Inst, v.builder.Instr()...)

		program.Units[name] = pUnit
	}

	return program
}

func (v *Visitor) VisitOberon(oberon *ast.Oberon) {}

func (v *Visitor) VisitModule(m *ast.Module) {
	v.env = v.scopes[m.BName.Name]

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

	id.MirExpr = &meer.Ident{Id: id.Name, Ty: meer.TransType(sym.Type())}
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	var instr meer.Expression

	switch expr.Op {
	case token.PLUS:
		instr = v.builder.CreateAdd(expr.Left.MirValue(), expr.Right.MirValue())
	case token.EQUAL:
		instr = v.builder.CreateCmp(meer.Eq, expr.Left.MirValue(), expr.Right.MirValue())
	case token.LESS:
		instr = v.builder.CreateCmp(meer.Lt, expr.Left.MirValue(), expr.Right.MirValue())
	case token.LEQ:
		instr = v.builder.CreateCmp(meer.Le, expr.Left.MirValue(), expr.Right.MirValue())
	case token.GEQ:
		instr = v.builder.CreateCmp(meer.Ge, expr.Left.MirValue(), expr.Right.MirValue())
	case token.GREAT:
		instr = v.builder.CreateCmp(meer.Gt, expr.Left.MirValue(), expr.Right.MirValue())
	case token.NEQ:
		instr = v.builder.CreateCmp(meer.Ne, expr.Left.MirValue(), expr.Right.MirValue())
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
		expr.MirExpr = v.builder.CreateNeg(expr.X.MirValue())
	case token.NOT:
		expr.MirExpr = v.builder.CreateNot(expr.X.MirValue())
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
	var numBits uint

	switch lit.Kind {
	case token.INT:
		numBits = 32
	case token.INT8, token.BYTE:
		numBits = 8
	case token.INT16:
		numBits = 16
	case token.INT32:
		numBits = 32
	case token.INT64:
		numBits = 64
	case token.TRUE, token.FALSE:
		numBits = 1
	}

	val, err := strconv.ParseUint(lit.Val, 10, 64)
	if err != nil {
		panic(fmt.Sprintf("[internal] unable to parse %s", lit.Val))
	}

	lit.MirExpr = meer.CreateIntegerConst(meer.GetIntegerType(numBits), val, false)
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
		v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), LblThen, LblCont)
	}

	// if-then-elif-else. elif and else paths exist. Create labels for both paths.
	// Set the false path of conditional branch to the elif label
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) > 0 {
		LblElse = meer.NewLabel("if.else")
		LblElsif = meer.NewLabel("elsif")

		v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), LblThen, LblElsif)
	}

	// no elsif path. but else path exist
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) == 0 {
		LblElse = meer.NewLabel("if.else")
		v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), LblThen, LblElse)
	}

	// at least one elsif path, but no else path
	if len(stmt.ElseIfBranches) > 0 && len(stmt.ElsePath) == 0 {
		LblElsif = meer.NewLabel("elsif")
		v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), LblThen, LblElsif)
	}

	// emit code for the 'True' path
	v.builder.SetInsertPoint(LblThen)
	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}
	v.builder.CreateJmp(LblCont)

	// emit code for the 'elsif' branches
	if len(stmt.ElseIfBranches) > 0 {
		v.builder.SetInsertPoint(LblElsif)

		for i, elif := range stmt.ElseIfBranches {
			LblElifThen := meer.NewLabel(fmt.Sprintf("elif.then.%d", i))
			LblElifElse := meer.NewLabel(fmt.Sprintf("elif.else.%d", i))

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.MirValue(), LblElifThen, LblElifElse)

			// emit code for the ith, elif branch unconditionally branch to 'cont' BasicBlock
			v.builder.SetInsertPoint(LblElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.builder.CreateJmp(LblCont)

			LblElsif = LblElifElse
			v.builder.SetInsertPoint(LblElsif)

			// we've reached the last elif-branch and there is no else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) == 0 {
				v.builder.CreateJmp(LblCont)
			}

			// we've reached the last elif-branch and there exists an else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) > 0 {
				v.builder.CreateJmp(LblElse)
			}
		}
	}

	// emit code for the 'False' path if it exists
	if len(stmt.ElsePath) > 0 {
		v.builder.SetInsertPoint(LblElse)
		for _, s := range stmt.ElsePath {
			s.Accept(v)
		}
		v.builder.CreateJmp(LblCont)
	}

	v.builder.SetInsertPoint(LblCont)
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	v.builder.CreateAssign(stmt.RValue.MirValue(), stmt.LValue.MirValue())
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	var value meer.Expression
	if stmt.Value != nil {
		stmt.Value.Accept(v)
		value = stmt.Value.MirValue()
	}

	v.builder.CreateRet(value)
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var args []meer.Expression

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.MirValue())
	}

	callee := meer.CreateIdent(call.Callee.String(), nil)

	v.builder.CreateProcCall(callee, args)
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	LblBody := meer.NewLabel("repeat.body")
	LblCont := meer.NewLabel("cont")

	v.builder.CreateJmp(LblBody)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	stmt.BoolExpr.Accept(v)
	v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), LblBody, LblCont)

	v.builder.SetInsertPoint(LblCont)
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	Loop := meer.NewLabel("loop")
	IfThen := meer.NewLabel("if.then")
	IfElse := meer.NewLabel("if.else")
	Cont := meer.NewLabel("cont")

	v.builder.SetInsertPoint(Loop)

	stmt.BoolExpr.Accept(v)
	v.builder.CreateCondBr(stmt.BoolExpr.MirValue(), IfThen, IfElse)

	v.builder.SetInsertPoint(IfThen)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(IfElse)
	if len(stmt.ElsIfs) > 0 {
		for i, elif := range stmt.ElsIfs {
			ElifThen := meer.NewLabel(fmt.Sprintf("elif.then.%d", i))
			ElifElse := meer.NewLabel(fmt.Sprintf("elif.else.%d", i))

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.MirValue(), ElifThen, ElifElse)

			v.builder.SetInsertPoint(ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.builder.CreateJmp(Loop)

			IfElse = ElifElse
			v.builder.SetInsertPoint(IfElse)
		}
	}

	v.builder.CreateJmp(Cont)

	v.builder.SetInsertPoint(Cont)
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	Loop := meer.NewLabel("loop")
	Next := meer.NewLabel("next")
	v.loopExitTarget = Next

	v.builder.SetInsertPoint(Loop)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(Next)
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

	stmt.InitVal.Accept(v)
	stmt.FinalVal.Accept(v)
	FinalV := stmt.FinalVal.MirValue()

	CtlVar := meer.CreateIdent(stmt.CtlVar.Name, meer.TransType(sym.Type()))

	v.builder.CreateAssign(stmt.InitVal.MirValue(), CtlVar)

	CondV := v.builder.CreateCmp(meer.Lt, CtlVar, FinalV)
	v.builder.CreateCondBr(CondV, LblBody, LblCont)

	// IR-Codegen for loop body
	v.builder.SetInsertPoint(LblBody)
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

	Val := v.builder.CreateAdd(CtlVar, inc)
	v.builder.CreateAssign(Val, CtlVar)

	CondV = v.builder.CreateCmp(meer.Lt, CtlVar, FinalV)
	v.builder.CreateCondBr(CondV, LblBody, LblCont)

	// point the builder which block to go to next
	v.builder.SetInsertPoint(LblCont)
}

func (v *Visitor) VisitExitStmt(stmt *ast.ExitStmt) {
	if v.loopExitTarget == nil {
		panic("[internal] some loop statement does not contain an exit statement")
	}

	v.builder.CreateJmp(v.loopExitTarget)
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
	decl.Type.Accept(v)
	for _, id := range decl.IdentList {
		id.Accept(v)
		id.MirExpr.SetType(decl.Type.MirType())
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

func (v *Visitor) VisitBasicType(t *ast.BasicType) {
	var ty meer.Type

	switch t := t.EType.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Int8:
			ty = meer.Int8Type
		case types.Int16:
			ty = meer.Int1Type
		case types.Int32:
			ty = meer.Int32Type
		case types.Int64:
			ty = meer.Int64Type
		}
	default:

	}

	t.MirTy = ty
}

func (v *Visitor) VisitArrayType(ty *ast.ArrayType) {
	ty.ElemType.Accept(v)
	arrType := ty.ElemType.MirType()

	if ty.LenList != nil {
		for i := len(ty.LenList.List); i >= 0; i-- {
			size := ty.LenList.List[i]
			size.Accept(v)

			arrType = meer.CreateArrayType(size.MirValue(), arrType)
		}
	}

	ty.MirTy = arrType
}

func (v *Visitor) VisitPointerType(ptr *ast.PointerType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcType(proc *ast.ProcType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitRecordType(rec *ast.RecordType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitEnumType(enum *ast.EnumType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitNamedType(namedType *ast.NamedType) {
	//TODO implement me
	panic("implement me")
}
