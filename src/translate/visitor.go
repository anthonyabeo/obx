package translate

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

type Visitor struct {
	builder *tacil.Builder
	module  *tacil.Module

	env    scope.Scope
	scopes map[string]scope.Scope

	symbols *tacil.SymbolTable

	loopExitTarget *tacil.BasicBlock
}

func NewVisitor(scopes map[string]scope.Scope) *Visitor {
	return &Visitor{
		scopes:  scopes,
		symbols: tacil.NewScope(nil, ""),
		builder: tacil.NewBuilder(),
	}
}

func (v *Visitor) Module() *tacil.Module {
	return v.module
}

func (v *Visitor) SymbolTable() *tacil.SymbolTable {
	return v.symbols
}

func (v *Visitor) VisitOberon(*ast.Oberon) {}

func (v *Visitor) Translate(ob *ast.Oberon, order []string) *tacil.Program {
	p := &tacil.Program{}

	for _, name := range order {
		unit := ob.Units()[name]
		unit.Accept(v)
		p.AddModule(v.module)
	}

	return p
}

func (v *Visitor) VisitModule(m *ast.Module) {
	v.env = v.scopes[m.BName.Name]

	v.module = tacil.NewModule(m.BName.Name)

	Main := tacil.CreateFunction(
		tacil.CreateFunctionType([]tacil.Type{}, tacil.Int32Type, false),
		tacil.Internal,
		"main",
		v.module,
	)

	var (
		EntryBB = tacil.CreateBasicBlock("entry", Main)
		MainBB  = tacil.CreateBasicBlock("main", Main)
	)

	v.builder.CFG = Main.CFG()

	v.builder.SetInsertPoint(EntryBB)
	v.builder.CreateJmp(MainBB)
	v.builder.CFG.Entry = EntryBB

	v.builder.SetInsertPoint(MainBB)

	for _, decl := range m.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range m.StmtSeq {
		stmt.Accept(v)
	}

	v.builder.CreateRet(tacil.NewConstantInt(tacil.Int32Type, 0, true))
}

func (v *Visitor) VisitDefinition(def *ast.Definition) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	obj := v.symbols.Lookup(id.Name)
	if obj == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.IRExpr = tacil.NewTemp(id.Name, obj.Type())
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	var instr tacil.Expr

	switch expr.Op {
	case token.PLUS:
		instr = v.builder.CreateAdd(expr.Left.Value(), expr.Right.Value())
	case token.EQUAL:
		instr = v.builder.CreateCmp(tacil.Eq, expr.Left.Value(), expr.Right.Value())
	case token.LESS:
		instr = v.builder.CreateCmp(tacil.Lt, expr.Left.Value(), expr.Right.Value())
	case token.LEQ:
		instr = v.builder.CreateCmp(tacil.Le, expr.Left.Value(), expr.Right.Value())
	case token.GEQ:
		instr = v.builder.CreateCmp(tacil.Ge, expr.Left.Value(), expr.Right.Value())
	case token.GREAT:
		instr = v.builder.CreateCmp(tacil.Gt, expr.Left.Value(), expr.Right.Value())
	case token.NEQ:
		instr = v.builder.CreateCmp(tacil.Ne, expr.Left.Value(), expr.Right.Value())
	}

	expr.IRExpr = instr
}

func (v *Visitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(v)
	if d.Selector == nil {
		d.IRExpr = d.QualifiedIdent.Value()
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
	var args []tacil.Expr

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Value())
	}

	F := v.module.GetFunction(call.Callee.String())
	if F == nil {
		panic(fmt.Sprintf("[internal] undeclared function '%s'", call.Callee.String()))
	}

	callee := tacil.NewTemp(call.Callee.String(), F.Type())
	call.IRExpr = v.builder.CreateFuncCall(callee, args)
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	expr.X.Accept(v)

	switch expr.Op {
	case token.MINUS:
		expr.IRExpr = v.builder.CreateNeg(expr.X.Value())
	case token.NOT:
		expr.IRExpr = v.builder.CreateNot(expr.X.Value())
	case token.PLUS:
		expr.IRExpr = expr.X.Value()
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
	lit.IRExpr = tacil.NewConstantInt(tacil.GetIntegerType(numBits), val, false)
}

func (v *Visitor) VisitExprRange(exprRange *ast.ExprRange) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	var (
		ElseBB  *tacil.BasicBlock
		ElsifBB *tacil.BasicBlock
	)

	stmt.BoolExpr.Accept(v)

	BB := v.builder.GetInsertBlock()

	ThenBB := tacil.CreateBasicBlock("if.then", BB.Parent())
	ContBB := tacil.CreateBasicBlock("if.cont", BB.Parent())

	// if-then only. No else or elsif paths
	if len(stmt.ElsePath) == 0 && len(stmt.ElseIfBranches) == 0 {
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ContBB)
	}

	// if-then-elif-else. elif and else paths exist. Create basic blocks for both paths.
	// Set the false path of conditional branch to the elif basic-block
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) > 0 {
		ElseBB = tacil.CreateBasicBlock("if.else", BB.Parent())
		ElsifBB = tacil.CreateBasicBlock("elsif", BB.Parent())

		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElsifBB)
	}

	// no elsif path. but else path exist
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) == 0 {
		ElseBB = tacil.CreateBasicBlock("if.else", BB.Parent())
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElseBB)
	}

	// at least one elsif path, but no else path
	if len(stmt.ElseIfBranches) > 0 && len(stmt.ElsePath) == 0 {
		ElsifBB = tacil.CreateBasicBlock("elsif", BB.Parent())
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElsifBB)
	}

	// emit code for the 'True' path
	v.builder.SetInsertPoint(ThenBB)
	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}
	ThenBB = v.builder.GetInsertBlock()
	v.builder.CreateJmp(ContBB)

	// emit code for the 'elsif' branches
	if len(stmt.ElseIfBranches) > 0 {
		v.builder.SetInsertPoint(ElsifBB)

		for i, elif := range stmt.ElseIfBranches {
			ElifThen := tacil.CreateBasicBlock(fmt.Sprintf("elif.then.%d", i), BB.Parent())
			ElifElse := tacil.CreateBasicBlock(fmt.Sprintf("elif.else.%d", i), BB.Parent())

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.Value(), ElifThen, ElifElse)

			// emit code for the ith, elif branch unconditionally branch to 'cont' BasicBlock
			v.builder.SetInsertPoint(ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.builder.SetInsertPoint(ElifThen)
			v.builder.CreateJmp(ContBB)

			ElsifBB = ElifElse
			v.builder.SetInsertPoint(ElsifBB)

			// we've reached the last elif-branch and there is no else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) == 0 {
				v.builder.CreateJmp(ContBB)
			}

			// we've reached the last elif-branch and there exists an else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) > 0 {
				v.builder.CreateJmp(ElseBB)
			}
		}
	}

	// emit code for the 'False' path if it exists
	if len(stmt.ElsePath) > 0 {
		v.builder.SetInsertPoint(ElseBB)
		for _, s := range stmt.ElsePath {
			s.Accept(v)
		}
		v.builder.SetInsertPoint(ElseBB)
		v.builder.CreateJmp(ContBB)
	}

	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	av := &AddrVisitor{Visitor{
		builder: v.builder,
		module:  v.module,
		env:     v.env,
		symbols: v.symbols,
	}}

	stmt.LValue.Accept(av)
	stmt.RValue.Accept(v)

	v.builder.CreateAssign(stmt.RValue.Value(), stmt.LValue.Value())
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	var value tacil.Expr
	if stmt.Value != nil {
		stmt.Value.Accept(v)
		value = stmt.Value.Value()
	}

	v.builder.CreateRet(value)
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var args []tacil.Expr

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Value())
	}

	Proc := v.module.GetFunction(call.Callee.String())
	if Proc == nil {
		panic(fmt.Sprintf("[internal] undeclared procedure '%s'", call.Callee.String()))
	}

	callee := tacil.NewTemp(call.Callee.String(), Proc.Type())
	v.builder.CreateProcCall(callee, args)
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	BB := v.builder.GetInsertBlock()
	BodyBB := tacil.CreateBasicBlock("repeat.body", BB.Parent())
	ContBB := tacil.CreateBasicBlock("cont", BB.Parent())

	// jump into and start executing the loop body
	v.builder.CreateJmp(BodyBB)

	// generate code for the body under the 'repeat.body' label
	v.builder.SetInsertPoint(BodyBB)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.SetInsertPoint(BodyBB)

	// generate conditional branch to regulate loop
	stmt.BoolExpr.Accept(v)
	v.builder.CreateCondBr(stmt.BoolExpr.Value(), BodyBB, ContBB)

	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	BB := v.builder.GetInsertBlock()

	Loop := tacil.CreateBasicBlock("loop", BB.Parent())
	IfThen := tacil.CreateBasicBlock("if.then", BB.Parent())
	IfElse := tacil.CreateBasicBlock("if.else", BB.Parent())
	ContBB := tacil.CreateBasicBlock("cont", BB.Parent())

	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(Loop)
	stmt.BoolExpr.Accept(v)
	v.builder.CreateCondBr(stmt.BoolExpr.Value(), IfThen, IfElse)

	// generate code for the 'True' path. The process may change the insert basic block.
	// so we should set it back to IfThen before creating the unconditional branch instruction
	v.builder.SetInsertPoint(IfThen)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.SetInsertPoint(IfThen)
	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(IfElse)
	if len(stmt.ElsIfs) > 0 {
		for i, elif := range stmt.ElsIfs {
			ElifThen := tacil.CreateBasicBlock(fmt.Sprintf("elif.then.%d", i), BB.Parent())
			ElifElse := tacil.CreateBasicBlock(fmt.Sprintf("elif.else.%d", i), BB.Parent())

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.Value(), ElifThen, ElifElse)

			v.builder.SetInsertPoint(ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.builder.SetInsertPoint(ElifThen)
			v.builder.CreateJmp(Loop)

			IfElse = ElifElse
			v.builder.SetInsertPoint(IfElse)
		}
	}

	v.builder.CreateJmp(ContBB)

	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	BB := v.builder.GetInsertBlock()

	Loop := tacil.CreateBasicBlock("loop", BB.Parent())
	NextBB := tacil.CreateBasicBlock("next", BB.Parent())
	v.loopExitTarget = NextBB

	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(Loop)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.CreateJmp(Loop)

	v.builder.SetInsertPoint(NextBB)
}

func (v *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	BB := v.builder.GetInsertBlock()

	BodyBB := tacil.CreateBasicBlock("body", BB.Parent())
	ContBB := tacil.CreateBasicBlock("cont", BB.Parent())

	sym := v.symbols.Lookup(stmt.CtlVar.Name)
	if sym == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", stmt.CtlVar.Name))
	}

	CtlVar := tacil.NewTemp(stmt.CtlVar.Name, sym.Type())

	stmt.InitVal.Accept(v)
	stmt.FinalVal.Accept(v)
	FinalV := stmt.FinalVal.Value()

	v.builder.CreateAssign(stmt.InitVal.Value(), CtlVar)

	CondV := v.builder.CreateCmp(tacil.Lt, CtlVar, FinalV)
	v.builder.CreateCondBr(CondV, BodyBB, ContBB)

	// IR-Codegen for loop body
	v.builder.SetInsertPoint(BodyBB)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.SetInsertPoint(BodyBB)

	// update control variable
	var inc tacil.Expr
	if stmt.By != nil {
		stmt.By.Accept(v)
		inc = stmt.By.Value()
	} else {
		inc = tacil.NewConstantInt(tacil.Int64Type, 1, true)
	}

	Val := v.builder.CreateAdd(CtlVar, inc)
	v.builder.CreateAssign(Val, CtlVar)

	CondV = v.builder.CreateCmp(tacil.Lt, CtlVar, FinalV)
	v.builder.CreateCondBr(CondV, BodyBB, ContBB)

	// point the builder which block to go to next
	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitExitStmt(*ast.ExitStmt) {
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
		if sym := v.env.Lookup(id.Name); sym != nil {

			obj := tacil.CreateVariableObject(id.Name, decl.Type.IRType(), sym.Offset(), tacil.Var)
			v.symbols.Insert(obj)
		}
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
	var ty tacil.Type

	switch t := t.EType.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Int8:
			ty = tacil.Int8Type
		case types.Int16:
			ty = tacil.Int1Type
		case types.Int32:
			ty = tacil.Int32Type
		case types.Int64:
			ty = tacil.Int64Type
		}
	default:

	}

	t.IRTy = ty
}

func (v *Visitor) VisitArrayType(ty *ast.ArrayType) {
	ty.ElemType.Accept(v)
	arrType := ty.ElemType.IRType()

	if ty.LenList != nil {
		for i := len(ty.LenList.List); i >= 0; i-- {
			size := ty.LenList.List[i]
			size.Accept(v)

			arrType = tacil.CreateArrayType(size.Value(), arrType)
		}
	}

	ty.IRTy = arrType
}

func (v *Visitor) VisitPointerType(ty *ast.PointerType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcType(ty *ast.ProcType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitRecordType(ty *ast.RecordType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitEnumType(ty *ast.EnumType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitNamedType(ty *ast.NamedType) {
	//TODO implement me
	panic("implement me")
}
