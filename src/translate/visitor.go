package translate

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Visitor struct {
	builder *ir.Builder
	Module  *ir.Module

	env    scope.Scope
	scopes map[string]scope.Scope
}

func NewVisitor(scopes map[string]scope.Scope) *Visitor {
	return &Visitor{
		scopes:  scopes,
		builder: ir.NewBuilder(),
	}
}

func (v *Visitor) VisitOberon(*ast.Oberon) {}

func (v *Visitor) Translate(ob *ast.Oberon, order []string) *ir.Program {
	p := &ir.Program{}

	for _, name := range order {
		unit := ob.Units()[name]
		unit.Accept(v)
		p.AddModule(v.Module)
	}

	return p
}

func (v *Visitor) VisitModule(m *ast.Module) {
	v.env = v.scopes[m.BName.Name]

	v.Module = ir.NewModule(m.BName.Name)

	Main := ir.CreateFunction(
		ir.CreateFunctionType([]ir.Type{}, ir.Int32Type, false),
		ir.Internal,
		"main",
		v.Module,
	)

	var (
		EntryBB = ir.CreateBasicBlock("entry", Main)
		MainBB  = ir.CreateBasicBlock("main", Main)
	)

	v.builder.CFG = Main.CFG()

	v.builder.SetInsertPoint(EntryBB)
	v.builder.CreateBr(MainBB)
	v.builder.CFG.Entry = EntryBB

	v.builder.SetInsertPoint(MainBB)

	for _, decl := range m.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range m.StmtSeq {
		stmt.Accept(v)
	}

	v.builder.CreateRet(ir.NewConstantInt(ir.Int32Type, 0, true, ""))
}

func (v *Visitor) VisitDefinition(def *ast.Definition) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	alloc := v.env.Lookup(id.Name).Alloca()
	if alloc == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.IRValue = v.builder.CreateLoad(alloc.AllocatedTy(), alloc, "")
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	var instr ir.Value

	switch expr.Op {
	case token.PLUS:
		instr = v.builder.CreateAdd(expr.Left.Value(), expr.Right.Value(), "")
	case token.EQUAL:
		instr = v.builder.CreateCmp(ir.Eq, expr.Left.Value(), expr.Right.Value(), "")
	case token.LESS:
		instr = v.builder.CreateCmp(ir.ULe, expr.Left.Value(), expr.Right.Value(), "")
	case token.GEQ:
		instr = v.builder.CreateCmp(ir.UGe, expr.Left.Value(), expr.Right.Value(), "")
	case token.GREAT:
		instr = v.builder.CreateCmp(ir.UGt, expr.Left.Value(), expr.Right.Value(), "")
	case token.NEQ:
		instr = v.builder.CreateCmp(ir.Ne, expr.Left.Value(), expr.Right.Value(), "")
	}

	expr.IRValue = instr
}

func (v *Visitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(v)
	d.IRValue = d.QualifiedIdent.Value()
}

func (v *Visitor) VisitFuncCall(call *ast.FuncCall) {
	var args []ir.Value

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Value())
	}

	F := v.Module.GetFunction(call.Callee.String())
	if F == nil {
		panic(fmt.Sprintf("[internal] undeclared function '%s'", call.Callee.String()))
	}

	fty := F.Type().(*ir.FunctionType)
	call.IRValue = v.builder.CreateCall(fty, F, args, "")
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	expr.X.Accept(v)

	switch expr.Op {
	case token.MINUS:
		expr.IRValue = v.builder.CreateNeg(expr.X.Value(), "")
	case token.NOT:
		expr.IRValue = v.builder.CreateNot(expr.X.Value(), "")
	case token.PLUS:
		expr.IRValue = expr.X.Value()
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
	lit.IRValue = ir.NewConstantInt(ir.GetIntegerType(numBits), val, false, "")
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	var (
		ElseBB  *ir.BasicBlock
		ElsifBB *ir.BasicBlock
	)

	stmt.BoolExpr.Accept(v)

	BB := v.builder.GetInsertBlock()

	ThenBB := ir.CreateBasicBlock("if.then", BB.Parent())
	ContBB := ir.CreateBasicBlock("cont", BB.Parent())

	// if-then only. No else or elsif paths
	if len(stmt.ElsePath) == 0 && len(stmt.ElseIfBranches) == 0 {
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ContBB)
	}

	// if-then-elif-else. elif and else paths exist. Create basic blocks for both paths.
	// Set the false path of conditional branch to the elif basic-block
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) > 0 {
		ElseBB = ir.CreateBasicBlock("if.else", BB.Parent())
		ElsifBB = ir.CreateBasicBlock("elsif", BB.Parent())

		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElsifBB)
	}

	// no elsif path. but else path exist
	if len(stmt.ElsePath) > 0 && len(stmt.ElseIfBranches) == 0 {
		ElseBB = ir.CreateBasicBlock("if.else", BB.Parent())
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElseBB)
	}

	// at least one elsif path, but no else path
	if len(stmt.ElseIfBranches) > 0 && len(stmt.ElsePath) == 0 {
		ElsifBB = ir.CreateBasicBlock("elsif", BB.Parent())
		v.builder.CreateCondBr(stmt.BoolExpr.Value(), ThenBB, ElsifBB)
	}

	// emit code for the 'True' path
	v.builder.SetInsertPoint(ThenBB)
	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}
	ThenBB = v.builder.GetInsertBlock()
	v.builder.CreateBr(ContBB)

	// emit code for the 'elsif' branches
	if len(stmt.ElseIfBranches) > 0 {
		v.builder.SetInsertPoint(ElsifBB)

		for i, elif := range stmt.ElseIfBranches {
			ElifThen := ir.CreateBasicBlock(fmt.Sprintf("elif.then.%d", i), BB.Parent())
			ElifElse := ir.CreateBasicBlock(fmt.Sprintf("elif.else.%d", i), BB.Parent())

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.Value(), ElifThen, ElifElse)

			// emit code for the ith, elif branch unconditionally branch to 'cont' BasicBlock
			v.builder.SetInsertPoint(ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			ElifThen = v.builder.GetInsertBlock()
			v.builder.CreateBr(ContBB)

			ElsifBB = ElifElse
			v.builder.SetInsertPoint(ElsifBB)

			// we've reached the last elif-branch and there is no else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) == 0 {
				v.builder.CreateBr(ContBB)
			}

			// we've reached the last elif-branch and there exists an else-branch
			if (i == len(stmt.ElseIfBranches)-1) && len(stmt.ElsePath) > 0 {
				v.builder.CreateBr(ElseBB)
			}
		}
	}

	// emit code for the 'False' path if it exists
	if len(stmt.ElsePath) > 0 {
		v.builder.SetInsertPoint(ElseBB)
		for _, s := range stmt.ElsePath {
			s.Accept(v)
		}
		ElseBB = v.builder.GetInsertBlock()
		v.builder.CreateBr(ContBB)
	}

	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	av := &AddrVisitor{Visitor{
		builder: v.builder,
		Module:  v.Module,
		env:     v.env,
	}}

	stmt.LValue.Accept(av)
	stmt.RValue.Accept(v)

	v.builder.CreateStore(stmt.RValue.Value(), stmt.LValue.Value())
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	var value ir.Value
	if stmt.Value != nil {
		stmt.Value.Accept(v)
		value = stmt.Value.Value()
	}

	v.builder.CreateRet(value)
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var (
		args  []ir.Value
		fArgs []ir.Type
	)

	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Value())

		fArgs = append(fArgs, arg.Value().Type())
	}

	v.builder.CreateCall(
		ir.CreateFunctionType(fArgs, ir.VoidType, false),
		v.Module.GetFunction(call.Callee.String()), args,
		call.Callee.String(),
	)
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	BB := v.builder.GetInsertBlock()
	BodyBB := ir.CreateBasicBlock("repeat.body", BB.Parent())
	ContBB := ir.CreateBasicBlock("cont", BB.Parent())

	// jump into and start executing the loop body
	v.builder.CreateBr(BodyBB)

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

	Loop := ir.CreateBasicBlock("loop", BB.Parent())
	IfThen := ir.CreateBasicBlock("if.then", BB.Parent())
	IfElse := ir.CreateBasicBlock("if.else", BB.Parent())
	ContBB := ir.CreateBasicBlock("cont", BB.Parent())

	v.builder.CreateBr(Loop)

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
	v.builder.CreateBr(Loop)

	v.builder.SetInsertPoint(IfElse)
	if len(stmt.ElsIfs) > 0 {
		for i, elif := range stmt.ElsIfs {
			ElifThen := ir.CreateBasicBlock(fmt.Sprintf("elif.then.%d", i), BB.Parent())
			ElifElse := ir.CreateBasicBlock(fmt.Sprintf("elif.else.%d", i), BB.Parent())

			elif.BoolExpr.Accept(v)
			v.builder.CreateCondBr(elif.BoolExpr.Value(), ElifThen, ElifElse)

			v.builder.SetInsertPoint(ElifThen)
			for _, s := range elif.ThenPath {
				s.Accept(v)
			}
			v.builder.SetInsertPoint(ElifThen)
			v.builder.CreateBr(Loop)

			IfElse = ElifElse
			v.builder.SetInsertPoint(IfElse)
		}
	}

	v.builder.CreateBr(ContBB)

	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO See VisitExitStmt for an idea
	panic("implement me")
}

func (v *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	BB := v.builder.GetInsertBlock()

	BodyBB := ir.CreateBasicBlock("body", BB.Parent())
	ContBB := ir.CreateBasicBlock("cont", BB.Parent())

	IterVar := v.env.Lookup(stmt.CtlVar.Name).Alloca()
	if IterVar == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", stmt.CtlVar.Name))
	}

	stmt.InitVal.Accept(v)
	stmt.FinalVal.Accept(v)
	FinalV := stmt.FinalVal.Value()

	v.builder.CreateStore(stmt.InitVal.Value(), IterVar)

	CtlVar := v.builder.CreateLoad(IterVar.AllocatedTy(), IterVar, "")
	CondV := v.builder.CreateCmp(ir.SLt, CtlVar, FinalV, "")

	v.builder.CreateCondBr(CondV, BodyBB, ContBB)

	// IR-Codegen for loop body
	v.builder.SetInsertPoint(BodyBB)
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
	v.builder.SetInsertPoint(BodyBB)

	// update control variable
	var inc ir.Value
	if stmt.By != nil {
		stmt.By.Accept(v)
		inc = stmt.By.Value()
	} else {
		inc = ir.NewConstantInt(ir.Int64Type, 1, true, "")
	}

	CtlVar = v.builder.CreateLoad(IterVar.AllocatedTy(), IterVar, "")
	Val := v.builder.CreateAdd(CtlVar, inc, "")
	v.builder.CreateStore(Val, IterVar)

	CondV = v.builder.CreateCmp(ir.SLt, IterVar, FinalV, "")

	v.builder.CreateCondBr(CondV, BodyBB, ContBB)

	// point the builder which block to go to next
	v.builder.SetInsertPoint(ContBB)
}

func (v *Visitor) VisitExitStmt(stmt *ast.ExitStmt) {
	// TODO Possible Implementation
	// --------------------------------
	// Assumption: Each Statement can be decorated with the BasicBlock
	// of the innermost loop it belongs to, if any.
	//
	// Therefore, the ast.ExitStmt can get the parent function of this
	// basic block and find the 'cont' BasicBlock. Then create a branch
	// instruction to this 'cont' block.
}

func (v *Visitor) VisitWithStmt(stmt *ast.WithStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitImport(imp *ast.Import) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	decl.Type.Accept(v)

	for _, dcl := range decl.IdentList {
		obj := v.env.Lookup(dcl.Name)

		alloc := v.builder.CreateAlloca(decl.Type.IRType(), obj.Name())
		obj.SetAlloca(alloc)
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

func (v *Visitor) VisitNamedType(n *ast.NamedType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitBasicType(t *ast.BasicType) {
	var ty ir.Type

	switch t := t.EType.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Int8:
			ty = ir.Int8Type
		case types.Int16:
			ty = ir.Int1Type
		case types.Int32:
			ty = ir.Int32Type
		case types.Int64:
			ty = ir.Int64Type
		}
	default:

	}

	t.IRTy = ty
}

func (v *Visitor) VisitArrayType(ty *ast.ArrayType) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitPointerType(ty *ast.PointerType) {
	ty.Base.Accept(v)
	ty.IRTy = ir.CreatePointerType(ty.Base.IRType())
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

func (v *Visitor) VisitProcHead(head *ast.ProcHead) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitExprRange(rng *ast.ExprRange) {
	//TODO implement me
	panic("implement me")
}
