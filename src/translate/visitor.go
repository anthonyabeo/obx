package translate

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Visitor struct {
	irSymbolTable map[string]*ir.AllocaInst
	builder       *ir.Builder
	module        *ir.Module

	ast *ast.Oberon
	env *sema.Scope
}

func NewVisitor(ast *ast.Oberon, env *sema.Scope) *Visitor {
	return &Visitor{
		ast:           ast,
		env:           env,
		irSymbolTable: make(map[string]*ir.AllocaInst),
		builder:       ir.NewBuilder(),
	}
}

func (v *Visitor) IRType(t types.Type) ir.Type {
	var ty ir.Type

	switch t := t.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Int8:
			ty = ir.Int8Type
		case types.Int16:
		case types.Int32:
			ty = ir.Int32Type
		case types.Int64:
		}
	default:

	}

	return ty
}

func (v *Visitor) VisitModule(name string) *ir.Module {
	v.module = ir.NewModule(name)

	Main := ir.CreateFunction(
		ir.CreateFunctionType([]ir.Type{}, ir.Int32Type, true),
		ir.Internal,
		"main",
		v.module,
	)

	EntryBB := ir.CreateBasicBlock("entry", Main)
	Main.AddNewBlock("entry", EntryBB, []string{}, []string{})

	v.builder.SetInsertPoint(EntryBB)

	module := v.ast.Program[name]
	for _, decl := range module.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(v)
	}

	v.builder.CreateRet(ir.NewConstantInt(ir.Int32Type, 0, true, ""))

	return v.module
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	alloc, found := v.irSymbolTable[id.Name]
	if !found {
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
	}

	expr.IRValue = instr
}

func (v *Visitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(v)
	d.IRValue = d.QualifiedIdent.Value()
}

func (v *Visitor) VisitFuncCall(call *ast.FuncCall) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	//TODO implement me
	panic("implement me")
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
	}

	val, err := strconv.ParseUint(lit.Val, 10, 64)
	if err != nil {
		panic(fmt.Sprintf("[internal] unable to parse %s", lit.Val))
	}
	lit.IRValue = ir.NewConstantInt(ir.CreateIntegerType(numBits), val, false, "")
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	lv := &LValueVisitor{Visitor{
		irSymbolTable: v.irSymbolTable,
		builder:       v.builder,
		module:        v.module,
		ast:           v.ast,
		env:           v.env,
	}}

	stmt.LValue.Accept(lv)
	stmt.RValue.Accept(v)

	v.builder.CreateStore(stmt.RValue.Value(), stmt.LValue.Value())
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
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
		v.module.GetFunction(call.Dsg.String()), args,
		call.Dsg.String(),
	)
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitExitStmt(stmt *ast.ExitStmt) {
	//TODO implement me
	panic("implement me")
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
	for _, dcl := range decl.IdentList {
		obj := v.env.Lookup(dcl.Name)

		alloc := v.builder.CreateAlloca(v.IRType(decl.Type.Type()), obj.Name())
		v.irSymbolTable[dcl.Name] = alloc
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

func (v *Visitor) VisitReceiver(receiver *ast.Receiver) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcHead(head *ast.ProcHead) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcBody(body *ast.ProcBody) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitFPSection(section *ast.FPSection) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitFormalParams(params *ast.FormalParams) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitDotOp(op *ast.DotOp) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitIndexOp(op *ast.IndexOp) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitTypeGuard(guard *ast.TypeGuard) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitPointerDeref(deref *ast.PointerDeref) {
	//TODO implement me
	panic("implement me")
}
