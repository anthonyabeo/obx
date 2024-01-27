package translate

import (
	"strconv"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Visitor struct {
	tmp           int
	Instr         []ir.Instruction
	irSymbolTable map[string]*ir.AllocaInst

	ast *ast.Oberon
	env *sema.Scope
}

func NewVisitor(ast *ast.Oberon, env *sema.Scope) *Visitor {
	return &Visitor{ast: ast, env: env, irSymbolTable: make(map[string]*ir.AllocaInst)}
}

func (v *Visitor) nextTemp() string {
	tmp := strconv.Itoa(v.tmp)
	v.tmp += 1

	return tmp
}

func (v *Visitor) getLLVMType(t types.Type) ir.Type {
	var ty ir.Type

	switch t := t.(type) {
	case *types.Basic:
		switch t.Kind() {
		case types.Int8:
		case types.Int16:
		case types.Int32:
			ty = ir.CreateIntegerType(32)
		case types.Int64:
		}
	default:

	}

	return ty
}

func (v *Visitor) VisitModule(name string) {
	module := v.ast.Program[name]

	for _, decl := range module.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	alloc, found := v.irSymbolTable[id.Name]
	if !found {
		panic("")
	}

	id.IRValue = alloc
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	var instr ir.Instruction

	switch expr.Op {
	case token.PLUS:
		instr = ir.CreateAdd(v.getLLVMType(expr.EType), expr.Left.Value(), expr.Right.Value(), "")
	case token.EQUAL:
		instr = ir.CreateCmp(expr.Left.Value().Type(), ir.Eq, expr.Left.Value(), expr.Right.Value(), "")
	}

	expr.IRValue = instr
	v.Instr = append(v.Instr, instr)
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
		// TODO handle this error
	}
	lit.IRValue = ir.NewConstantInt(ir.CreateIntegerType(numBits), numBits, val, false, "")
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	v.Instr = append(v.Instr, ir.CreateStore(stmt.RValue.Value(), stmt.LValue.Value()))
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var args []ir.Value
	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Value())

	}

	v.Instr = append(v.Instr, ir.CreateCall(ir.GetVoidType(), call.Dsg.String(), args, ""))
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

		alloc := ir.CreateAlloca(v.getLLVMType(decl.Type.Type()), 1, 4, obj.Name())
		v.irSymbolTable[dcl.Name] = alloc
		v.Instr = append(v.Instr, alloc)
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
