package translate

import (
	"strconv"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/translate/ir"
)

type Visitor struct {
	tmp   int
	Instr []ir.Instruction

	ast *ast.Oberon
	env *sema.Scope
}

func NewVisitor(ast *ast.Oberon, env *sema.Scope) *Visitor {
	return &Visitor{ast: ast, env: env}
}

func (v *Visitor) nextTemp() string {
	tmp := strconv.Itoa(v.tmp)
	v.tmp += 1

	return tmp
}

func (v *Visitor) VisitModule(name string) {
	module := v.ast.Program[name]

	//for _, decl := range module.DeclSeq {
	//	decl.Accept(v)
	//}

	for _, stmt := range module.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	sym := v.env.Lookup(id.Name)
	id.IOperand = ir.Register{
		Name:   sym.Name(),
		Offset: sym.Offset(),
		Type:   sym.Type(),
		//Attr:   sym.Props(),
		OpKind: ir.KRegister,
	}
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	tmp := v.nextTemp()

	var instr ir.Instruction

	switch expr.Op {
	case token.PLUS:
		//src := []ir.Operand{expr.Left.Operand(), expr.Right.Operand()}
		//dst := []ir.Operand{ir.Register{Name: tmp, OpKind: ir.KRegister}}

		//instr = ir.CreateNormalInstr(ir.Add, src, dst)
		//instr = ir.BinaryOp{
		//	Left:   expr.Left.Operand(),
		//	Right:  expr.Right.Operand(),
		//	Result: ir.Register{Name: tmp, OpKind: ir.KRegister},
		//}
		instr = ir.CreateAdd(expr.Left.Operand(), expr.Right.Operand(), tmp)
	case token.EQUAL:
		//instr = ir.CmpInstr{
		//	Cond:   ir.Eq,
		//	X:      expr.Left.Operand(),
		//	Y:      expr.Right.Operand(),
		//	Result: ir.Register{Name: tmp, OpKind: ir.KRegister},
		//}
		instr = ir.CreateCmp(ir.Eq, expr.Left.Operand(), expr.Right.Operand(), tmp)
	}

	expr.IOperand = ir.Register{Name: tmp, OpKind: ir.KRegister}
	v.Instr = append(v.Instr, instr)
}

func (v *Visitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(v)
	d.IOperand = d.QualifiedIdent.Operand()
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
	switch lit.Kind {
	case token.INT, token.INT8, token.INT16, token.INT32, token.INT64, token.BYTE:
		lit.IOperand = ir.Number{
			Value:  lit.Value,
			OpKind: ir.KNumber,
		}
	}
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	//src := []ir.Operand{stmt.RValue.Operand()}
	//dst := []ir.Operand{stmt.LValue.Operand()}

	//v.Instr = append(v.Instr, ir.CreateNormalInstr(ir.Load, src, dst))
	v.Instr = append(v.Instr, ir.CreateStore(stmt.RValue.Operand(), stmt.LValue.Operand()))
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	var args []ir.Operand
	for _, arg := range call.ActualParams {
		arg.Accept(v)
		args = append(args, arg.Operand())

	}

	v.Instr = append(v.Instr, ir.CallInstr{
		Result: ir.Register{Name: v.nextTemp(), OpKind: ir.KRegister},
		Proc:   call.Dsg.String(),
		Args:   args,
	})
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
	//TODO implement me
	panic("implement me")
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
