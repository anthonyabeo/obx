package sema

import (
	"fmt"
	scope2 "github.com/anthonyabeo/obx/src/scope"
	"strconv"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Visitor struct {
	offset int
	err    diagnostics.ErrReporter

	ast    *ast.Oberon
	env    scope2.Scope
	scopes map[string]scope2.Scope
}

func NewVisitor(scopes map[string]scope2.Scope, report diagnostics.ErrReporter) *Visitor {
	return &Visitor{scopes: scopes, err: report}
}

func (v *Visitor) VisitOberon(ob *ast.Oberon) {
	v.ast = ob
	for _, m := range ob.Units() {
		m.Accept(v)
	}
}

func (v *Visitor) VisitModule(m *ast.Module) {
	if m.BName.Name != m.EName.Name {
		msg := fmt.Sprintf("module start name '%s' does not match end name '%s'", m.BName, m.EName)
		v.err.AddError(m.Pos(), msg)
	}

	scp := scope2.NewScope(scope2.Global, m.BName.Name)
	v.scopes[m.BName.Name] = scp
	v.env = scp

	for _, metaParam := range m.MetaParams {
		if metaParam.TyConst != nil {
			metaParam.TyConst.Accept(v)
		}

		switch metaParam.Mode {
		case token.CONST:
		default:
			for _, ty := range metaParam.Ids {
				var tyConst types.Type
				if metaParam.TyConst != nil {
					tyConst = metaParam.TyConst.Type()
				}

				genTy := types.NewGenericType(ty.Name, tyConst)
				v.env.Insert(scope2.NewTypeName(ty.NamePos, ty.Name, genTy, ty.IProps, -1))
			}
		}
	}

	for _, imp := range m.ImportList {
		imp.Accept(v)
	}

	for _, decl := range m.DeclSeq {
		decl.Accept(v)
	}

	for _, stmt := range m.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitDefinition(def *ast.Definition) {
	if def.BName.Name != def.EName.Name {
		msg := fmt.Sprintf("module start name '%s' does not match end name '%s'", def.BName, def.EName)
		v.err.AddError(def.Pos(), msg)
	}

	for _, imp := range def.ImportList {
		imp.Accept(v)
	}

	for _, decl := range def.DeclSeq {
		decl.Accept(v)
	}
}

func (v *Visitor) VisitIdentifier(id *ast.Ident) {
	obj := v.env.Lookup(id.Name)
	if obj == nil {
		v.err.AddError(id.Pos(), fmt.Sprintf("name %v is undecleared", id.Name))
	}

	id.EType = obj.Type()
}

func (v *Visitor) VisitBasicLit(b *ast.BasicLit) {
	switch b.Kind {
	case token.INT:
		b.EType = scope2.Typ[types.Int]
	case token.BYTE:
		b.EType = scope2.Typ[types.Byte]
	case token.INT8:
		b.EType = scope2.Typ[types.Int8]
	case token.INT16:
		b.EType = scope2.Typ[types.Int16]
	case token.INT32:
		b.EType = scope2.Typ[types.Int32]
	case token.INT64:
		b.EType = scope2.Typ[types.Int64]
	case token.REAL:
		b.EType = scope2.Typ[types.Real]
	case token.LONGREAL:
		b.EType = scope2.Typ[types.LReal]
	case token.TRUE, token.FALSE:
		b.EType = scope2.Typ[types.Bool]
	case token.STRING:
		b.EType = scope2.Typ[types.String]
	case token.NIL:
		b.EType = scope2.Typ[types.Nil]
	}
}

func (v *Visitor) VisitSet(s *ast.Set) {
	for _, elem := range s.Elem {
		elem.Accept(v)

		ty := elem.Type().(*types.Basic)
		if ty == nil || ty.Info() != types.IsInteger {
			msg := fmt.Sprintf("elements of set '%s' must be integers. Found '%s' at '%s'",
				s, ty, elem.Pos())
			v.err.AddError(elem.Pos(), msg)
		}
	}

	s.EType = scope2.Typ[types.Set]
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	LTy := expr.Left.Type()
	RTy := expr.Right.Type()

	switch expr.Op {
	case token.PLUS, token.MINUS, token.STAR, token.QUOT:
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if !leftOk || !rightOk {
			msg := fmt.Sprintf("one of the operands of '%s' is not a basic type", expr)
			v.err.AddError(expr.Pos(), msg)

			return
		}

		if left.Info()|types.IsNumeric != types.IsNumeric && left.Info()|types.IsNumeric != types.IsNumeric && left.Info() != types.IsSet {
			msg := fmt.Sprintf("cannot perform operation '%v' with '%s' (of type '%s')", expr.Op, expr.Left, expr.Left.Type())
			v.err.AddError(expr.Pos(), msg)
		}

		if right.Info()|types.IsNumeric != types.IsNumeric && right.Info()|types.IsNumeric != types.IsNumeric && right.Info() != types.IsSet {
			msg := fmt.Sprintf("cannot perform operation '%v' with '%s' (of type '%s')", expr.Op, expr.Right, expr.Right.Type())
			v.err.AddError(expr.Pos(), msg)
		}

		if left.Info() == types.IsSet && right.Info() == types.IsSet {
			expr.EType = scope2.Typ[types.Set]
			return
		}

		if expr.Op == token.QUOT {
			expr.EType = scope2.Typ[types.LReal]
			return
		}

		if expr.EType = left; left.Kind() < right.Kind() {
			expr.EType = right
		}
	case token.DIV, token.MOD:
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if !leftOk || !rightOk {
			msg := fmt.Sprintf("one of the operands of '%s' is not a basic type", expr)
			v.err.AddError(expr.Pos(), msg)

			return
		}

		if left.Info() != types.IsInteger || right.Info() != types.IsInteger {
			msg := fmt.Sprintf("cannot perform operation '%v' on non-integer types, '%v' and '%v'", expr.Op, expr.Left, expr.Right)
			v.err.AddError(expr.Pos(), msg)
		}

		if expr.EType = left; left.Kind() < right.Kind() {
			expr.EType = right
		}
	case token.EQUAL, token.NEQ:
		// apply to the numeric types, as well as enumerations, CHAR, strings, and CHAR arrays
		// containing 0x as a terminator. Also apply to BOOLEAN and SET, as well as to pointer
		// and procedure types (including the value NIL)
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if leftOk && rightOk {
			if left.Info()|types.IsNumeric != types.IsNumeric && left.Info() != types.IsChar &&
				left.Info() != types.IsString && left.Info() != types.IsBoolean && left.Info() != types.IsSet &&
				left.Info() != types.IsNil {

				msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
				v.err.AddError(expr.Left.Pos(), msg)
			}

			if right.Info()|types.IsNumeric != types.IsNumeric && right.Info() != types.IsChar &&
				right.Info() != types.IsString && right.Info() != types.IsBoolean && right.Info() != types.IsSet &&
				right.Info() != types.IsNil {

				msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
				v.err.AddError(expr.Right.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		LTyEnum, LTyEnumOk := LTy.(*types.Enum)
		RTyEnum, RTyEnumOk := RTy.(*types.Enum)
		if LTyEnumOk && RTyEnumOk {
			if !LTyEnum.SameAs(RTyEnum) {
				msg := fmt.Sprintf("cannot compare different enum types, '%s' and '%s'", expr.Left, expr.Right)
				v.err.AddError(expr.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		LTyPtr, LTyPtrOk := LTy.(*types.PtrType)
		RTyPtr, RTyPtrOk := RTy.(*types.PtrType)
		if LTyPtrOk && RTyPtrOk {
			if !v.ptrExt(LTyPtr, RTyPtr) || !v.ptrExt(RTyPtr, LTyPtr) {
				msg := fmt.Sprintf("cannot compare pointer types '%s' and '%s'", expr.Left, expr.Right)
				v.err.AddError(expr.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		LTyProc, LTyProcOk := LTy.(*ProcedureType)
		RTyProc, RTyProcOk := RTy.(*ProcedureType)
		if LTyProcOk && RTyProcOk {
			if !v.sameType(LTyProc.fp.RetType.Type(), RTyProc.fp.RetType.Type()) || !v.paramListMatch(LTyProc.fp, RTyProc.fp) {
				msg := fmt.Sprintf("cannot compare operand '%s' (of type '%s') and operand '%s' (of type '%s')",
					expr.Left, LTyProc, expr.Right, RTyProc)
				v.err.AddError(expr.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		// TODO check for CHAR and WCHAR arrays
	case token.LESS, token.LEQ, token.GREAT, token.GEQ:
		// apply to the numeric types, as well as enumerations, CHAR, strings, and CHAR arrays
		// containing 0x as a terminator.
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if leftOk && rightOk {
			if left.Info()|types.IsNumeric != types.IsNumeric && left.Info() != types.IsChar && left.Info() != types.IsString {
				msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
				v.err.AddError(expr.Left.Pos(), msg)
			}

			if right.Info()|types.IsNumeric != types.IsNumeric && right.Info() != types.IsChar && right.Info() != types.IsString {
				msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
				v.err.AddError(expr.Right.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		LTyEnum, LTyEnumOk := LTy.(*types.Enum)
		RTyEnum, RTyEnumOk := RTy.(*types.Enum)
		if LTyEnumOk && RTyEnumOk {
			if !LTyEnum.SameAs(RTyEnum) {
				msg := fmt.Sprintf("cannot compare different enum types, '%s' and '%s'", expr.Left, expr.Right)
				v.err.AddError(expr.Pos(), msg)
			}

			expr.EType = scope2.Typ[types.Bool]
			return
		}

		// TODO check for CHAR and WCHAR arrays

		expr.EType = scope2.Typ[types.Bool]
	case token.OR, token.AND:
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if !leftOk || !rightOk {
			msg := fmt.Sprintf("one of the operands of '%s' is not a basic type", expr)
			v.err.AddError(expr.Pos(), msg)

			return
		}

		if left.Info() != types.IsBoolean && right.Info() != types.IsBoolean {
			msg := fmt.Sprintf("both operands of a '%v' operation must be boolean", expr.Op)
			v.err.AddError(expr.OpPos, msg)
		}

		expr.EType = scope2.Typ[types.Bool]
	case token.IN:
		left, leftOk := LTy.(*types.Basic)
		right, rightOk := RTy.(*types.Basic)
		if !leftOk || !rightOk {
			msg := fmt.Sprintf("one of the operands of '%s' is not a basic type", expr)
			v.err.AddError(expr.Pos(), msg)

			return
		}

		if left.Info() != types.IsInteger {
			msg := fmt.Sprintf("the key for an IN operation must be an integer")
			v.err.AddError(expr.Left.Pos(), msg)
		}

		if right.Info() != types.IsSet {
			msg := fmt.Sprintf("IN operation expects the a set, got '%v'", expr.Right.Type())
			v.err.AddError(expr.Right.Pos(), msg)
		}

		expr.EType = scope2.Typ[types.Bool]
	case token.IS:
		var a types.Type

		switch ty := LTy.(type) {
		case *Record:
			a = ty
		case *types.PtrType:
			a = ty.UTy.(*Record)
			if a == nil {
				msg := fmt.Sprintf("'%s' must be a (pointer to) record type", expr.Left)
				v.err.AddError(expr.Left.Pos(), msg)
			}
		default:
			msg := fmt.Sprintf("'%s' must be a (pointer to) record type", expr.Left)
			v.err.AddError(expr.Left.Pos(), msg)
		}

		if !v.recordTyExt(a, RTy) {
			msg := fmt.Sprintf("'%s' is not an extension of '%s'", RTy, a)
			v.err.AddError(expr.Pos(), msg)
		}

		expr.EType = scope2.Typ[types.Bool]
	}
}

func (v *Visitor) VisitDesignator(des *ast.Designator) {
	des.QualifiedIdent.Accept(v)
	if des.Selector == nil {
		des.EType = des.QualifiedIdent.Type()
		return
	}

	switch sel := des.Selector.(type) {
	case *ast.DotOp:
		switch ty := des.QualifiedIdent.Type().(type) {
		case *Record:
			sym := ty.fields.Lookup(sel.Field.Name)
			if sym == nil {
				v.err.AddError(sel.Field.Pos(), fmt.Sprintf("'%s' is not a field in '%s'", sel.Field, ty))
			}

			des.EType = sym.Type()
		case *types.PtrType:
			record, ok := ty.UTy.(*Record)
			if !ok {
				msg := fmt.Sprintf("pointer '%s' does not reference a record type", ty)
				v.err.AddError(des.QualifiedIdent.Pos(), msg)
			}

			var sym scope2.Symbol
			sym = record.fields.Lookup(sel.Field.Name)
			if sym != nil {
				des.EType = sym.Type()
				return
			}

			v.err.AddError(sel.Field.Pos(), fmt.Sprintf("'%s' is not a field in '%s'", sel.Field, record))

		default:
			msg := fmt.Sprintf("'%s' must be a (pointer to) record type", des.QualifiedIdent.Type())
			v.err.AddError(des.QualifiedIdent.Pos(), msg)
		}
	case *ast.PtrDref:
		ptr, ok := des.QualifiedIdent.Type().(*types.PtrType)
		if !ok {
			msg := fmt.Sprintf("name '%s' is not defined as an pointer type", des.QualifiedIdent.String())
			v.err.AddError(des.QualifiedIdent.Pos(), msg)
		}

		des.EType = ptr.UTy
	case *ast.IndexOp:
		switch d := des.QualifiedIdent.Type().(type) {
		case *Array:
			des.EType = d.ElemTy
		case *types.PtrType:
			dArr, ok := d.UTy.(*Array)
			if !ok {
				msg := fmt.Sprintf("name '%s' is not defined as an array type", des.QualifiedIdent.String())
				v.err.AddError(des.QualifiedIdent.Pos(), msg)
			}
			des.EType = dArr.ElemTy
		default:
			msg := fmt.Sprintf("name '%s' is not defined as an array type", des.QualifiedIdent.String())
			v.err.AddError(des.QualifiedIdent.Pos(), msg)
		}
	case *ast.TypeGuard:
		var a types.Type

		switch ty := des.QualifiedIdent.Type().(type) {
		case *Record:
			a = ty
		case *types.PtrType:
			a = ty.UTy.(*Record)
			if a == nil {
				msg := fmt.Sprintf("'%s' must be a (pointer to) record type", des.QualifiedIdent)
				v.err.AddError(des.QualifiedIdent.Pos(), msg)
			}
		default:
			msg := fmt.Sprintf("'%s' must be a (pointer to) record type", des.QualifiedIdent)
			v.err.AddError(des.QualifiedIdent.Pos(), msg)
		}

		sel.Ty.Accept(v)

		if !v.recordTyExt(a, sel.Ty.Type()) {
			msg := fmt.Sprintf("'%s' is not an extension of '%s'", sel.Ty.Type(), a)
			v.err.AddError(des.QualifiedIdent.Pos(), msg)
		}

		des.EType = sel.Ty.Type()
	}

	return
}

func (v *Visitor) VisitFuncCall(call *ast.FuncCall) {
	call.Callee.Accept(v)
	for _, param := range call.ActualParams {
		param.Accept(v)
	}

	if call.Callee.Type() == scope2.Typ[types.Invalid] {
		obj := v.env.Lookup(call.Callee.String())
		if obj == nil {
			v.err.AddError(call.Pos(), fmt.Sprintf("unknown procedure '%v'", call.Callee.String()))
		}

		b := obj.(*scope.Builtin)
		v.checkFuncBuiltin(b, call)

		return
	}

	sig, ok := call.Callee.Type().(*types.Signature)
	if !ok {
		v.err.AddError(call.Pos(), fmt.Sprintf("cannot call a non-procedure %v", call.Callee.String()))
	}

	// ensure that the number of arguments matches the number of formal parameters
	if sig.NumParams() != len(call.ActualParams) {
		v.err.AddError(call.Pos(), fmt.Sprintf("not enough arguments to function call '%v'", call.String()))
	}

	// ensure that the ith argument is assignment-compatible with the ith formal parameter
	for i := 0; i < sig.NumParams(); i++ {
		if !v.assignCompat(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
			msg := fmt.Sprintf(
				"argument '%v' (of type '%v') does not match the corresponding parameter type '%v' in function call '%s'",
				call.ActualParams[i], call.ActualParams[i].Type(), sig.Params[i].Type.Type(), call)
			v.err.AddError(call.ActualParams[i].Pos(), msg)
		}
	}

	call.EType = sig.ReturnType().Type()
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	expr.Operand.Accept(v)

	X, _ := expr.Operand.Type().(*types.Basic)

	switch expr.Op {
	case token.PLUS, token.MINUS:
		if X.Info()|types.IsNumeric != types.IsNumeric {
			msg := fmt.Sprintf("'%v' must be numeric type", expr.Operand)
			v.err.AddError(expr.Operand.Pos(), msg)
		}

		expr.EType = X
	case token.NOT:
		if X.Info() != types.IsBoolean {
			msg := fmt.Sprintf("'%v' does not evaluate to Boolean type", expr.Operand)
			v.err.AddError(expr.Operand.Pos(), msg)
		}

		expr.EType = scope2.Typ[types.Bool]
	}
}

func (v *Visitor) VisitQualifiedIdent(id *ast.QualifiedIdent) {
	sym := v.env.Lookup(id.Prefix.String())
	if sym == nil || sym.Kind() != scope2.MOD {
		msg := fmt.Sprintf("'%s' is not recognised as a module", id.Prefix.String())
		v.err.AddError(id.Prefix.Pos(), msg)
	}

	mod := sym.(*scope2.Module)
	decl := mod.Scope.Lookup(id.Sel.Name)
	if decl == nil || decl.Props()&ast.ExportedReadOnly != ast.Exported {
		msg := fmt.Sprintf("'%s' is not declared/exported in module '%s'", id.Sel.Name, id.Prefix.String())
		v.err.AddError(id.Sel.Pos(), msg)
	}

	id.EType = decl.Type()
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope2.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.err.AddError(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElseIfBranches {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != scope2.Typ[types.Bool] {
			msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", elsif.BoolExpr)
			v.err.AddError(elsif.BoolExpr.Pos(), msg)
		}

		for _, s := range elsif.ThenPath {
			s.Accept(v)
		}
	}

	for _, s := range stmt.ElsePath {
		s.Accept(v)
	}
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignmentStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	if !v.assignCompat(stmt.LValue.Type(), stmt.RValue.Type()) {
		v.err.AddError(stmt.AssignPos, fmt.Sprintf("%v and %v are not assignment compatible", stmt.LValue, stmt.RValue))
	}
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope2.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.err.AddError(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope2.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.err.AddError(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElsIfs {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != scope2.Typ[types.Bool] {
			msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", elsif.BoolExpr)
			v.err.AddError(elsif.BoolExpr.Pos(), msg)
		}

		for _, s := range elsif.ThenPath {
			s.Accept(v)
		}
	}
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	if stmt.Value != nil {
		stmt.Value.Accept(v)
	}
}

func (v *Visitor) VisitCaseStmt(stmt *ast.CaseStmt) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitForStmt(stmt *ast.ForStmt) {
	stmt.CtlVar.Accept(v)
	stmt.InitVal.Accept(v)
	stmt.FinalVal.Accept(v)
	if stmt.By != nil {
		stmt.By.Accept(v)
	} else {
		stmt.By = &ast.BasicLit{
			Kind:  token.INT,
			Val:   "1",
			EType: types.NewBasicType(types.Int, types.IsInteger, "integer"),
		}
	}

	// check that the control variable is of integer or enum type
	ctrl, ctrlOk := stmt.CtlVar.Type().(*types.Basic)
	if !ctrlOk || ctrl.Info() != types.IsInteger {
		msg := fmt.Sprintf("for-loop control variable '%v' must be of integer or enum type. It is a '%v' type",
			stmt.CtlVar.Name, stmt.CtlVar.Type())
		v.err.AddError(stmt.CtlVar.NamePos, msg)
	}

	// check that ctrlID is compatible to initValue and finalValue
	if !v.assignCompat(stmt.CtlVar.Type(), stmt.InitVal.Type()) {
		msg := fmt.Sprintf("control variable '%s' (of type '%s'), cannot be initialized with initial value of type '%s'",
			stmt.CtlVar.Name, stmt.CtlVar.Type(), stmt.InitVal.Type())
		v.err.AddError(stmt.CtlVar.NamePos, msg)
	}

	if !v.assignCompat(stmt.CtlVar.Type(), stmt.FinalVal.Type()) {
		msg := fmt.Sprintf("control variable '%s' (of type '%s'), and final loop value '%s' (of type '%s')",
			stmt.CtlVar.Name, stmt.CtlVar.Type(), stmt.FinalVal, stmt.FinalVal.Type())
		v.err.AddError(stmt.CtlVar.NamePos, msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitExitStmt(*ast.ExitStmt) {
	return
}

func (v *Visitor) VisitWithStmt(stmt *ast.WithStmt) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitProcCall(call *ast.ProcedureCall) {
	call.Callee.Accept(v)
	for _, param := range call.ActualParams {
		param.Accept(v)
	}

	if call.Callee.Type() != scope2.Typ[types.Invalid] {
		sig, ok := call.Callee.Type().(*types.Signature)
		if !ok {
			v.err.AddError(call.Pos(), fmt.Sprintf("cannot call a non-procedure %v", call.Callee.String()))
		}

		// ensure that the number of arguments matches the number of formal parameters
		if sig.NumParams() != len(call.ActualParams) {
			v.err.AddError(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
		}

		// ensure that the ith argument is assignment-compatible with the ith formal parameter
		for i := 0; i < sig.NumParams(); i++ {
			if !v.assignCompat(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], sig.Params[i].Name)
				v.err.AddError(call.ActualParams[i].Pos(), msg)
			}
		}
	} else {
		obj := v.env.Lookup(call.Callee.String())
		if obj == nil {
			v.err.AddError(call.Pos(), fmt.Sprintf("unknown procedure '%v'", call.Callee.String()))
		}

		b := obj.(*scope.Builtin)
		v.checkProcBuiltin(b, call)
	}
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcedureDecl) {
	if decl.Head.Rcv == nil {
		sig := types.NewSignature(decl.Head.Rcv, decl.Head.FP)
		obj := v.env.Insert(scope2.NewProcedure(decl.Pos(), decl.Head.Name.Name, sig, decl.Head.Name.Props(), v.offset))
		if obj != nil {
			v.err.AddError(decl.Pos(), fmt.Sprintf("name %s already declared at %v", obj.String(), obj.Pos()))
		} else {
			v.offset += 8
		}
	}

	off := v.offset
	v.offset = 0

	parent := v.env

	// create a new scope to accommodate the symbols in the procedure. Make this new scope
	// the current scope (v.env) so that subsequent insertions of symbols will go into this
	// scope while we are processing the procedure.
	v.env = scope2.NewScope(v.env, decl.Head.Name.Name)

	decl.Head.Accept(v)

	for _, dec := range decl.Body.DeclSeq {
		dec.Accept(v)
	}

	for _, stmt := range decl.Body.StmtSeq {
		stmt.Accept(v)
	}

	// TODO check that all the paths in the body have a return statement that matches the procedure's return type

	// revert the scope and symbol offset back to the parent
	v.env = parent
	v.offset = off
}

func (v *Visitor) VisitVarDecl(decl *ast.VariableDecl) {
	decl.Type.Accept(v)

	for _, ident := range decl.IdentList {
		obj := v.env.Insert(scope2.NewVar(ident.Pos(), ident.Name, decl.Type.Type(), ident.Props(), v.offset))
		if obj != nil {
			v.err.AddError(decl.Pos(), fmt.Sprintf("variable name '%s' already declared at '%v'", obj.String(), obj.Pos()))
		} else {
			v.offset += decl.Type.Type().Width()
		}
	}
}

func (v *Visitor) VisitConstDecl(decl *ast.ConstantDecl) {
	decl.Value.Accept(v)

	obj := v.env.Insert(scope2.NewConst(decl.Name.NamePos, decl.Name.Name, decl.Value.Type(), decl.Name.Props(), decl.Value, v.offset))
	if obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.err.AddError(decl.Pos(), msg)
	} else {
		v.offset += decl.Value.Type().Width()
	}
}

func (v *Visitor) VisitTypeDecl(decl *ast.TypeDecl) {
	decl.DenotedType.Accept(v)

	obj := v.env.Insert(scope2.NewTypeName(decl.Type, decl.Name.Name, decl.DenotedType.Type(), decl.Name.Props(), v.offset))
	if obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.err.AddError(decl.Pos(), msg)
	} else {
		v.offset += decl.DenotedType.Type().Width()
	}
}

func (v *Visitor) VisitNamedType(n *ast.NamedType) {
	n.Name.Accept(v)
	n.EType = n.Name.Type()
}

func (v *Visitor) VisitBasicType(b *ast.BasicType) {
	obj := v.env.Lookup(b.Name())
	if obj == nil {
		msg := fmt.Sprintf("name '%v' is not declared and is not a predeclared type", b.Name())
		v.err.AddError(b.Pos(), msg)
	}

	t, ok := obj.Type().(types.Type)
	if !ok {
		msg := fmt.Sprintf("'%v' is not recognized as a type", b.Name())
		v.err.AddError(b.Pos(), msg)
	}

	b.EType = t
}

func (v *Visitor) VisitArrayType(a *ast.ArrayType) {
	if a.LenList != nil {
		for _, index := range a.LenList.List {
			index.Accept(v)
			i, ok := index.Type().(*types.Basic)
			if !ok {
				msg := fmt.Sprintf("expected the size of array to be basic (integer) type, got '%s'", index.Type())
				v.err.AddError(index.Pos(), msg)
			}

			if i.Info() != types.IsInteger {
				msg := fmt.Sprintf("expected the size of array to be integer-type, got '%s'", index.Type())
				v.err.AddError(index.Pos(), msg)
			}
		}
	}

	a.ElemType.Accept(v)

	a.EType = NewArray(a.ElemType.Type(), a.LenList)
}

func (v *Visitor) VisitProcType(p *ast.ProcedureType) {
	for _, sec := range p.FP.Params {
		sec.Type.Accept(v)

		for _, name := range sec.Names {
			obj := v.env.Lookup(name.Name)
			if obj != nil {
				v.err.AddError(obj.Pos(), fmt.Sprintf("parameter name %s already declared at %v", obj.String(), obj.Pos()))
				continue
			}

			if sec.Mod == token.IN {
				v.env.Insert(scope2.NewConst(name.NamePos, name.Name, sec.Type.Type(), name.Props(), nil, v.offset))
			} else {
				v.env.Insert(scope2.NewVar(name.Pos(), name.Name, sec.Type.Type(), name.Props(), v.offset))
			}

			v.offset += sec.Type.Type().Width()
		}
	}
	p.FP.RetType.Accept(v)

	pTy := &ProcedureType{Proc: p.Proc, fp: p.FP}
	pTy.DeSugarParams()

	p.EType = pTy
}

func (v *Visitor) VisitPointerType(p *ast.PointerType) {
	p.Base.Accept(v)
	if _, isRecordType := p.Base.Type().(*Record); !isRecordType {
		if _, isArrayType := p.Base.Type().(*Array); !isArrayType {
			v.err.AddError(p.Pos(), "pointer base type must be an array or record-type")
		}
	}

	p.EType = &types.PtrType{UTy: p.Base.Type()}
}

func (v *Visitor) VisitRecordType(r *ast.RecordType) {
	var (
		base     *Record
		baseOk   bool
		baseFlds *scope2.RecordSymTable
	)

	// entering a new scope. store the current offset and reset v.offset to be used in the new scope
	off := v.offset
	v.offset = 0

	if r.BaseType != nil {
		r.BaseType.Accept(v)

		switch ty := r.BaseType.Type().(type) {
		case *Record:
			base = ty
		case *types.PtrType:
			base, baseOk = ty.UTy.(*Record)
			if !baseOk {
				msg := fmt.Sprintf("cannot extend from a '%s' which is not a (pointer to) record-type", r.BaseType.Type())
				v.err.AddError(r.BaseType.Pos(), msg)
			}
		default:
			msg := fmt.Sprintf("cannot extend from a '%s' which is not a (pointer to) record-type", r.BaseType.Type())
			v.err.AddError(r.BaseType.Pos(), msg)
		}

		baseFlds = base.fields
	}

	ty := NewRecordType(scope2.NewRecordScope(baseFlds, v.env), base)

	for _, field := range r.Fields {
		field.Type.Accept(v)
		if _, ok := field.Type.Type().(types.Type); !ok {
			msg := fmt.Sprintf("cannot use '%s' as a field type", field.Type)
			v.err.AddError(field.Type.Pos(), msg)
		}

		for _, id := range field.IdList {
			obj := ty.fields.Insert(scope2.NewVar(id.NamePos, id.Name, field.Type.Type(), id.IProps, v.offset))
			if obj != nil {
				msg := fmt.Sprintf("field name '%s' already declared at '%v'", id.Name, obj.Pos())
				v.err.AddError(id.NamePos, msg)
			}
		}
	}

	r.EType = ty

	// restore the previous offset
	v.offset = off
}

func (v *Visitor) VisitEnumType(e *ast.EnumType) {
	variants := make(map[string]int, 0)
	for idx, id := range e.Variants {
		variants[id.Name] = idx
	}
	ty := types.NewEnumType(variants)

	for i, c := range e.Variants {
		value := &ast.BasicLit{
			Kind:  token.INT,
			Val:   strconv.Itoa(i),
			EType: types.NewBasicType(types.Int, types.IsInteger, "integer"),
		}

		obj := v.env.Insert(scope2.NewConst(c.NamePos, c.Name, ty, c.Props(), value, v.offset))
		if obj != nil {
			msg := fmt.Sprintf("name '%s' already declared at '%v'", c.Name, obj.Pos())
			v.err.AddError(c.NamePos, msg)
		} else {
			v.offset += value.EType.Width()
		}
	}

	e.EType = ty
}

func (v *Visitor) VisitProcHead(head *ast.ProcedureHeading) {
	if head.Rcv != nil {
		head.Rcv.Type.Accept(v)

		switch ty := head.Rcv.Type.Type().(type) {
		case *Record:
			sig := types.NewSignature(head.Rcv, head.FP)
			ty.fields.Insert(scope2.NewProcedure(head.Name.NamePos, head.Name.Name, sig, head.Name.IProps, v.offset))
		case *types.PtrType:
			rec, recOk := ty.UTy.(*Record)
			if !recOk {
				msg := fmt.Sprintf("receiver must be a (pointer to) record type, got '%s'", ty)
				v.err.AddError(head.Rcv.Type.Pos(), msg)
			}

			sig := types.NewSignature(head.Rcv, head.FP)
			rec.fields.Insert(scope2.NewProcedure(head.Name.NamePos, head.Name.Name, sig, head.Name.IProps, v.offset))
		default:
			msg := fmt.Sprintf("receiver must be a (pointer to) record type, got '%s'", ty)
			v.err.AddError(head.Rcv.Type.Pos(), msg)
		}

		var sym scope2.Symbol
		if head.Rcv.Mod == token.IN {
			sym = scope2.NewVar(head.Rcv.Var.NamePos, head.Rcv.Var.Name, head.Rcv.Type.Type(), ast.ReadOnly, v.offset)
		} else {
			sym = scope2.NewVar(head.Rcv.Var.NamePos, head.Rcv.Var.Name, head.Rcv.Type.Type(), head.Rcv.Var.IProps, v.offset)
		}

		v.offset += head.Rcv.Type.Type().Width()
		v.env.Insert(sym)

		head.Rcv.Var.EType = head.Rcv.Type.EType
	}

	for _, sec := range head.FP.Params {
		sec.Type.Accept(v)

		for _, name := range sec.Names {
			obj := v.env.Lookup(name.Name)
			if obj != nil {
				v.err.AddError(obj.Pos(), fmt.Sprintf("parameter name %s already declared at %v", obj.String(), obj.Pos()))
				continue
			}

			if sec.Mod == token.IN {
				v.env.Insert(scope2.NewVar(name.Pos(), name.Name, sec.Type.Type(), ast.ReadOnly, v.offset))
			} else {
				v.env.Insert(scope2.NewVar(name.Pos(), name.Name, sec.Type.Type(), name.Props(), v.offset))
			}

			v.offset += sec.Type.Type().Width()
		}
	}

	if head.FP.RetType != nil {
		head.FP.RetType.Accept(v)
	}
}

func (v *Visitor) VisitImport(imp *ast.Import) {
	if len(imp.Meta) > 0 {

	}

	scp := v.scopes[imp.Name.Name]
	v.env.Insert(scope2.NewModule(nil, imp.Alias.Name, scp))
	v.env.Insert(scope2.NewModule(nil, imp.Name.Name, scp))
}

func (v *Visitor) VisitExprRange(rng *ast.ExprRange) {
	rng.Beg.Accept(v)
	rng.Ed.Accept(v)

	b := rng.Beg.Type().(*types.Basic)
	if b == nil || b.Info() != types.IsInteger {
		msg := fmt.Sprintf("start value of range '%s' is not an integer", rng)
		v.err.AddError(rng.Beg.Pos(), msg)
	}

	b = rng.Ed.Type().(*types.Basic)
	if b == nil || b.Info() != types.IsInteger {
		msg := fmt.Sprintf("end value of range '%s' is not an integer", rng)
		v.err.AddError(rng.Ed.Pos(), msg)
	}

	rng.EType = rng.Beg.Type()
}
