package sema

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Visitor struct {
	offset int
	errors lexer.ErrorList

	ast *ast.Oberon
	env *scope.Scope
}

func (v *Visitor) InitSemaVisitor(ast *ast.Oberon, env *scope.Scope) {
	v.ast = ast
	v.env = env
}

func (v *Visitor) error(pos *token.Position, msg string) {
	n := len(v.errors)
	if n > 10 {
		for _, err := range v.errors {
			println(err.Error())
		}

		panic("too many errors")
	}

	v.errors.Append(pos, msg)
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
	obj := v.env.Lookup(id.Name)
	if obj == nil {
		v.error(id.Pos(), fmt.Sprintf("name %v is undecleared", id.Name))
	}

	id.EType = obj.Type()
}

func (v *Visitor) VisitBasicLit(b *ast.BasicLit) {
	switch b.Kind {
	case token.INT:
		b.EType = scope.Typ[types.Int]
	case token.BYTE:
		b.EType = scope.Typ[types.Byte]
	case token.INT8:
		b.EType = scope.Typ[types.Int8]
	case token.INT16:
		b.EType = scope.Typ[types.Int16]
	case token.INT32:
		b.EType = scope.Typ[types.Int32]
	case token.INT64:
		b.EType = scope.Typ[types.Int64]
	case token.REAL:
		b.EType = scope.Typ[types.Real]
	case token.LONGREAL:
		b.EType = scope.Typ[types.LReal]
	case token.TRUE, token.FALSE:
		b.EType = scope.Typ[types.Bool]
	}
}

func (v *Visitor) VisitSet(s *ast.Set) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	// TODO the operands could be non-basic types.
	left, _ := expr.Left.Type().(*types.Basic)
	right, _ := expr.Right.Type().(*types.Basic)

	switch expr.Op {
	case token.PLUS, token.MINUS, token.STAR, token.QUOT:
		if left.Info()|types.IsNumeric != types.IsNumeric || right.Info()|types.IsNumeric != types.IsNumeric {
			msg := fmt.Sprintf("cannot perform operation '%v' on non-numeric types, '%v' and '%v'", expr.Op, expr.Left, expr.Right)
			v.error(expr.Pos(), msg)
		}

		if expr.Op == token.QUOT {
			expr.EType = scope.Typ[types.LReal]
			return
		}

		if expr.EType = left; left.Kind() < right.Kind() {
			expr.EType = right
		}

	case token.DIV, token.MOD:
		if left.Info() != types.IsInteger || right.Info() != types.IsInteger {
			msg := fmt.Sprintf("cannot perform operation '%v' on non-integer types, '%v' and '%v'", expr.Op, expr.Left, expr.Right)
			v.error(expr.Pos(), msg)
		}

		if expr.EType = left; left.Kind() < right.Kind() {
			expr.EType = right
		}
	case token.EQUAL, token.NEQ:
		if left.Info()|types.IsNumeric != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray && IsBool && IsSet && IsPointer && IsProc && IsNil */ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
			v.error(expr.Left.Pos(), msg)
		}

		if right.Info()|types.IsNumeric != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray & IsBool && IsSet && IsPointer && IsProc && IsNil */ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
			v.error(expr.Right.Pos(), msg)
		}

		expr.EType = scope.Typ[types.Bool]
	case token.LESS, token.LEQ, token.GREAT, token.GEQ:
		if left.Info()|types.IsNumeric != types.IsNumeric /*&& left.Info() != types.IsEnum*/ && left.Info() != types.IsChar && left.Info() != types.IsString /*&& IsCharArray */ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
			v.error(expr.Left.Pos(), msg)
		}

		if right.Info()|types.IsNumeric != types.IsNumeric && right.Info() != types.IsChar && right.Info() != types.IsString /* && IsEnum && IsCharArray*/ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
			v.error(expr.Right.Pos(), msg)
		}

		expr.EType = scope.Typ[types.Bool]
	case token.OR, token.AND:
		if left.Info() != types.IsBoolean && right.Info() != types.IsBoolean {
			msg := fmt.Sprintf("both operands of a '%v' operation must be boolean", expr.Op)
			v.error(expr.OpPos, msg)
		}

		expr.EType = scope.Typ[types.Bool]
	case token.IN:
		if left.Info() != types.IsInteger {
			msg := fmt.Sprintf("the key for an IN operation must be an integer")
			v.error(expr.Left.Pos(), msg)
		}

		if right.Info() != types.IsSet {
			msg := fmt.Sprintf("IN operation expects the a set, got '%v'", expr.Right.Type())
			v.error(expr.Right.Pos(), msg)
		}

		expr.EType = scope.Typ[types.Bool]
	case token.IS:
		expr.EType = scope.Typ[types.Bool]
	}
}

func (v *Visitor) VisitDesignator(des *ast.Designator) {
	des.QualifiedIdent.Accept(v)
	if des.Selector != nil {
		des.Selector.Accept(v)
		des.EType = des.Selector.Type()
		return
	}

	des.EType = des.QualifiedIdent.Type()
}

func (v *Visitor) VisitFuncCall(call *ast.FuncCall) {
	call.Dsg.Accept(v)
	for _, param := range call.ActualParams {
		param.Accept(v)
	}

	sig, ok := call.Dsg.Type().(*ast.Signature)
	if !ok {
		v.error(call.Pos(), fmt.Sprintf("cannot call a non-procedure %v", call.Dsg.String()))
	}

	// ensure that the number of arguments matches the number of formal parameters
	if sig.NumParams() != len(call.ActualParams) {
		v.error(call.Pos(), fmt.Sprintf("not enough arguments to function call '%v'", call.String()))
	}

	// ensure that the ith argument is assignment-compatible with the ith formal parameter
	for i := 0; i < sig.NumParams(); i++ {
		if !v.assignCompat(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
			msg := fmt.Sprintf(
				"argument '%v' (of type '%v') does not match the corresponding parameter type '%v' in function call '%s'",
				call.ActualParams[i], call.ActualParams[i].Type(), sig.Params[i].Type.Type(), call)
			v.error(call.ActualParams[i].Pos(), msg)
		}
	}

	call.EType = sig.ReturnType().Type()
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	expr.X.Accept(v)

	X, _ := expr.X.Type().(*types.Basic)

	switch expr.Op {
	case token.PLUS, token.MINUS:
		if X.Info()|types.IsNumeric != types.IsNumeric {
			msg := fmt.Sprintf("'%v' must be numeric type", expr.X)
			v.error(expr.X.Pos(), msg)
		}

		expr.EType = X
	case token.NOT:
		if X.Info() != types.IsBoolean {
			msg := fmt.Sprintf("'%v' does not evaluate to Boolean type", expr.X)
			v.error(expr.X.Pos(), msg)
		}

		expr.EType = scope.Typ[types.Bool]
	}
}

func (v *Visitor) VisitQualifiedIdent(id *ast.QualifiedIdent) {
	id.X.Accept(v)
	if id.Sel != nil {
		id.Sel.Accept(v)
		id.EType = id.Sel.Type()
		return
	}

	id.EType = id.X.Type()
}

func (v *Visitor) VisitIfStmt(stmt *ast.IfStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElseIfBranches {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != scope.Typ[types.Bool] {
			msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", elsif.BoolExpr)
			v.error(elsif.BoolExpr.Pos(), msg)
		}

		for _, s := range elsif.ThenPath {
			s.Accept(v)
		}
	}

	for _, s := range stmt.ElsePath {
		s.Accept(v)
	}
}

func (v *Visitor) VisitAssignStmt(stmt *ast.AssignStmt) {
	stmt.LValue.Accept(v)
	stmt.RValue.Accept(v)

	if !v.assignCompat(stmt.LValue.Type(), stmt.RValue.Type()) {
		v.error(stmt.AssignPos, fmt.Sprintf("%v and %v are not assignment compatible", stmt.LValue, stmt.RValue))
	}
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != scope.Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElsIfs {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != scope.Typ[types.Bool] {
			msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", elsif.BoolExpr)
			v.error(elsif.BoolExpr.Pos(), msg)
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

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	// check that the control variable is of integer or enum type
	if stmt.CtlVar.Type() != scope.Typ[types.Int] {
		msg := fmt.Sprintf("for-loop control variable '%v' must be of integer or enum type. It is a '%v' type",
			stmt.CtlVar.Name, stmt.CtlVar.Type())
		v.error(stmt.CtlVar.NamePos, msg)
	}

	// check that ctrlID is compatible to initValue and finalValue
	if !v.assignCompat(stmt.CtlVar.Type(), stmt.InitVal.Type()) {
		msg := fmt.Sprintf("control variable '%s' (of type '%s'), cannot be initialized with initial value of type '%s'",
			stmt.CtlVar.Name, stmt.CtlVar.Type(), stmt.InitVal.Type())
		v.error(stmt.CtlVar.NamePos, msg)
	}

	if !v.assignCompat(stmt.CtlVar.Type(), stmt.FinalVal.Type()) {
		msg := fmt.Sprintf("control variable '%s' (of type '%s'), and final loop value '%s' (of type '%s')",
			stmt.CtlVar.Name, stmt.CtlVar.Type(), stmt.FinalVal, stmt.FinalVal.Type())
		v.error(stmt.CtlVar.NamePos, msg)
	}
}

func (v *Visitor) VisitExitStmt(*ast.ExitStmt) {
	return
}

func (v *Visitor) VisitWithStmt(stmt *ast.WithStmt) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	call.Dsg.Accept(v)
	for _, param := range call.ActualParams {
		param.Accept(v)
	}

	if call.Dsg.Type() != scope.Typ[types.Invalid] {
		sig, ok := call.Dsg.Type().(*ast.Signature)
		if !ok {
			v.error(call.Pos(), fmt.Sprintf("cannot call a non-procedure %v", call.Dsg.String()))
		}

		// ensure that the number of arguments matches the number of formal parameters
		if sig.NumParams() != len(call.ActualParams) {
			v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
		}

		// ensure that the ith argument is assignment-compatible with the ith formal parameter
		for i := 0; i < sig.NumParams(); i++ {
			if !v.assignCompat(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], sig.Params[i].Name)
				v.error(call.ActualParams[i].Pos(), msg)
			}
		}
	} else {
		obj := v.env.Lookup(call.Dsg.String())
		if obj == nil {
			v.error(call.Pos(), fmt.Sprintf("unknown procedure '%v'", call.Dsg.String()))
		}

		b := obj.(*scope.Builtin)
		v.checkBuiltin(b, call)
	}
}

func (v *Visitor) checkBuiltin(b *scope.Builtin, call *ast.ProcCall) {
	proc := scope.PredeclaredProcedures[b.Id]
	switch b.Id {
	case scope.Assert_:
		if proc.Nargs != len(call.ActualParams) {
			v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
		}

		for i := 0; i < proc.Nargs; i++ {
			if !v.assignCompat(scope.Typ[types.Bool], call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], scope.Typ[types.Bool])
				v.error(call.ActualParams[i].Pos(), msg)
			}
		}
	//case _Assertn:
	//case _Bytes:
	//case _Dec:
	//case _Decn:
	//case _Excl:
	//case _Halt:
	case scope.Inc_:
		//case _Incn:
		//case _Incl:
		//case _New:
		//case _Newn:
		//case _Number:
		//case _PCall:
		//case _Raise:
		//case _Copy:
		//case _Pack:
		//case _UnPk:
		//
		//case _Abs:
		//case _Cap:
		//case _BitAnd:
		//case _BitAsr:
		//case _BitNot:
		//case _BitOr:
		//case _Bits:
		//case _BitShl:
		//case _BitShr:
		//case _BitXor:
		//case _Cast:
		//case _Chr:
		//case _Default:
		//case _Floor:
		//case _Flt:
		//case _LdCmd:
		//case _LdMod:
		//case _Len:
		//case _LenN:
		//case _Long:
		//case _Max:
		//case _MaxN:
		//case _Min:
		//case _MinN:
		//case _Odd:
		//case _Ord:
		//case _Short:
		//case _Size:
		//case _StrLen:
		//case _WChr:
		//case _ASh:
		//case _ASr:
		//case _Entier:
		//case _Lsl:
		//case _Ror:
	}
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	if obj := v.env.Lookup(decl.Head.Name.Name); obj != nil {
		v.error(decl.Pos(), fmt.Sprintf("name %s already declared at %v", obj.String(), obj.Pos()))
	} else {
		sig := ast.NewSignature(decl.Head.Rcv, decl.Head.FP)
		v.env.Insert(scope.NewProcedure(decl.Pos(), decl.Head.Name.Name, sig, decl.Head.Name.Props(), v.offset))
		v.offset += 8
	}

	off := v.offset
	v.offset = 0

	parent := v.env

	// create a new scope to accommodate the symbols in the procedure. Make this new scope
	// the current scope (v.env) so that subsequent insertions of symbols will go into this
	// scope while we are processing the procedure.
	v.env = scope.NewScope(v.env, decl.Head.Name.Name)

	decl.Head.Accept(v)
	decl.Body.Accept(v)

	// TODO check that all the paths in the body have a return statement that matches the procedure's return type

	// revert the scope and symbol offset back to the parent
	v.env = parent
	v.offset = off
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	decl.Type.Accept(v)

	for _, ident := range decl.IdentList {
		if obj := v.env.Lookup(ident.Name); obj != nil {
			v.error(decl.Pos(), fmt.Sprintf("variable name '%s' already declared at '%v'", obj.String(), obj.Pos()))
		} else {
			v.env.Insert(scope.NewVar(ident.Pos(), ident.Name, decl.Type.Type(), ident.Props(), v.offset))
			v.offset += decl.Type.Type().Width()
		}
	}
}

func (v *Visitor) VisitConstDecl(decl *ast.ConstDecl) {
	decl.Value.Accept(v)

	if obj := v.env.Lookup(decl.Name.Name); obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.error(decl.Pos(), msg)
	} else {
		v.env.Insert(scope.NewConst(decl.Name.NamePos, decl.Name.Name, decl.Value.Type(), decl.Name.Props(), decl.Value, v.offset))
		v.offset += decl.Value.Type().Width()
	}
}

func (v *Visitor) VisitTypeDecl(decl *ast.TypeDecl) {
	decl.DenotedType.Accept(v)
	if obj := v.env.Lookup(decl.Name.Name); obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.error(decl.Pos(), msg)
	} else {
		v.env.Insert(scope.NewTypeName(decl.Type, decl.Name.Name, decl.DenotedType.Type(), decl.Name.Props(), v.offset))
		v.offset += decl.DenotedType.Type().Width()
	}
}

func (v *Visitor) VisitBasicType(b *ast.BasicType) {
	obj := v.env.Lookup(b.Name())
	if obj == nil {
		msg := fmt.Sprintf("name '%v' is not declared and is not a predeclared type", b.Name())
		v.error(b.Pos(), msg)
	}

	t, ok := obj.Type().(types.Type)
	if !ok {
		msg := fmt.Sprintf("'%v' is not recognized as a type", b.Name())
		v.error(b.Pos(), msg)
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
				v.error(index.Pos(), msg)
			}

			if i.Info() != types.IsInteger {
				msg := fmt.Sprintf("expected the size of array to be integer-type, got '%s'", index.Type())
				v.error(index.Pos(), msg)
			}
		}
	}

	a.ElemType.Accept(v)

	a.EType = NewArray(a.ElemType.Type(), a.LenList)
}

func (v *Visitor) VisitProcType(p *ast.ProcType) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitPointerType(p *ast.PointerType) {
	p.Base.Accept(v)
	if _, isRecordType := p.Base.Type().(*Record); !isRecordType {
		if _, isArrayType := p.Base.Type().(*Array); !isArrayType {
			v.error(p.Pos(), "pointer base type must be an array or record-type")
		}
	}

	p.EType = &PtrType{p.Base.Type()}
}

func (v *Visitor) VisitRecordType(r *ast.RecordType) {
	var (
		base      *Record
		isRecType bool
	)

	// entering a new scope. store the current offset and reset v.offset to be used in the new scope
	off := v.offset
	v.offset = 0

	if r.BaseType != nil {
		r.BaseType.Accept(v)
		base, isRecType = r.BaseType.Type().(*Record)
		if !isRecType {
			msg := fmt.Sprintf("cannot extend from a '%s' which is not a record-type", r.BaseType.Type())
			v.error(r.BaseType.Pos(), msg)
		}
	}

	ty := NewRecordType(scope.NewScope(v.env, ""), base)

	for _, field := range r.Fields {
		field.Type.Accept(v)
		if _, ok := field.Type.Type().(types.Type); !ok {
			msg := fmt.Sprintf("cannot use '%s' as a field type", field.Type)
			v.error(field.Type.Pos(), msg)
		}

		for _, id := range field.IdList {
			if obj := ty.fields.Lookup(id.Name); obj != nil {
				msg := fmt.Sprintf("field name '%s' already declared at '%v'", id.Name, obj.Pos())
				v.error(id.NamePos, msg)
			} else {
				ty.fields.Insert(scope.NewVar(id.NamePos, id.Name, field.Type.Type(), id.IProps, v.offset))
				v.offset += field.Type.Type().Width()
			}
		}
	}

	r.EType = ty

	// restore the previous offset
	v.offset = off
}

func (v *Visitor) VisitEnumType(e *ast.EnumType) {
	ty := NewEnumType(e.Variants)

	for i, c := range e.Variants {
		if obj := v.env.Lookup(c.Name); obj != nil {
			msg := fmt.Sprintf("name '%s' already declared at '%v'", c.Name, obj.Pos())
			v.error(c.NamePos, msg)
		} else {
			value := &ast.BasicLit{
				Kind:  token.INT,
				Val:   strconv.Itoa(i),
				EType: types.NewBasicType(types.Int, types.IsInteger, "integer"),
			}

			v.env.Insert(scope.NewConst(c.NamePos, c.Name, ty, c.Props(), value, v.offset))
			v.offset += value.EType.Width()
		}
	}

	e.EType = ty
}

func (v *Visitor) VisitReceiver(rcv *ast.Receiver) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitProcHead(head *ast.ProcHead) {
	//head.Rcv.Accept(v)
	head.FP.Accept(v)
}

func (v *Visitor) VisitProcBody(body *ast.ProcBody) {
	for _, dec := range body.DeclSeq {
		dec.Accept(v)
	}

	for _, stmt := range body.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitFPSection(sec *ast.FPSection) {
	sec.Type.Accept(v)

	for _, name := range sec.Names {
		obj := v.env.Lookup(name.Name)
		if obj != nil {
			v.error(obj.Pos(), fmt.Sprintf("parameter name %s already declared at %v", obj.String(), obj.Pos()))
			continue
		}

		if sec.Mod == token.IN {
			v.env.Insert(scope.NewConst(name.NamePos, name.Name, sec.Type.Type(), name.Props(), nil, v.offset))
		} else {
			v.env.Insert(scope.NewVar(name.Pos(), name.Name, sec.Type.Type(), name.Props(), v.offset))
		}

		v.offset += sec.Type.Type().Width()
	}
}

func (v *Visitor) VisitFormalParams(params *ast.FormalParams) {
	for _, sec := range params.Params {
		sec.Accept(v)
	}

	params.RetType.Accept(v)
}

func (v *Visitor) VisitDotOp(op *ast.DotOp) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitIndexOp(op *ast.IndexOp) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitTypeGuard(guard *ast.TypeGuard) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitPointerDeref(deref *ast.PtrDref) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitImport(imp *ast.Import) {
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitExprRange(rng *ast.ExprRange) {
	panic("not implemented")
}
