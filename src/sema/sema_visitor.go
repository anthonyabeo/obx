package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

type Visitor struct {
	errors lexer.ErrorList

	ast *ast.Oberon
	env *Scope
}

func (v *Visitor) InitSemaVisitor(ast *ast.Oberon, env *Scope) {
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
		return
	}

	id.EType = obj.Type()
}

func (v *Visitor) VisitBasicLit(b *ast.BasicLit) {
	switch b.Kind {
	case token.INT:
		// TODO Number literals are (unsigned) integer or real constants.
		// TODO The type of an integer literal is the minimal type to which the constant value belongs
		// TODO If a decimal or hexadecimal literal is specified with the suffix I (or i), then the type is INT32.
		// TODO If a decimal or hexadecimal constant is specified with the suffix L (or l), then the type is INT64.
		b.EType = Typ[types.Int]
	case token.REAL:
	case token.STRING:
	case token.HEXSTRING:
	}
}

func (v *Visitor) VisitSet(s *ast.Set) {

}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	// TODO the operands could be non-basic types.
	left, _ := expr.Left.Type().(*types.Basic)
	right, _ := expr.Right.Type().(*types.Basic)

	switch expr.Op {
	case token.PLUS, token.MINUS, token.STAR, token.QUOT:
		if left.Info() != types.IsNumeric || right.Info() != types.IsNumeric {
			msg := fmt.Sprintf("cannot perform operation '%v' on non-numeric types, '%v' and '%v'", expr.Op, expr.Left, expr.Right)
			v.error(expr.Pos(), msg)
		}

		if expr.Op == token.QUOT {
			expr.EType = Typ[types.LReal]
			return
		}

		if left.Kind() == types.Int && right.Kind() == types.Int {
			if expr.EType = left; left.Kind() < right.Kind() {
				expr.EType = right
			}
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
		if left.Info() != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray && IsBool && IsSet && IsPointer && IsProc && IsNil */ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
			v.error(expr.Left.Pos(), msg)
		}

		if right.Info() != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray & IsBool && IsSet && IsPointer && IsProc && IsNil */ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
			v.error(expr.Right.Pos(), msg)
		}

		expr.EType = Typ[types.Bool]
	case token.LESS, token.LEQ, token.GREAT, token.GEQ:
		if left.Info() != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray*/ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Left)
			v.error(expr.Left.Pos(), msg)
		}

		if right.Info() != types.IsNumeric /* && left.info() != IsEnum && IsChar && IsString && IsCharArray*/ {
			msg := fmt.Sprintf("cannot perform operation '%v' on '%v' type", expr.Op, expr.Right)
			v.error(expr.Right.Pos(), msg)
		}

		expr.EType = Typ[types.Bool]
	case token.OR, token.AND:
		expr.EType = Typ[types.Bool]
	case token.NOT:
		expr.EType = Typ[types.Bool]
	case token.IN:
		expr.EType = Typ[types.Bool]
	case token.IS:
		expr.EType = Typ[types.Bool]
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
		if !v.AreAssignmentComp(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
			msg := fmt.Sprintf("argument '%v' (of type '%v') does not match the corresponding parameter type '%v'",
				call.ActualParams[i], call.ActualParams[i].Type(), sig.Params[i].Type.Type())
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
		if X.Info() != types.IsNumeric {
			msg := fmt.Sprintf("'%v' must be numeric type", expr.X)
			v.error(expr.X.Pos(), msg)
			return
		}

		expr.EType = X
	case token.NOT:
		if X.Info() != types.IsBoolean {
			msg := fmt.Sprintf("'%v' does not evaluate to Boolean type", expr.X)
			v.error(expr.X.Pos(), msg)
			return
		}

		expr.EType = Typ[types.Bool]
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
	if stmt.BoolExpr.Type() != Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElseIfBranches {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != Typ[types.Bool] {
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

	if !v.AreAssignmentComp(stmt.LValue.Type(), stmt.RValue.Type()) {
		v.error(stmt.AssignPos, fmt.Sprintf("%v and %v are not assignment compatible", stmt.LValue, stmt.RValue))
	}
}

func (v *Visitor) AreAssignmentComp(left, right types.Type) bool {
	if left.String() == right.String() {
		return true
	}

	l, _ := left.(*types.Basic)
	r, _ := right.(*types.Basic)
	if l.Info() == types.IsNumeric && r.Info() == types.IsNumeric {
		return l.Kind() >= r.Kind()
	}

	return false
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	stmt.BoolExpr.Accept(v)
	if stmt.BoolExpr.Type() != Typ[types.Bool] {
		msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", stmt.BoolExpr)
		v.error(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.StmtSeq {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElsIfs {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != Typ[types.Bool] {
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
	// TODO not implemented
	panic("not implemented")
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	call.Dsg.Accept(v)
	for _, param := range call.ActualParams {
		param.Accept(v)
	}

	if call.Dsg.Type() != Typ[types.Invalid] {
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
			if !v.AreAssignmentComp(sig.Params[i].Type.Type(), call.ActualParams[i].Type()) {
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

		b := obj.(*Builtin)
		v.checkBuiltin(b, call)
	}
}

func (v *Visitor) checkBuiltin(b *Builtin, call *ast.ProcCall) {
	proc := predeclaredProcedures[b.id]
	switch b.id {
	case _Assert:
		if proc.nargs != len(call.ActualParams) {
			v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
		}

		for i := 0; i < proc.nargs; i++ {
			if !v.AreAssignmentComp(Typ[types.Bool], call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], Typ[types.Bool])
				v.error(call.ActualParams[i].Pos(), msg)
			}
		}
	}
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	if obj := v.env.Lookup(decl.Head.Name.Name); obj != nil {
		v.error(decl.Pos(), fmt.Sprintf("name %s already declared at %v", obj.String(), obj.Pos()))
	} else {
		sig := ast.NewSignature(decl.Head.Rcv, decl.Head.FP)
		v.env.Insert(NewProcedure(decl.Pos(), decl.Head.Name.Name, sig, decl.Head.Name.Props()))
	}

	parent := v.env

	// create a new scope to accommodate the symbols in the procedure. Make this new scope
	// the current scope (v.env) so that subsequent insertions of symbols will go into this
	// scope while we are processing the procedure.
	v.env = NewScope(v.env, decl.Head.Name.Name)

	decl.Head.Accept(v)
	decl.Body.Accept(v)

	// TODO check that all the paths in the body have a return statement that matches the procedure's return type

	// revert the scope back to the parent
	v.env = parent
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	decl.Type.Accept(v)

	for _, ident := range decl.IdentList {
		if obj := v.env.Lookup(ident.Name); obj != nil {
			v.error(decl.Pos(), fmt.Sprintf("variable name '%s' already declared at '%v'", obj.String(), obj.Pos()))
		} else {
			v.env.Insert(NewVar(ident.Pos(), ident.Name, decl.Type.Type(), ident.Props()))
		}
	}
}

func (v *Visitor) VisitConstDecl(decl *ast.ConstDecl) {
	decl.Value.Accept(v)

	if obj := v.env.Lookup(decl.Name.Name); obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.error(decl.Pos(), msg)
	} else {
		v.env.Insert(NewConst(decl.Name.NamePos, decl.Name.Name, decl.Value.Type(), decl.Name.Props(), decl.Value))
	}
}

func (v *Visitor) VisitTypeDecl(decl *ast.TypeDecl) {
	decl.DenotedType.Accept(v)
	if obj := v.env.Lookup(decl.Name.Name); obj != nil {
		msg := fmt.Sprintf("name '%s' already declared at '%v'", obj.String(), obj.Pos())
		v.error(decl.Pos(), msg)
	} else {
		v.env.Insert(NewTypeName(decl.Type, decl.Name.Name, decl.DenotedType.Type(), decl.Name.Props()))
	}
}

func (v *Visitor) VisitBasicType(b *ast.BasicType) {
	obj := v.env.Lookup(b.Name())
	if obj == nil {
		msg := fmt.Sprintf("name '%v' is not declared and is not a predeclared type", b.Name())
		v.error(b.Pos(), msg)
		return
	}

	t, ok := obj.Type().(types.Type)
	if !ok {
		msg := fmt.Sprintf("'%v' is not recognized as a type", b.Name())
		v.error(b.Pos(), msg)
	}

	b.EType = t
}

func (v *Visitor) VisitArrayType(a *ast.ArrayType) {
	var Len ast.Expression
	if a.LenList != nil {
		for _, index := range a.LenList.List {
			index.Accept(v)
		}

		Len = a.LenList.List[0]

	}

	a.ElemType.Accept(v)

	a.EType = NewArray(a.ElemType.Type(), Len)
}

func (v *Visitor) VisitReceiver(rcv *ast.Receiver) {
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
		if obj := v.env.Lookup(name.Name); obj != nil {
			v.error(obj.Pos(), fmt.Sprintf("parameter name %s already declared at %v", obj.String(), obj.Pos()))
		} else {
			v.env.Insert(NewVar(name.Pos(), name.Name, sec.Type.Type(), name.Props()))
		}
	}
}

func (v *Visitor) VisitFormalParams(params *ast.FormalParams) {
	for _, sec := range params.Params {
		sec.Accept(v)
	}

	params.RetType.Accept(v)
}

func (v *Visitor) VisitDotOp(op *ast.DotOp) {}

func (v *Visitor) VisitIndexOp(op *ast.IndexOp) {}

func (v *Visitor) VisitTypeGuard(guard *ast.TypeGuard) {}

func (v *Visitor) VisitPointerDeref(deref *ast.PointerDeref) {}
