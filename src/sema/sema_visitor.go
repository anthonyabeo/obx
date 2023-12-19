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

func (v *Visitor) VisitUInt(uInt *ast.UInt) {
	uInt.EType = types.NewBasicType(types.Int, types.IsInteger|types.IsNumeric, "integer")
}

func (v *Visitor) VisitBinaryExpr(expr *ast.BinaryExpr) {
	expr.Left.Accept(v)
	expr.Right.Accept(v)

	leftType := expr.Left.Type()
	rightType := expr.Right.Type()

	switch expr.Op {
	case token.PLUS, token.MINUS, token.STAR:
		left := leftType.(*types.Basic)
		right := rightType.(*types.Basic)

		if left.Info() != types.IsNumeric || right.Info() != types.IsNumeric {
			v.error(expr.Pos(), fmt.Sprintf("cannot perform operation '%v' on non-numeric types, '%v' and '%v'",
				expr.Op, expr.Left, expr.Right))
		}

		if left.Kind() == types.Int && right.Kind() == types.Int {
			if left.Kind() > right.Kind() {
				expr.EType = leftType
			} else {
				expr.EType = rightType
			}
		}
	case token.EQUAL, token.OR:
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
		if !v.AreAssignmentComp(v.typeFromExpr(sig.Params[i].Type), call.ActualParams[i].Type()) {
			msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
				call.ActualParams[i], sig.Params[i].Name)
			v.error(call.ActualParams[i].Pos(), msg)
		}
	}

	call.EType = v.typeFromExpr(sig.ReturnType())
}

func (v *Visitor) VisitUnaryExpr(expr *ast.UnaryExpr) {
	//TODO implement me
	panic("implement me")
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

	return false
}

func (v *Visitor) VisitLoopStmt(stmt *ast.LoopStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitRepeatStmt(stmt *ast.RepeatStmt) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitWhileStmt(stmt *ast.WhileStmt) {
	panic("implement me")
}

func (v *Visitor) VisitReturnStmt(stmt *ast.ReturnStmt) {
	if stmt.Value != nil {
		stmt.Value.Accept(v)
	}
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
			if !v.AreAssignmentComp(v.typeFromExpr(sig.Params[i].Type), call.ActualParams[i].Type()) {
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

	curEnv := v.env

	// create a new scope to accommodate the symbols in the procedure
	procScope := NewScope(v.env, decl.Head.Name.Name)
	// make this new procedure scope the current scope so that subsequent insertions of symbols will
	// go into this scope while we are in processing the procedure
	v.env = procScope

	for _, FP := range decl.Head.FP.Params {
		for _, name := range FP.Names {
			if sym := v.env.Lookup(name.Name); sym != nil {
				v.error(decl.Pos(), fmt.Sprintf("parameter name %s already declared at %v", sym.String(), sym.Pos()))
			} else {
				Type := v.typeFromExpr(FP.Type)
				v.env.Insert(NewVar(name.Pos(), name.Name, Type, name.Props()))
			}
		}
	}

	for _, dec := range decl.Body.DeclSeq {
		dec.Accept(v)
	}

	for _, stmt := range decl.Body.StmtSeq {
		stmt.Accept(v)
	}

	// TODO check that all the paths in the body have a return statement that matches the
	// procedure's return type

	// return the environment back to the previous env
	v.env = curEnv
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	var varDeclType = v.typeFromExpr(decl.Type)

	for _, ident := range decl.IdentList {
		if obj := v.env.Lookup(ident.Name); obj != nil {
			v.error(decl.Pos(), fmt.Sprintf("variable name '%s' already declared at '%v'", obj.String(), obj.Pos()))
		} else {
			v.env.Insert(NewVar(ident.Pos(), ident.Name, varDeclType, ident.Props()))
		}
	}
}

func (v *Visitor) typeFromExpr(expr ast.Expression) types.Type {
	var Type types.Type

	switch typ := expr.(type) {
	case *ast.Ident:
		obj := v.env.Lookup(typ.Name)
		if obj != nil {
			vdt, ok := obj.Type().(types.Type)
			if !ok {
				v.error(obj.Pos(), fmt.Sprintf("%v is not a recognized data-type", typ.Name))
			} else {
				Type = vdt
			}
		}
	case *ast.Designator:
		switch t := typ.QualifiedIdent.(type) {
		case *ast.Ident:
			obj := v.env.Lookup(t.Name)
			vdt, ok := obj.(types.Type)
			if obj == nil || !ok {
				v.error(obj.Pos(), fmt.Sprintf("%v is not a recognized data-type", t.Name))
				return nil
			}

			Type = vdt
		}
		// case *ast.ArrayType:
		// case *ast.RecordType
	}

	return Type
}
