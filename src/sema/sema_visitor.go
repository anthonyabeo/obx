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
		v.errors.Append(obj.Pos(), fmt.Sprintf("name %v is undecleared", id.Name))
		return
	}

	id.EType = obj.Type()
}

func (v *Visitor) VisitUInt(uInt *ast.UInt) {
	uInt.EType = types.NewBasicType(types.Int, types.IsInteger, "integer")
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
			v.errors.Append(expr.Pos(), fmt.Sprintf("operation %v can only be perfomed on numeric types", expr.OpPos))
		}

		if left.Info() == types.IsInteger && right.Info() == types.IsInteger {
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

	obj := v.env.Lookup(call.String())
	if proc, ok := obj.(*Procedure); !ok {
		v.errors.Append(call.Pos(), fmt.Sprintf("cannot call a non-procedure %v", proc.name))
	}

	sig := call.Dsg.Type().(*types.Signature)
	sig.DeSugarParams()

	// ensure that the number of arguments matches the number of formal parameters
	if sig.NumParams() != len(call.ActualParams) {
		v.errors.Append(call.Pos(), fmt.Sprintf("not enough arguments to function call '%v'", call.String()))
	}

	// ensure that the ith argument is assignment-compatible with the ith formal parameter
	for i := 0; i < sig.NumParams(); i++ {
		if !v.AreAssignmentComp(sig.Params[i].Type, call.ActualParams[i]) {
			msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
				call.ActualParams[i], sig.Params[i].Name)
			v.errors.Append(call.ActualParams[i].Pos(), msg)
		}
	}

	call.EType = v.typeFromExpr(sig.FP.RetType)

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
		v.errors.Append(stmt.BoolExpr.Pos(), msg)
	}

	for _, s := range stmt.ThenPath {
		s.Accept(v)
	}

	for _, elsif := range stmt.ElseIfBranches {
		elsif.BoolExpr.Accept(v)
		if elsif.BoolExpr.Type() != Typ[types.Bool] {
			msg := fmt.Sprintf("expression '%v' does not evaluate to a boolean", elsif.BoolExpr)
			v.errors.Append(elsif.BoolExpr.Pos(), msg)
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

	if !v.AreAssignmentComp(stmt.LValue, stmt.RValue) {
		v.errors.Append(stmt.AssignPos, fmt.Sprintf("%v and %v are not assignment compatible", stmt.LValue, stmt.RValue))
	}
}

func (v *Visitor) AreAssignmentComp(left, right ast.Expression) bool {
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
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcCall(call *ast.ProcCall) {
	//TODO implement me
	panic("implement me")
}

func (v *Visitor) VisitProcDecl(decl *ast.ProcDecl) {
	if obj := v.env.Lookup(decl.Head.Name.Name); obj != nil {
		v.errors.Append(decl.Pos(), fmt.Sprintf("name %s already declared at %v", obj.String(), obj.Pos()))
	} else {
		sig := &types.Signature{Rcv: decl.Head.Rcv, FP: decl.Head.FP}
		v.env.Insert(NewProcedure(decl.Pos(), decl.Head.Name.Name, sig, decl.Head.Name.Exported))
	}

	// create a new scope to accommodate the symbols in the procedure
	procScope := NewScope(v.env, decl.Head.Name.Name)

	for _, FP := range decl.Head.FP.Params {
		for _, name := range FP.Names {
			if sym := procScope.Lookup(name.Name); sym != nil {
				v.errors.Append(decl.Pos(), fmt.Sprintf("parameter name %s already declared at %v", sym.String(), sym.Pos()))
			} else {
				Type := v.typeFromExpr(FP.Type)
				procScope.Insert(NewVar(name.Pos(), name.Name, Type, name.Exported))
			}
		}
	}

	for _, dec := range decl.Body.DeclSeq {
		dec.Accept(v)
	}

	for _, stmt := range decl.Body.StmtSeq {
		stmt.Accept(v)
	}
}

func (v *Visitor) VisitVarDecl(decl *ast.VarDecl) {
	var varDeclType = v.typeFromExpr(decl.Type)

	for _, ident := range decl.IdentList {
		if obj := v.env.Lookup(ident.Name); obj != nil {
			v.errors.Append(decl.Pos(), fmt.Sprintf("variable name %s already declared at %v", obj.String(), obj.Pos()))
		} else {
			v.env.Insert(NewVar(ident.Pos(), ident.Name, varDeclType, ident.Exported))
		}
	}
}

func (v *Visitor) typeFromExpr(expr ast.Expression) types.Type {
	var Type types.Type

	switch typ := expr.(type) {
	case *ast.Designator:
		switch t := typ.QualifiedIdent.(type) {
		case *ast.Ident:
			obj := v.env.Lookup(t.Name)
			vdt, ok := obj.(types.Type)
			if obj == nil || !ok {
				v.errors.Append(obj.Pos(), fmt.Sprintf("%v is not a recognized data-type", t.Name))
				return nil
			}

			Type = vdt
		}
		// case *ast.ArrayType:
		// case *ast.RecordType
	}

	return Type
}
