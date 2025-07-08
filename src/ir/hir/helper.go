package hir

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

func (g Generator) emitOp(tok token.Kind) Op {
	switch tok {
	case token.PLUS:
		return Add
	case token.MINUS:
		return Sub
	case token.STAR:
		return Mul
	case token.DIV:
		return Div
	case token.MOD:
		return Mod
	case token.AND:
		return And
	case token.OR:
		return Or
	case token.EQUAL:
		return EQ
	case token.NEQ:
		return NEQ
	case token.LESS:
		return LT
	case token.LEQ:
		return LE
	case token.GREAT:
		return GT
	case token.GEQ:
		return GE
	case token.NOT:
		return Not
	default:
		return "nil"
	}
}

func (g Generator) emitType(ty types.Type) Type {
	switch ty := ty.(type) {
	case types.StringType:
		return &StringType{}
	case *types.BasicType:
		switch ty.Kind {
		case types.BYTE:
			return &IntType{Bits: ty.Size * 8, Signed: false}
		case types.INT8, types.INT16, types.INT32, types.INT64, types.SHORTINT, types.INTEGER, types.LONGINT:
			return &IntType{Bits: ty.Size * 8, Signed: true}
		case types.REAL, types.LONGREAL:
			return &RealType{Bits: ty.Size * 8}
		case types.CHAR, types.WCHAR:
			return &CharType{}
		case types.BOOLEAN:
			return &BoolType{}
		case types.SET:
			return &SetType{}
		case types.NIL:
			return &NilType{}
		default:
			return &UnknownType{}
		}
	default:
		return nil

	}
}

func (g Generator) emitParamKind(kind token.Kind) ParamKind {
	switch kind {
	case token.VAR:
		return VarParam
	case token.IN:
		return InParam
	default:
		return ValueParam
	}
}

func (g Generator) visitStmtSeq(stmts []ast.Statement) *CompoundStmt {
	var bodyStmts []Stmt
	for _, stmt := range stmts {
		hirStmt := stmt.Accept(g).(Stmt)
		bodyStmts = append(bodyStmts, hirStmt)
	}

	return &CompoundStmt{
		Stmts: bodyStmts,
	}
}

func (g Generator) visitDeclSeq(decls []ast.Declaration) (d []Decl) {
	for _, decl := range decls {
		d = append(d, decl.Accept(g).(Decl))
	}

	return d
}

func (g Generator) visitElseIfs(branches []*ast.ElseIfBranch) []*ElseIfBranch {
	var elseIfs []*ElseIfBranch
	for _, branch := range branches {
		elseIfs = append(elseIfs, branch.Accept(g).(*ElseIfBranch))
	}

	return elseIfs
}

func hirFormat(m *Module) string {
	var b strings.Builder
	fmt.Fprintf(&b, "Module %s\n", m.Name)

	if len(m.Imports) > 0 {
		b.WriteString("Imports:\n")
		for _, imp := range m.Imports {
			fmt.Fprintf(&b, "  - %s\n", imp.Path)
		}
	}

	if len(m.Globals) > 0 {
		b.WriteString("Globals:\n")
		for _, decl := range m.Globals {
			switch v := decl.(type) {
			case *VarDecl:
				fmt.Fprintf(&b, "  - VAR %s\n", v.String())
			case *ConstDecl:
				fmt.Fprintf(&b, "  - CONST %s\n", v.String())
			case *TypeDecl:
				fmt.Fprintf(&b, "  - TYPE %s\n", v.String())
			}

		}
	}

	if m.Init != nil {
		b.WriteString("Body:\n")
		for _, stmt := range m.Init.Body.Stmts {
			fmt.Fprintf(&b, "  %s\n", stmt)
		}
	}

	//if len(m.Procedures) > 0 {
	//	b.WriteString("Body:\n")
	//	for _, stmt := range m.Body {
	//		fmt.Fprintf(&b, "  %s\n", formatStmt(stmt))
	//	}
	//}

	return strings.TrimSpace(b.String())
}
