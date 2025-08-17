package format

import (
	"github.com/anthonyabeo/obx/src/ir/hir"
	"github.com/anthonyabeo/obx/src/types"
)

type Formatter struct {
	program *hir.Program
}

func NewFormatter(program *hir.Program) *Formatter {
	return &Formatter{program: program}
}

func (fmt *Formatter) FormatProgram() any {
	var modules []any
	for _, module := range fmt.program.Modules {
		modules = append(modules, fmt.formatModule(module))
	}

	return map[string]any{
		"type":    "HIRProgram",
		"modules": modules,
	}
}

func (fmt *Formatter) formatModule(m *hir.Module) any {
	return map[string]any{
		"type":         "Module",
		"name":         m.Name,
		"declarations": fmt.formatDecl(m.Decls),
		"statements":   fmt.formatFunction(m.Init),
	}
}

func (fmt *Formatter) formatFunction(fxn *hir.Function) any {
	return map[string]any{
		"type":   "Function",
		"name":   fxn.Name,
		"params": fmt.formatParams(fxn.Params),
		"result": fmt.formatType(fxn.Result),
		"locals": fmt.formatDecl(fxn.Locals),
		"body":   fmt.formatStmt(fxn.Body),
	}
}

func (fmt *Formatter) formatParams(params []*hir.Param) any {
	var formalParams []any
	for _, param := range params {
		formalParams = append(formalParams, map[string]any{
			"type":      "param",
			"name":      param.Name,
			"kind":      param.Kind,
			"paramType": fmt.formatType(param.Type()),
		})
	}

	return map[string]any{
		"type":   "FormalParams",
		"params": formalParams,
	}
}

func (fmt *Formatter) formatDecl(decls []hir.Decl) any {
	var result []any
	for _, decl := range decls {
		switch d := decl.(type) {
		case *hir.Variable:
			result = append(result, map[string]any{
				"type": "Variable",
				"name": d.Name,
				"typ":  fmt.formatType(d.Type),
			})
		case *hir.Function:
			result = append(result, fmt.formatFunction(d))
		case *hir.Constant:
			result = append(result, map[string]any{
				"type":      "Constant",
				"name":      d.Name,
				"constType": fmt.formatType(d.Type),
				"value":     fmt.formatExpr(d.Value),
			})
		case *hir.Type:
			result = append(result, map[string]any{
				"type": "TypeDecl",
				"name": d.Name,
				"def":  fmt.formatType(d.Type),
			})
		}
	}

	return map[string]any{
		"type":  "Declarations",
		"decls": result,
	}
}

func (fmt *Formatter) formatStmt(stmt hir.Stmt) any {
	if stmt == nil {
		return nil
	}

	switch s := stmt.(type) {
	case *hir.CompoundStmt:
		var stmts []any
		for _, st := range s.Stmts {
			stmts = append(stmts, fmt.formatStmt(st))
		}

		return map[string]any{
			"type":       "Block",
			"statements": stmts,
		}
	case *hir.ReturnStmt:
		return map[string]any{
			"type":  "Return",
			"value": fmt.formatExpr(s.Result),
		}
	case *hir.AssignStmt:
		return map[string]any{
			"type":   "Assignment",
			"target": fmt.formatExpr(s.Left),
			"value":  fmt.formatExpr(s.Right),
		}
	case *hir.IfStmt:
		m := map[string]any{
			"type":      "If",
			"condition": fmt.formatExpr(s.Cond),
			"then":      fmt.formatStmt(s.Then),
		}

		if s.Else != nil {
			m["else"] = fmt.formatStmt(s.Else)
		}

		var elseifs []any
		for _, elseIf := range s.ElseIfs {
			elseifs = append(elseifs, map[string]any{
				"condition": fmt.formatExpr(elseIf.Cond),
				"body":      fmt.formatStmt(elseIf.Body),
			})
		}
		if len(elseifs) > 0 {
			m["else_ifs"] = elseifs
		}

		return m
	case *hir.LoopStmt:
		return map[string]any{
			"type": "LoopStmt",
			"body": fmt.formatStmt(s.Body),
		}
	case *hir.CaseStmt:
		var cases []any
		for _, c := range s.Cases {
			var labels []any
			for _, label := range c.Labels {
				labels = append(labels, map[string]any{
					"type": "LabelRange",
					"low":  fmt.formatExpr(label.Low),
					"high": fmt.formatExpr(label.High),
				})
			}

			cases = append(cases, map[string]any{
				"type":   "Case",
				"labels": labels,
				"body":   fmt.formatStmt(c.Body),
			})
		}

		return map[string]any{
			"type":  "Case",
			"expr":  fmt.formatExpr(s.Expr),
			"cases": cases,
			"else":  fmt.formatStmt(s.Else),
		}
	case *hir.WithStmt:
		var guards []any
		for _, guard := range s.Guards {
			guards = append(guards, map[string]any{
				"type":     "Guard",
				"expr":     fmt.formatExpr(guard.Expr),
				"exprType": fmt.formatExpr(guard.Type),
			})
		}

		return map[string]any{
			"type":   "WithStmt",
			"guards": guards,
			"body":   fmt.formatStmt(s.Else),
		}
	case *hir.ExitStmt:
		return map[string]any{
			"type":   "Exit",
			"target": s.LoopLabel,
		}
	case *hir.FuncCall:
		var args []any
		for _, arg := range s.Args {
			args = append(args, fmt.formatExpr(arg))
		}
		return map[string]any{
			"type":      "FunctionCall",
			"function":  fmt.formatExpr(s.Func),
			"arguments": args,
		}
	default:
		return nil
	}
}

func (fmt *Formatter) formatType(typ types.Type) any {
	if typ == nil {
		return map[string]any{
			"type": "VoidType",
			"name": "void",
		}
	}

	switch t := typ.(type) {
	case *types.ArrayType:
		return map[string]any{
			"type":    "ArrayType",
			"length":  t.Length,
			"element": fmt.formatType(t.Elem),
		}
	case *types.PointerType:
		return map[string]any{
			"type": "PointerType",
			"base": fmt.formatType(t.Base),
		}
	case *types.ProcedureType:
		var formalParams []any
		for _, param := range t.Params {
			formalParams = append(formalParams, map[string]any{
				"type":      "param",
				"name":      param.Name,
				"kind":      param.Kind,
				"paramType": fmt.formatType(param.Type),
			})
		}

		return map[string]any{
			"type":   "ProcedureType",
			"params": formalParams,
			"return": fmt.formatType(t.Result),
		}
	case *types.RecordType:
		var fields []any
		for s, field := range t.Fields {
			fields = append(fields, map[string]any{
				"type":      "Field",
				"name":      s,
				"fieldType": fmt.formatType(field.Type),
			})
		}

		return map[string]any{
			"type":   "RecordType",
			"fields": fields,
			"super":  fmt.formatType(t.Base),
		}
	case *types.EnumType:
		var members []string
		for s := range t.Variants {
			members = append(members, s)
		}

		return map[string]any{
			"type":    "EnumType",
			"members": members,
		}
	case *types.NamedType:
		return map[string]any{
			"type":      "NamedType",
			"name":      t.Name,
			"underType": fmt.formatType(t.Def),
		}
	case *types.BasicType:
		return map[string]any{
			"type": "BasicType",
			"name": t.String(),
		}
	default:
		panic("unknown type")
	}
}

func (fmt *Formatter) formatExpr(expr hir.Expr) any {
	if expr == nil {
		return nil
	}

	switch e := expr.(type) {
	case *hir.BinaryExpr:
		return map[string]any{
			"type":     "BinaryExpr",
			"operator": e.Op,
			"left":     fmt.formatExpr(e.Left),
			"right":    fmt.formatExpr(e.Right),
		}
	case *hir.Literal:
		return map[string]any{
			"type":  "Literal",
			"value": e.Value,
		}
	case *hir.VariableRef:
		return map[string]any{
			"type": "VariableRef",
			"name": e.Name,
		}
	case *hir.ConstantRef:
		return map[string]any{
			"type": "ConstantRef",
			"name": e.Name,
		}
	case *hir.UnaryExpr:
		return map[string]any{
			"type":     "UnaryExpr",
			"operator": e.Op,
			"operand":  fmt.formatExpr(e.Operand),
		}
	case *hir.FuncCall:
		var args []any
		for _, arg := range e.Args {
			args = append(args, fmt.formatExpr(arg))
		}
		return map[string]any{
			"type":      "FunctionCall",
			"function":  fmt.formatExpr(e.Func),
			"arguments": args,
			"return":    fmt.formatType(e.RetType),
		}
	case *hir.FunctionRef:
		return map[string]any{
			"type": "FunctionRef",
			"name": e.Name,
		}
	case *hir.FieldAccess:
		return map[string]any{
			"type":   "FieldAccess",
			"record": fmt.formatExpr(e.Record),
			"field":  e.Field,
		}
	case *hir.IndexExpr:
		var indices []any
		for _, index := range e.Index {
			indices = append(indices, fmt.formatExpr(index))
		}
		return map[string]any{
			"type":     "IndexExpr",
			"array":    fmt.formatExpr(e.Array),
			"index":    indices,
			"elemType": fmt.formatType(e.SemaType),
		}
	case *hir.DerefExpr:
		return map[string]any{
			"type":    "DerefExpr",
			"operand": fmt.formatExpr(e.Pointer),
		}
	case *hir.TypeGuardExpr:
		return map[string]any{
			"type":       "TypeGuard",
			"expression": fmt.formatExpr(e.Expr),
			"guardType":  fmt.formatType(e.Typ),
		}
	case *hir.SetExpr:
		var elements []any
		for _, elem := range e.Elems {
			elements = append(elements, fmt.formatExpr(elem))
		}
		return map[string]any{
			"type":     "Set",
			"elements": elements,
		}
	case *hir.RangeExpr:
		return map[string]any{
			"type":  "Range",
			"start": fmt.formatExpr(e.Low),
			"end":   fmt.formatExpr(e.High),
		}
	case *hir.TypeRef:
		return map[string]any{
			"type": "TypeRef",
			"name": e.Name,
		}
	default:
		return nil
	}
}
