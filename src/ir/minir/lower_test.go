package minir

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── helpers to build minimal HIR nodes ───────────────────────────────────────

func hirProg(fn *desugar.Function) *desugar.Program {
	return &desugar.Program{
		Modules: []*desugar.Module{{
			Name:  "Test",
			Decls: []desugar.Decl{fn},
		}},
	}
}

func hirParam(name string, ty types.Type) *desugar.Param {
	return &desugar.Param{Name: name, Kind: desugar.ValueParam, Typ: ty}
}

func hirParamRef(name string, ty types.Type) *desugar.Param {
	return &desugar.Param{Name: name, Kind: desugar.ValueParam, Typ: ty}
}

// helper for VAR parameter (passed by reference)
func hirParamVar(name string, ty types.Type) *desugar.Param {
	return &desugar.Param{Name: name, Kind: desugar.VarParam, Typ: ty}
}

func hirLit(val string, ty types.Type) *desugar.Literal {
	return &desugar.Literal{Kind: token.INT32_LIT, Value: val, SemaType: ty}
}

func checkLower(t *testing.T, label string, prog *Program) {
	t.Helper()
	if len(prog.Modules) != 1 || len(prog.Modules[0].Functions) != 1 {
		mods := 0
		fns := 0
		if len(prog.Modules) > 0 {
			mods = len(prog.Modules)
			fns = len(prog.Modules[0].Functions)
		}
		t.Fatalf("%s: expected 1 module with 1 function, got %d module(s) / %d function(s)", label, mods, fns)
	}
	fn := prog.Modules[0].Functions[0]
	errs := VerifyIR(fn)
	for _, e := range errs {
		t.Logf("%s verify error: %s", label, e.Error())
	}
	if len(errs) != 0 {
		t.Fatalf("%s: expected no verify errors, got %d", label, len(errs))
	}
	t.Logf("%s:\n%s", label, FormatFunction(fn))
}

// ── 1. identity(x: INT32): INT32 = RETURN x ──────────────────────────────────

func TestLower_Identity(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	fn := &desugar.Function{
		Name:   "identity",
		Params: []*desugar.Param{hirParam("x", i32)},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.ReturnStmt{Result: hirParamRef("x", i32)},
		}},
	}
	checkLower(t, "identity", Lower(hirProg(fn)))
}

// ── 2. max(a, b: INT32): INT32 – if-else ─────────────────────────────────────

func TestLower_Max(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	fn := &desugar.Function{
		Name:   "max",
		Params: []*desugar.Param{hirParam("a", i32), hirParam("b", i32)},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.IfStmt{
				Cond: &desugar.BinaryExpr{
					Op:       token.GREAT,
					Left:     hirParamRef("a", i32),
					Right:    hirParamRef("b", i32),
					SemaType: types.BooleanType,
				},
				Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
					&desugar.ReturnStmt{Result: hirParamRef("a", i32)},
				}},
				Else: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
					&desugar.ReturnStmt{Result: hirParamRef("b", i32)},
				}},
			},
		}},
	}
	checkLower(t, "max", Lower(hirProg(fn)))
}

// ── 3. sum10(): INT32 – counted loop + EXIT ───────────────────────────────────

func TestLower_CountedLoop(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	nVar := &desugar.Variable{Name: "n", Type: i32}
	sVar := &desugar.Variable{Name: "s", Type: i32}

	nRef := &desugar.VariableRef{Name: "n", Mangled: "n", SemaType: i32}
	sRef := &desugar.VariableRef{Name: "s", Mangled: "s", SemaType: i32}

	lit0 := hirLit("0", i32)
	lit1 := hirLit("1", i32)
	lit9 := hirLit("9", i32)

	fn := &desugar.Function{
		Name:   "sum10",
		Result: i32,
		Locals: []desugar.Decl{nVar, sVar},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.AssignStmt{Left: nRef, Right: lit0},
			&desugar.AssignStmt{Left: sRef, Right: lit0},
			&desugar.LoopStmt{Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
				&desugar.IfStmt{
					Cond: &desugar.BinaryExpr{
						Op:       token.GREAT,
						Left:     nRef,
						Right:    lit9,
						SemaType: types.BooleanType,
					},
					Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
						&desugar.ExitStmt{},
					}},
				},
				&desugar.AssignStmt{
					Left: sRef,
					Right: &desugar.BinaryExpr{
						Op: token.PLUS, Left: sRef, Right: nRef, SemaType: i32,
					},
				},
				&desugar.AssignStmt{
					Left: nRef,
					Right: &desugar.BinaryExpr{
						Op: token.PLUS, Left: nRef, Right: lit1, SemaType: i32,
					},
				},
			}}},
			&desugar.ReturnStmt{Result: sRef},
		}},
	}
	checkLower(t, "sum10", Lower(hirProg(fn)))
}

// ── 4. classify – CASE statement ────────────────────────────────────────────

func TestLower_Case(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	fn := &desugar.Function{
		Name:   "classify",
		Params: []*desugar.Param{hirParam("x", i32)},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.CaseStmt{
				Expr: hirParamRef("x", i32),
				Cases: []*desugar.Case{
					{
						Labels: []*desugar.LabelRange{{
							Low: hirLit("1", i32), High: hirLit("1", i32),
						}},
						Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
							&desugar.ReturnStmt{Result: hirLit("10", i32)},
						}},
					},
					{
						Labels: []*desugar.LabelRange{{
							Low: hirLit("2", i32), High: hirLit("2", i32),
						}},
						Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
							&desugar.ReturnStmt{Result: hirLit("20", i32)},
						}},
					},
				},
				Else: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
					&desugar.ReturnStmt{Result: hirLit("0", i32)},
				}},
			},
		}},
	}
	checkLower(t, "classify", Lower(hirProg(fn)))
}

// ── 5. abs – if with no else, then fall-through ──────────────────────────────

func TestLower_IfNoElse(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	fn := &desugar.Function{
		Name:   "abs",
		Params: []*desugar.Param{hirParam("x", i32)},
		Result: i32,
		Locals: []desugar.Decl{&desugar.Variable{Name: "r", Type: i32}},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			// r := x
			&desugar.AssignStmt{
				Left:  &desugar.VariableRef{Name: "r", Mangled: "r", SemaType: i32},
				Right: hirParamRef("x", i32),
			},
			// IF x < 0 THEN r := 0 - x END
			&desugar.IfStmt{
				Cond: &desugar.BinaryExpr{
					Op:       token.LESS,
					Left:     hirParamRef("x", i32),
					Right:    hirLit("0", i32),
					SemaType: types.BooleanType,
				},
				Then: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
					&desugar.AssignStmt{
						Left: &desugar.VariableRef{Name: "r", Mangled: "r", SemaType: i32},
						Right: &desugar.BinaryExpr{
							Op:       token.MINUS,
							Left:     hirLit("0", i32),
							Right:    hirParamRef("x", i32),
							SemaType: i32,
						},
					},
				}},
			},
			// RETURN r
			&desugar.ReturnStmt{
				Result: &desugar.VariableRef{Name: "r", Mangled: "r", SemaType: i32},
			},
		}},
	}
	checkLower(t, "abs", Lower(hirProg(fn)))
}

// ── 6. increment(var x: INT32) – ensure VAR param lowers to incoming address

func TestLower_Increment(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	// increment(VAR x: INT32) := x := x + 1
	fn := &desugar.Function{
		Name:   "increment",
		Params: []*desugar.Param{hirParamVar("x", i32)},
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.AssignStmt{
				Left: &desugar.Param{Name: "x", Kind: desugar.VarParam, Typ: i32},
				Right: &desugar.BinaryExpr{
					Op:       token.PLUS,
					Left:     &desugar.Param{Name: "x", Kind: desugar.VarParam, Typ: i32},
					Right:    hirLit("1", i32),
					SemaType: i32,
				},
			},
		}},
	}

	prog := Lower(hirProg(fn))

	// Basic sanity checks on lowered function shape
	if len(prog.Modules) != 1 || len(prog.Modules[0].Functions) != 1 {
		t.Fatalf("increment: expected 1 module with 1 function, got %d modules / %d functions",
			len(prog.Modules), func() int {
				if len(prog.Modules) > 0 {
					return len(prog.Modules[0].Functions)
				}
				return 0
			}())
	}
	lfn := prog.Modules[0].Functions[0]
	if len(lfn.Params) != 1 {
		t.Fatalf("increment: expected 1 lowered param, got %d", len(lfn.Params))
	}
	p := lfn.Params[0]
	// VAR params should be incoming pointer/address temps
	if _, ok := p.Type().(*PointerType); !ok {
		t.Fatalf("increment: expected param to be pointer type, got %T", p.Type())
	}
	if !p.IsAddr {
		t.Fatalf("increment: expected param temp to be addressable (IsAddr=true)")
	}

	// Run the usual verification and dump the lowered IR for inspection
	checkLower(t, "increment", prog)
}

// ── 7. in parameter – ensure IN param lowers to incoming address (read-only)

func TestLower_InParam(t *testing.T) {
	tempIDCounter = 0
	i32 := types.Int32Type

	// increment(IN x: INT32) := RETURN x + 1
	fn := &desugar.Function{
		Name:   "increment_in",
		Params: []*desugar.Param{{Name: "x", Kind: desugar.InParam, Typ: i32}},
		Result: i32,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.ReturnStmt{Result: &desugar.BinaryExpr{
				Op:       token.PLUS,
				Left:     &desugar.Param{Name: "x", Kind: desugar.InParam, Typ: i32},
				Right:    hirLit("1", i32),
				SemaType: i32,
			}},
		}},
	}

	prog := Lower(hirProg(fn))

	// Basic sanity checks on lowered function shape
	if len(prog.Modules) != 1 || len(prog.Modules[0].Functions) != 1 {
		t.Fatalf("increment_in: expected 1 module with 1 function, got %d modules / %d functions",
			len(prog.Modules), func() int {
				if len(prog.Modules) > 0 {
					return len(prog.Modules[0].Functions)
				}
				return 0
			}())
	}
	lfn := prog.Modules[0].Functions[0]
	if len(lfn.Params) != 1 {
		t.Fatalf("increment_in: expected 1 lowered param, got %d", len(lfn.Params))
	}
	p := lfn.Params[0]
	// IN params should be incoming pointer/address temps
	if _, ok := p.Type().(*PointerType); !ok {
		t.Fatalf("increment_in: expected param to be pointer type, got %T", p.Type())
	}
	if !p.IsAddr {
		t.Fatalf("increment_in: expected param temp to be addressable (IsAddr=true)")
	}

	// Run the usual verification and dump the lowered IR for inspection
	checkLower(t, "increment_in", prog)
}
