package minir

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestLower_StringLiteralBecomesGlobalConst(t *testing.T) {
	ResetTempCounter()

	strTy := types.NewStringType(5)
	lit := &desugar.Literal{Kind: token.STR_LIT, Value: "hello", SemaType: strTy}
	call := &desugar.FuncCall{
		Func: &desugar.FunctionRef{Name: "puts", Mangled: "puts", IsExternal: true},
		Args: []desugar.Expr{lit},
	}

	fn := &desugar.Function{
		Name:   "main",
		Result: types.Int32Type,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			call,
			&desugar.ReturnStmt{Result: &desugar.Literal{Kind: token.INT32_LIT, Value: "0", SemaType: types.Int32Type}},
		}},
	}

	prog := Lower(&desugar.Program{Modules: []*desugar.Module{{Name: "M", Decls: []desugar.Decl{fn}}}})
	if len(prog.Modules) != 1 {
		t.Fatalf("modules=%d, want 1", len(prog.Modules))
	}
	mod := prog.Modules[0]
	if len(mod.Constants) != 1 {
		t.Fatalf("string constants=%d, want 1", len(mod.Constants))
	}
	gc := mod.Constants[0]
	if !strings.HasPrefix(gc.Name, "_Lstr_") {
		t.Fatalf("global string name = %q, want prefix _Lstr_", gc.Name)
	}

	fnOut := mod.Functions[0]
	var gotArg Value
	for _, b := range fnOut.Blocks {
		for _, ins := range b.Instrs {
			if c, ok := ins.(*CallInst); ok && len(c.Args) == 1 {
				gotArg = c.Args[0]
			}
		}
	}
	ref, ok := gotArg.(*GlobalRef)
	if !ok {
		t.Fatalf("call arg type = %T, want *GlobalRef", gotArg)
	}
	if ref.GlobalName != gc.Name {
		t.Fatalf("call arg global = %q, want %q", ref.GlobalName, gc.Name)
	}
}

func TestLower_StringLiteralInterning(t *testing.T) {
	ResetTempCounter()

	strTy := types.NewStringType(5)
	lit1 := &desugar.Literal{Kind: token.STR_LIT, Value: "hello", SemaType: strTy}
	lit2 := &desugar.Literal{Kind: token.STR_LIT, Value: "hello", SemaType: strTy}

	fn := &desugar.Function{
		Name:   "main",
		Result: types.Int32Type,
		Body: &desugar.CompoundStmt{Stmts: []desugar.Stmt{
			&desugar.FuncCall{Func: &desugar.FunctionRef{Name: "puts", Mangled: "puts", IsExternal: true}, Args: []desugar.Expr{lit1}},
			&desugar.FuncCall{Func: &desugar.FunctionRef{Name: "puts", Mangled: "puts", IsExternal: true}, Args: []desugar.Expr{lit2}},
			&desugar.ReturnStmt{Result: &desugar.Literal{Kind: token.INT32_LIT, Value: "0", SemaType: types.Int32Type}},
		}},
	}

	prog := Lower(&desugar.Program{Modules: []*desugar.Module{{Name: "M", Decls: []desugar.Decl{fn}}}})
	mod := prog.Modules[0]
	if len(mod.Constants) != 1 {
		t.Fatalf("string constants=%d, want 1", len(mod.Constants))
	}
}

