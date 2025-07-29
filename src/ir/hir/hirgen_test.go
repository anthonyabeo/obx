package hir

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
	"github.com/anthonyabeo/obx/src/types"
)

func ParseAndCheck(t *testing.T, src string) (*ast.OberonX, *report.Context) {
	// parse → resolve → type check
	tmp := t.TempDir()
	file := filepath.Join(tmp, "test.obx")
	if err := os.WriteFile(file, []byte(src), 0644); err != nil {
		panic(err)
	}

	obx := ast.NewOberonX()
	srcMgr := report.NewSourceManager()
	reporter := report.NewBufferedReporter(srcMgr, 32, report.StdoutSink{
		Source: srcMgr,
		Writer: os.Stdout,
	})
	ctx := &report.Context{
		FileName:        file,
		FilePath:        file,
		Content:         []byte(src),
		Source:          srcMgr,
		Reporter:        reporter,
		TabWidth:        4,
		Env:             ast.NewEnv(),
		Names:           adt.NewStack[string](),
		ExprLists:       adt.NewStack[[]ast.Expression](),
		SymbolOverrides: map[string]ast.Symbol{},
		TypeOverrides:   map[string]types.Type{},
	}
	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		return nil, ctx
	}

	obx.AddUnit(unit)
	s := sema.NewSema(ctx, obx)
	s.Validate()

	return obx, ctx
}

func TestHIRGen(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected *Module
	}{
		{
			name: "RepeatUntil",
			source: `
        MODULE Test;
        VAR x: INTEGER;
        BEGIN
          REPEAT
            x := x + 1
          UNTIL x > 10
        END Test.`,
			expected: &Module{
				Name:  "Test",
				Decls: []Decl{&VariableDecl{Name: "x", Type: types.Int32Type}},
				Init: &ProcedureDecl{
					Name: "__init_Test",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&LoopStmt{
								label: "repeat.loop.0",
								Body: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Left: &Variable{Ident: "x", Ty: types.Int32Type},
											Right: &BinaryExpr{
												Op: token.PLUS,
												Left: &Variable{
													Ident: "x",
													Ty:    types.Int32Type,
												},
												Right: &Literal{
													Value: "1",
													Kind:  token.BYTE_LIT,
													Ty:    types.ByteType,
												},
												Ty: types.Int32Type,
											},
										},
										&IfStmt{
											Cond: &BinaryExpr{
												Op:    token.GREAT,
												Left:  &Variable{Ident: "x", Ty: types.Int32Type},
												Right: &Literal{Kind: token.BYTE_LIT, Value: "10", Ty: types.ByteType},
												Ty:    types.BooleanType,
											},
											Then: &CompoundStmt{
												Stmts: []Stmt{
													&ExitStmt{loopLabel: "repeat.loop.0"},
												},
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "WhileLoop",
			source: `
    MODULE M;
    VAR i: INTEGER;
    BEGIN
      WHILE i < 5 DO
        i := i + 1
      END
    END M.
  `,
			expected: &Module{
				Name: "M",
				Decls: []Decl{
					&VariableDecl{Name: "i", Type: types.Int32Type},
				},
				Init: &ProcedureDecl{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&LoopStmt{
								label: "while.loop.0",
								Body: &CompoundStmt{
									Stmts: []Stmt{
										&IfStmt{
											Cond: &UnaryExpr{
												Op: token.NOT,
												Operand: &BinaryExpr{
													Op:    token.LESS,
													Left:  &Variable{Ident: "i", Ty: types.Int32Type},
													Right: &Literal{Kind: token.BYTE_LIT, Value: "5", Ty: types.ByteType},
													Ty:    types.BooleanType,
												},
											},
											Then: &CompoundStmt{Stmts: []Stmt{&ExitStmt{loopLabel: "while.loop.0"}}},
										},
										&AssignStmt{
											Left: &Variable{Ident: "i", Ty: types.Int32Type},
											Right: &BinaryExpr{
												Op:    token.PLUS,
												Left:  &Variable{Ident: "i", Ty: types.Int32Type},
												Right: &Literal{Kind: token.BYTE_LIT, Value: "1", Ty: types.ByteType},
												Ty:    types.Int32Type,
											},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "IfThenElse",
			source: `
    MODULE M;
    VAR a: INTEGER;
    BEGIN
      IF a = 0 THEN a := 1 ELSE a := 2 END
    END M.
  `,
			expected: &Module{
				Name: "M",
				Decls: []Decl{
					&VariableDecl{Name: "a", Type: types.Int32Type},
				},
				Init: &ProcedureDecl{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&IfStmt{
								Cond: &BinaryExpr{
									Op:    token.EQUAL,
									Left:  &Variable{Ident: "a", Ty: types.Int32Type},
									Right: &Literal{Kind: token.BYTE_LIT, Value: "0", Ty: types.ByteType},
									Ty:    types.BooleanType,
								},
								Then: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Left:  &Variable{Ident: "a", Ty: types.Int32Type},
											Right: &Literal{Kind: token.BYTE_LIT, Value: "1", Ty: types.ByteType},
										},
									},
								},
								Else: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Left:  &Variable{Ident: "a", Ty: types.Int32Type},
											Right: &Literal{Kind: token.BYTE_LIT, Value: "2", Ty: types.ByteType},
										},
									},
								},
							},
						},
					},
				},
			},
		},
		{
			name: "ProcedureCall",
			source: `
    MODULE M;
    PROCEDURE Write(x: INTEGER); END Write;
    BEGIN Write(42) END M.
  `,
			expected: &Module{
				Name: "M",
				Decls: []Decl{
					&ProcedureDecl{
						Name:   "Write",
						Params: []*Param{{Name: "x", Type: types.Int32Type}},
					},
				},
				Init: &ProcedureDecl{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&ProcedureCallStmt{
								Proc: &Procedure{Ident: "Write", Ty: &types.ProcedureType{
									Params:          []*types.FormalParam{{Name: "x", Type: types.Int32Type, Kind: "VALUE"}},
									Result:          nil,
									IsTypeBoundType: false,
								}},
								Args: []Expr{&Literal{Kind: token.BYTE_LIT, Value: "42", Ty: types.ByteType}},
							},
						},
					},
				},
			},
		},
		{
			name: "",
			source: `
MODULE M;

TYPE
  T = POINTER TO RECORD END;

PROCEDURE (t: T) Act*;
END Act;

END M.
`,
			expected: &Module{
				Name: "M",
				Decls: []Decl{
					&TypeDecl{
						Name: "T",
						Type: &types.PointerType{
							Base: &types.RecordType{
								Base: nil,
								Fields: map[string]*types.Field{
									"Act": {
										Name:       "Act",
										IsExported: true,
									},
								},
							},
						},
					},
					&ProcedureDecl{
						Name: "Act",
						Params: []*Param{
							{
								Name: "t",
								Type: &types.NamedType{
									Name: "T",
									Def: &types.PointerType{
										Base: &types.RecordType{
											Base: nil,
											Fields: map[string]*types.Field{
												"Act": {
													Name:       "Act",
													IsExported: true,
												},
											},
										},
									},
								},
							},
						},
						Props: ast.Exported,
					},
				},
				Init: &ProcedureDecl{
					Name: "__init_M",
					Body: &CompoundStmt{},
				},
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obx, ctx := ParseAndCheck(t, tt.source)
			hirGen := &Generator{ctx: ctx}

			hirModule := obx.Units[0].Accept(hirGen).(*Module)

			if diff := cmp.Diff(tt.expected, hirModule,
				cmp.AllowUnexported(
					Module{}, ProcedureDecl{}, VariableDecl{},
					LoopStmt{}, CompoundStmt{}, AssignStmt{},
					Variable{}, Literal{}, BinaryExpr{},
					IfStmt{}, ExitStmt{}, Procedure{},
				),
				cmpopts.IgnoreFields(Variable{}, "MangledName", "Props"),
				cmpopts.IgnoreFields(Procedure{}, "MangledName", "Props"),
				cmpopts.IgnoreFields(Param{}, "Type"),
				cmpopts.IgnoreFields(types.Field{}, "Type"),
			); diff != "" {
				t.Errorf("HIR mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
