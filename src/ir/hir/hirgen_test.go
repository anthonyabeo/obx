package hir

import (
	"github.com/google/go-cmp/cmp"
	"os"
	"path/filepath"
	//"strings"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
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
				Name:    "Test",
				Globals: []Decl{&VarDecl{Name: "x", Type: &IntType{Bits: 32, Signed: true}}},
				Init: &Procedure{
					Name: "__init_Test",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&LoopStmt{
								label: "repeat.loop.0",
								Body: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Lhs: &VarExpr{Name: "x", Ty: &IntType{Bits: 32, Signed: true}},
											Rhs: &BinaryExpr{
												Op: "+",
												Left: &VarExpr{
													Name: "x",
													Ty:   &IntType{Bits: 32, Signed: true},
												},
												Right: &IntConst{
													Value: 1,
													Ty:    &IntType{Bits: 8, Signed: false},
												},
												Ty: &IntType{Bits: 32, Signed: true},
											},
										},
										&IfStmt{
											Cond: &BinaryExpr{
												Op:    ">",
												Left:  &VarExpr{Name: "x", Ty: &IntType{Bits: 32, Signed: true}},
												Right: &IntConst{Value: 10, Ty: &IntType{Bits: 8, Signed: false}},
												Ty:    &BoolType{},
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
				Globals: []Decl{
					&VarDecl{Name: "i", Type: &IntType{Bits: 32, Signed: true}},
				},
				Init: &Procedure{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&LoopStmt{
								label: "while.loop.0",
								Body: &CompoundStmt{
									Stmts: []Stmt{
										&IfStmt{
											Cond: &UnaryExpr{
												Op: Not,
												E: &BinaryExpr{
													Op:    "<",
													Left:  &VarExpr{Name: "i", Ty: &IntType{Bits: 32, Signed: true}},
													Right: &IntConst{Value: 5, Ty: &IntType{Bits: 8, Signed: false}},
													Ty:    &BoolType{},
												},
											},
											Then: &CompoundStmt{Stmts: []Stmt{&ExitStmt{loopLabel: "while.loop.0"}}},
										},
										&AssignStmt{
											Lhs: &VarExpr{Name: "i", Ty: &IntType{Bits: 32, Signed: true}},
											Rhs: &BinaryExpr{
												Op:    "+",
												Left:  &VarExpr{Name: "i", Ty: &IntType{Bits: 32, Signed: true}},
												Right: &IntConst{Value: 1, Ty: &IntType{Bits: 8, Signed: false}},
												Ty:    &IntType{Bits: 32, Signed: true},
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
				Globals: []Decl{
					&VarDecl{Name: "a", Type: &IntType{Bits: 32, Signed: true}},
				},
				Init: &Procedure{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&IfStmt{
								Cond: &BinaryExpr{
									Op:    "=",
									Left:  &VarExpr{Name: "a", Ty: &IntType{Bits: 32, Signed: true}},
									Right: &IntConst{Value: 0, Ty: &IntType{Bits: 8, Signed: false}},
									Ty:    &BoolType{},
								},
								Then: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Lhs: &VarExpr{Name: "a", Ty: &IntType{Bits: 32, Signed: true}},
											Rhs: &IntConst{Value: 1, Ty: &IntType{Bits: 8, Signed: false}},
										},
									},
								},
								Else: &CompoundStmt{
									Stmts: []Stmt{
										&AssignStmt{
											Lhs: &VarExpr{Name: "a", Ty: &IntType{Bits: 32, Signed: true}},
											Rhs: &IntConst{Value: 2, Ty: &IntType{Bits: 8, Signed: false}},
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
				Procedures: []*Procedure{
					{
						Name:   "Write",
						Params: []*Param{{Name: "x", Type: &IntType{Bits: 32, Signed: true}}},
					},
				},
				Init: &Procedure{
					Name: "__init_M",
					Body: &CompoundStmt{
						Stmts: []Stmt{
							&CallStmt{
								Proc: &VarExpr{Name: "Write"},
								Args: []Expr{&IntConst{Value: 42, Ty: &IntType{Bits: 8, Signed: false}}},
							},
						},
					},
				},
			},
		},
		// add more test cases
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obx, ctx := ParseAndCheck(t, tt.source)
			hirGen := &Generator{ctx: ctx}

			hirModule := obx.Units[0].Accept(hirGen).(*Module)

			if diff := cmp.Diff(tt.expected, hirModule,
				cmp.AllowUnexported(
					Module{}, Procedure{}, VarDecl{},
					LoopStmt{}, CompoundStmt{}, AssignStmt{},
					VarExpr{}, IntConst{}, BinaryExpr{},
					IfStmt{}, ExitStmt{},
				),
			); diff != "" {
				t.Errorf("HIR mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
