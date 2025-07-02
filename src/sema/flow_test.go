package sema

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/types"
)

type TestCase struct {
	Name     string
	Source   string
	WantErrs []string
}

func TestLoopLabeling(t *testing.T) {
	tests := []TestCase{
		{
			Name: "valid_exit_while",
			Source: `
MODULE M;
BEGIN
  WHILE TRUE DO
    IF x THEN
      EXIT
    END
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "invalid_exit_top_level",
			Source: `
MODULE M;
BEGIN
  EXIT
END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "exit_in_nested_loop",
			Source: `
MODULE M;
BEGIN
  WHILE cond DO
    LOOP
      IF x THEN EXIT END
    END
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_in_repeat",
			Source: `
MODULE M;
BEGIN
  REPEAT
    EXIT
  UNTIL done
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_in_proc_outside_loop",
			Source: `
MODULE M;
PROCEDURE Foo;
BEGIN
  EXIT
END Foo

END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "exit_in_for_loop",
			Source: `
MODULE M;
BEGIN
  FOR i := 0 TO 10 DO
    IF i = 5 THEN
      EXIT
    END
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "nested_proc_with_exit",
			Source: `
MODULE M;
PROCEDURE Inner;
BEGIN
  WHILE TRUE DO
    EXIT
  END
END Inner

END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_outside_all_loops_nested_blocks",
			Source: `
MODULE M;
BEGIN
  IF TRUE THEN
    EXIT
  END
END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "multiple_invalid_exits",
			Source: `
MODULE M;
BEGIN
  EXIT
  IF TRUE THEN
    EXIT
  END
END M.
`,
			WantErrs: []string{
				"Exit statement outside of loop",
				"Exit statement outside of loop",
			},
		},
		{
			Name: "exit_inside_procedure_inside_loop",
			Source: `
MODULE M;
PROCEDURE DoSomething;
BEGIN
  EXIT
END DoSomething

BEGIN
  WHILE TRUE DO
    DoSomething()
  END
END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "exit_in_case_statement",
			Source: `
MODULE M;
BEGIN
  CASE x OF
    1: EXIT
  END
END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "exit_in_inner_of_two_loops",
			Source: `
MODULE M;
BEGIN
  WHILE x DO
    LOOP
      EXIT  (* should exit inner LOOP *)
    END
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_in_outer_after_inner_loop",
			Source: `
MODULE M;
BEGIN
  WHILE x DO
    LOOP
      IF y THEN EXIT END  (* inner loop exit *)
    END
    EXIT  (* outer loop exit *)
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_outside_both_loops",
			Source: `
MODULE M;
BEGIN
  WHILE x DO
    LOOP
		a := 5
      (* no EXIT here *)
    END
  END
  EXIT  (* invalid *)
END M.
`,
			WantErrs: []string{"Exit statement outside of loop"},
		},
		{
			Name: "exit_in_inner_repeat_loop",
			Source: `
MODULE M;
BEGIN
  WHILE x DO
    REPEAT
      EXIT  (* valid, exits REPEAT *)
    UNTIL y
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_in_inner_for_loop",
			Source: `
MODULE M;
BEGIN
  LOOP
    FOR i := 0 TO 5 DO
      EXIT  (* exits FOR *)
    END
    EXIT  (* exits LOOP *)
  END
END M.
`,
			WantErrs: nil,
		},
		{
			Name: "exit_exits_closest_loop",
			Source: `
MODULE M;
BEGIN
  REPEAT
    LOOP
      WHILE TRUE DO
        EXIT  (* should exit WHILE *)
      END
    END
  UNTIL done
END M.
`,
			WantErrs: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			src := []byte(tt.Source)

			// Set up source manager and reporter
			filename := "test.obx"
			mgr := report.NewSourceManager()
			ctx := &report.Context{
				FileName: filename,
				Content:  src,
				Env:      ast.NewEnv(),
				Source:   mgr,
				Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
					Source: mgr,
					Writer: os.Stdout,
				}),
				TabWidth:  4,
				Names:     adt.NewStack[string](),
				ExprLists: adt.NewStack[[]ast.Expression](),
			}

			p := parser.NewParser(ctx)
			unit := p.Parse()
			if ctx.Reporter.ErrorCount() > 0 {
				t.Error("found parse errors")
				ctx.Reporter.Flush()
			}

			// Run semantic analysis: loop labeling
			labeler := NewFlowChecker(ctx)
			labeler.Analyse(unit)

			// Filter diagnostics
			diagnostics := ctx.Reporter.Diagnostics()
			if len(diagnostics) != len(tt.WantErrs) {
				t.Errorf("Expected %d errors, got %d", len(tt.WantErrs), len(diagnostics))
				for _, d := range diagnostics {
					t.Logf("Got: %s", d.Message)
				}
				return
			}

			for i, want := range tt.WantErrs {
				if !strings.Contains(diagnostics[i].Message, want) {
					t.Errorf("Error %d: expected message containing %q, got %q", i, want, diagnostics[i].Message)
				}
			}
		})
	}
}

func TestFlowCheckerProcedureReturn(t *testing.T) {
	tests := []TestCase{
		//{
		//	Name: "Function with missing RETURN",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(): INTEGER;
		//		  (* no return here *)
		//		END F;
		//	END M.`,
		//	WantErrs: []string{
		//		`missing RETURN in function F`,
		//	},
		//},
		//{
		//	Name: "Function with valid RETURN",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(): INTEGER;
		//		BEGIN
		//		  RETURN 1
		//		END F;
		//	END M.`,
		//},
		//{
		//	Name: " Proper procedure with a RETURN value (invalid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  RETURN 1
		//		END P;
		//	END M.
		//	`,
		//	WantErrs: []string{
		//		`RETURN value in non-function procedure`,
		//	},
		//},
		//{
		//	Name: "Function without return value (invalid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(): INTEGER;
		//		BEGIN
		//		  RETURN
		//		END F;
		//	END M.
		//	`,
		//	WantErrs: []string{
		//		`missing RETURN value in function`,
		//	},
		//},
		//{
		//	Name: "Proper procedure with RETURN (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  RETURN
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "EXIT inside loop (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  WHILE TRUE DO
		//			EXIT
		//		  END
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "EXIT outside loop (invalid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  EXIT
		//		END P;
		//	END M.
		//	`,
		//	WantErrs: []string{`Exit statement outside of loop`},
		//},
		//{
		//	Name: "Nested loops and valid EXIT",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  LOOP
		//			LOOP
		//			  EXIT
		//			END
		//		  END
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: " EXIT in FOR, WHILE, LOOP — all valid",
		//	Source: `
		//		MODULE M;
		//		VAR i: INTEGER;
		//		PROCEDURE P;
		//		BEGIN
		//		  FOR i := 0 TO 5 DO EXIT END;
		//		  WHILE i < 5 DO EXIT END;
		//		  LOOP EXIT END
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: " Function with conditional RETURN (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(x: INTEGER): INTEGER;
		//		BEGIN
		//		  IF x < 0 THEN
		//			RETURN -1
		//		  ELSE
		//			RETURN x
		//		  END
		//		END F;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "Function with conditional RETURN (missing return path)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(x: INTEGER): INTEGER;
		//		BEGIN
		//		  IF x < 0 THEN
		//			RETURN -1
		//		  END
		//		END F;
		//	END M.
		//	`,
		//	WantErrs: []string{`missing RETURN in function F`},
		//},
		//{
		//	Name: "EXIT inside IF but not inside loop (invalid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  IF TRUE THEN
		//			EXIT
		//		  END
		//		END P;
		//	END M.`,
		//	WantErrs: []string{`EXIT outside loop`},
		//},
		//{
		//	Name: "EXIT inside nested IF inside loop (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  LOOP
		//			IF TRUE THEN
		//			  EXIT
		//			END
		//		  END
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "Function with RETURN in only one branch of CASE (invalid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(x: INTEGER): INTEGER;
		//		BEGIN
		//		  CASE x OF
		//			0: RETURN 0
		//		  ELSE
		//			(* missing return here *)
		//		  END
		//		END F;
		//	END M.
		//	`,
		//	WantErrs: []string{`missing RETURN in function F`},
		//},
		//{
		//	Name: "Function with RETURN in all CASE branches (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE F(x: INTEGER): INTEGER;
		//		BEGIN
		//		  CASE x OF
		//			0: RETURN 0
		//		  ELSE
		//			RETURN 1
		//		  END
		//		END F;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "Multiple EXIT in deeply nested loop/IF blocks (valid)",
		//	Source: `
		//		MODULE M;
		//		PROCEDURE P;
		//		BEGIN
		//		  LOOP
		//			IF TRUE THEN
		//			  IF FALSE THEN
		//				EXIT
		//			  ELSE
		//				EXIT
		//			  END
		//			END
		//		  END
		//		END P;
		//	END M.
		//	`,
		//},
		//{
		//	Name: "EXIT in WITH statement inside LOOP (valid)",
		//	Source: `
		//		MODULE M;
		//		TYPE R = RECORD a: INTEGER END;
		//		VAR p: POINTER TO R;
		//		PROCEDURE P;
		//		BEGIN
		//		  LOOP
		//			WITH p^: r DO
		//			  IF r.a = 0 THEN EXIT END
		//			END
		//		  END
		//		END P;
		//	END M.
		//	`,
		//},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			ctx := flowCheckSnippet(t, tt.Source)

			diags := ctx.Reporter.Diagnostics()
			if len(diags) != len(tt.WantErrs) {
				t.Errorf("got %d diagnostics, want %d", len(diags), len(tt.WantErrs))
				for _, d := range diags {
					t.Logf("diag: %s", d.Message)
				}
				return
			}
			for i, want := range tt.WantErrs {
				if !strings.Contains(diags[i].Message, want) {
					t.Errorf("diagnostic %d = %q, want substring %q", i, diags[i].Message, want)
				}
			}
		})
	}
}

func flowCheckSnippet(t *testing.T, code string) *report.Context {
	tmp := t.TempDir()
	file := filepath.Join(tmp, "test.obx")
	if err := os.WriteFile(file, []byte(code), 0644); err != nil {
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
		Content:         []byte(code),
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
		return ctx
	}

	obx.AddUnit(unit)
	s := NewSema(ctx, obx)
	s.Validate()

	return ctx
}
