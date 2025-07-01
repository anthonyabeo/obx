package sema

import (
	"os"
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

type loopTest struct {
	Name     string
	Source   string
	WantErrs []string
}

func TestLoopLabeling(t *testing.T) {
	tests := []loopTest{
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
			labeler := NewFlowControlAnalyzer(ctx)
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
