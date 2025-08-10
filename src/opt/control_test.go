package opt

import (
	"fmt"
	"os"
	"os/exec"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/cmd/cli"
	"github.com/anthonyabeo/obx/src/pprint/mir"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestBuildCFG(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		filename string
	}{
		{
			name: "RecursiveProcedureCall",
			input: `module Main
 var res: integer
 proc fib(n : integer): integer
  var a, b: integer

   begin
  if (n = 0) or (n = 1) then
   return n
  else
    a := fib(n - 1)
    b := fib(n - 2)
    return a + b
  end
   end fib

begin
 res := fib(21)
   assert(res = 10946)
end Main
`,
			filename: "main.obx",
		},
		{
			name: "IfStatement",
			input: `
			 MODULE IfTest;
			 VAR x, y: INTEGER;

			 BEGIN
			  x := 5;
			  y := 0;
			  IF x > 0 THEN
			   y := 1
			  ELSE
			   y := -1
			  END
			 END IfTest.
			`,
			filename: "if_test.obx",
		},
		{
			name: "MultipleStatementsAndLoop",
			input: `
			 MODULE LoopTest;
			 VAR x, y: INTEGER;

			 BEGIN
			  x := 0;
			  y := 10;
			  WHILE x < y DO
			   x := x + 1
			  END
			 END LoopTest.
			`,
			filename: "loop_test.obx",
		},
		{
			name: "IfElseAndWhile",
			input: `
  MODULE ControlFlowTest;
  VAR x, y: INTEGER;

  PROCEDURE Foo;
  VAR z: INTEGER;
  BEGIN
   z := 0;
   IF x > 0 THEN
    z := z + 1
   ELSE
    z := z - 1
   END;
   WHILE z < 10 DO
    z := z + 2
   END
  END Foo;

  BEGIN
   x := 5;
   y := 0;
   Foo()
  END ControlFlowTest.
 `,
			filename: "control_flow_test.obx",
		},
		{
			name: "NestedIfAndBreakInLoop",
			input: `
  MODULE ComplexFlow;
  VAR x, y: INTEGER;

  PROCEDURE Bar;
  VAR i: INTEGER;
  BEGIN
   i := 0;
   WHILE i < 10 DO
    IF i = 5 THEN
     y := i;
     EXIT
    ELSE
     x := x + i
    END;
    i := i + 1
   END
  END Bar;

  BEGIN
   x := 0;
   y := -1;
   Bar()
  END ComplexFlow.
 `,
			filename: "complex_flow_test.obx",
		},
		{
			name: "RepeatUntilWithIf",
			input: `
  MODULE RepeatUntilTest;
  VAR x, y: INTEGER;

  PROCEDURE Baz;
  VAR i: INTEGER;
  BEGIN
   i := 0;
   REPEAT
    IF i MOD 2 = 0 THEN
     x := x + i
    ELSE
     y := y + i
    END;
    i := i + 1
   UNTIL i >= 5
  END Baz;

  BEGIN
   x := 0;
   y := 0;
   Baz()
  END RepeatUntilTest.
 `,
			filename: "repeat_until_test.obx",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mgr := report.NewSourceManager()
			ctx := &report.Context{
				FileName: tc.filename,
				Content:  []byte(tc.input),
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

			program := mir.ParseSourceAndLowerToMIR(t, ctx)

			for _, module := range program.Modules {
				for _, function := range module.Funcs {
					BuildCFG(function)
					CleanCFG(function)

					dot := function.OutputDOT()

					Root, err := cli.FindProjectRoot()
					if err != nil {
						t.Errorf("failed to find project root: %s", err)
						continue
					}

					dotFile := fmt.Sprintf("%s/out/%s.cfg.dot", Root, function.Name)
					pngFile := fmt.Sprintf("%s/out/%s.cfg.png", Root, function.Name)

					if err := os.WriteFile(dotFile, []byte(dot), 0644); err != nil {
						t.Errorf("failed to write dot: %s", err)
						continue
					}

					cmd := exec.Command("dot", "-Tpng", dotFile, "-o", pngFile)
					if err := cmd.Run(); err != nil {
						t.Errorf("failed to run dot: %s", err)
					} else {
						fmt.Printf("Generated %s\n", pngFile)
					}
				}
			}
		})
	}
}
