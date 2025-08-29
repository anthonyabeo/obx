package format

import (
	"os"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestFormatMIRPrograms(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		filename string
	}{
		{
			name: "SampleProgram",
			input: `
			 MODULE Test;
			 VAR x: INTEGER;
			 BEGIN x := 42
			 END Test.
			`,
			filename: "test.obx",
		},
		{
			name: "ProcedureAndCall",
			input: `
			 MODULE ProcTest;
			 VAR x: INTEGER;

			 PROCEDURE Inc;
			 BEGIN
			  x := x + 1
			 END Inc;

			 BEGIN
			  Inc()
			 END ProcTest.
			`,
			filename: "proc_test.obx",
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
				return y
			 END IfTest.
			`,
			filename: "if_test.obx",
		},
		{
			name: "NestedProcedures",
			input: `
			 MODULE NestedProcTest;
			 VAR x: INTEGER;

			 PROCEDURE Outer;
			  VAR y: INTEGER;
			  PROCEDURE Inner;
			  BEGIN
			   y := y + 1
			  END Inner;
			 BEGIN
			  Inner()
			 END Outer;

			 BEGIN
			  Outer()
			 END NestedProcTest.
			`,
			filename: "nested_proc_test.obx",
		},
		{
			name: "EmptyModule",
			input: `
			 MODULE Empty;
			 END Empty.
			`,
			filename: "empty.obx",
		},
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
			name: "TwoDIntArray",
			input: `
		  MODULE Array2DTest;
		  VAR mat: ARRAY 3, 3 OF INTEGER;
		  VAR i, j: INTEGER;
		
		  BEGIN
		   i := 0;
		   WHILE i < 3 DO
		    j := 0;
		    WHILE j < 3 DO
		     mat[i, j] := i * j;
		     j := j + 1
		    END;
		    i := i + 1
		   END
		  END Array2DTest.
		 `,
			filename: "array2d_test.obx",
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

			program := ParseSourceAndLowerToMIR(t, ctx)

			if err := EmitMIR(os.Stdout, program); err != nil {
				t.Fatalf("EmitMIR failed: %v", err)
			}
		})
	}
}
