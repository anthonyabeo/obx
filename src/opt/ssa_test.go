package opt

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/support/testutil"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestSSAConstruct(t *testing.T) {
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
		{
			// Regression test: lowerNumberBuiltin previously unconditionally
			// panicked. Verify that NUMBER(v, a) lowers without panicking and
			// emits a call to __obx_bytes(dst, src, SIZE(v)).
			name: "NumberBuiltin_BYTE",
			input: `
  MODULE NumberTest;
  VAR n: BYTE;
      a: ARRAY 1 OF BYTE;
  BEGIN
   NUMBER(n, a)
  END NumberTest.
 `,
			filename: "number_test.obx",
		},
		{
			name: "NumberBuiltin_INT32",
			input: `
  MODULE NumberTest2;
  VAR n: INT32;
      a: ARRAY 4 OF BYTE;
  BEGIN
   NUMBER(n, a)
  END NumberTest2.
 `,
			filename: "number_test2.obx",
		},
		{
			name: "NumberBuiltin_SET",
			input: `
  MODULE NumberTest3;
  VAR s: SET;
      a: ARRAY 4 OF BYTE;
  BEGIN
   NUMBER(s, a)
  END NumberTest3.
 `,
			filename: "number_test3.obx",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mgr := source.NewSourceManager()
			reporter := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
			ctx := compiler.New(tt.filename, mgr, reporter, ast.NewEnv(), 8)

			program := testutil.ParseSourceAndLowerToMIR(t, ctx, tt.filename, []byte(tt.input))
			for _, module := range program.Modules {
				for _, fn := range module.Funcs {
					SSAConstruct(fn)
					if err := VizSSA(fn); err != nil {
						t.Errorf("Error visualizing CFG: %v", err)
					}
				}
			}
		})
	}
}
