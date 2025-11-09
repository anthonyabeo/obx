package backend

import (
	"github.com/anthonyabeo/obx/src/opt"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/modgraph"
	"github.com/anthonyabeo/obx/src/backend/target/riscv"
	"github.com/anthonyabeo/obx/src/format"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestCompile(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		filename string
	}{
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
		{
			name: "ProcedureAndCall",
			input: `
				MODULE TestCall;
				
				VAR
				  result: INTEGER;
				
				PROCEDURE AddMany(a, b, c, d, e, f, g, h, i, j: INTEGER): INTEGER;
				VAR
				  sum, temp: INTEGER;
				BEGIN
				  sum := a + b + c + d + e + f + g + h + i + j;
				  temp := sum * 2;
				  RETURN temp
				END AddMany;
				
				BEGIN
				  result := AddMany(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
 					printf("Final x: %d\n", result)
				END TestCall.`,
			filename: "procedure_and_call.obx",
		},
		{
			name: "MultipleStatementsAndLoop",
			input: `
			 MODULE LoopTest;
			 VAR x, y: INTEGER;

			 BEGIN
			  x := 0;
			  y := 20;
			  WHILE x < y DO
			   x := x + 1
			  END
		       printf("Final x: %d\n", x)
			 END LoopTest.
			`,
			filename: "loop_test.obx",
		},
		{
			name: "VarInValueParams",
			input: `
				MODULE ParamTest;
				VAR x, y: INTEGER;
				
				PROCEDURE Foo(VAR a: INTEGER; IN b: INTEGER; c: INTEGER; d: INTEGER);
				BEGIN
					a := a + b + c + d
				END Foo;
				
				BEGIN
					x := 1;
					y := 2;
					Foo(x, y, 3, x)
		       		printf("Final x: %d\n", x)

				END ParamTest.
				`,
			filename: "param_test.obx",
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

			program := format.ParseSourceAndLowerToMIR(t, ctx)
			for _, module := range program.Modules {
				for _, function := range module.Funcs {
					opt.BuildCFG(function)
				}
			}

			for _, module := range program.Modules {
				root, err := modgraph.FindProjectRoot()
				if err != nil {
					t.Fatalf("failed to find project root: %v", err)
				}

				path := root + "/out/" + module.Name + ".s"
				asmFile, err := os.Create(path)
				if err != nil {
					t.Fatalf("failed to create assembly file: %v", err)
				}
				defer asmFile.Close()

				asm := Compile(module, riscv.NewRV64IMAFDTarget(), root+"/src/backend/target/desc")
				if _, err := asmFile.WriteString(asm + "\n\n"); err != nil {
					t.Errorf("failed to write to assembly file: %v", err)
				}
			}
		})
	}
}
