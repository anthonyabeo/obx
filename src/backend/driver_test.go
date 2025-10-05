package backend

import (
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

				asm := Compile(module, riscv.NewRV64IMAFDTarget())
				if _, err := asmFile.WriteString(asm + "\n\n"); err != nil {
					t.Errorf("failed to write to assembly file: %v", err)
				}
			}
		})
	}
}
