package codegen

import (
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/codegen/target/riscv"
	"github.com/anthonyabeo/obx/src/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/support/testutil"
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
		{
			name: "RecordFieldAccess",
			input: `
				MODULE RecordTest;
				TYPE Point = RECORD 
						x, y: INTEGER;
						z: INT64
				END;
				VAR p: Point;
				
				BEGIN
					p.x := 10;
					p.y := 20;
					p.z := p.x + p.y;
					printf("Points: (%d, %d, %d)\n", p.x, p.y, p.z)
				END RecordTest.
				`,
			filename: "record_field_access.obx",
		},
		{
			name: "DerefLowering",
			input: `
				MODULE DerefTest;
				VAR p: POINTER TO RECORD x, y: INTEGER END;
				VAR x: INTEGER;
				BEGIN
					NEW(p);
					x := 42;
					p^.x := x;
					x := p^.y
				END DerefTest.
			`,
			filename: "deref_test.obx",
		},
		{
			name: "PointerToFixedArray",
			input: `
				MODULE PtrArrTest;
				VAR p: ARRAY 5 OF INTEGER;
				BEGIN
					NEW(p)
					p[0] := 999
					printf("p[0] x: %d\n", p[0])
				END PtrArrTest.
			`,
			filename: "ptr_arr_test.obx",
		},
		{
			name: "NewOnOpenArray",
			input: `
				MODULE NewOpenArrayTest;
				VAR arr: POINTER TO ARRAY OF INTEGER;
		
				BEGIN
					NEW(arr, 10)
					arr[0] := 42
					arr[1] := 84
					arr[2] := 126
					printf("arr: [%d, %d, %d]\n", arr[0], arr[1], arr[2])
				END NewOpenArrayTest.
			`,
			filename: "new_open_array_test.obx",
		},
		{
			name: "SetInOperation",
			input: `
		MODULE SetInTest;
		VAR s: SET;
		VAR x: INTEGER;
		BEGIN
			s := {1, 3, 5, 7, 9 .. 15};
			IF 16 IN s THEN
				x := 42
			ELSE
				x := 0
			END
			printf("x: %d\n", x)
		END SetInTest.
	`,
			filename: "set_in_test.obx",
		},
		{
			name: "SetArithOperations",
			input: `
				MODULE SetOpsTest;
				VAR A, B, C: SET;
				VAR x: INTEGER;
				BEGIN
					A := {1,2,3,5 .. 7};
					B := {3,4,5,8};
					(* union *)
					C := A + B;
					(* intersection *)
					C := A * B; 
					(* difference 
					(* C := A - B; *)
					IF 1 IN C THEN
						x := 100
					ELSE
						x := 0
					END
					printf("x: %d\n", x)
				END SetOpsTest.
				`,
			filename: "set_ops_test.obx",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mgr := source.NewSourceManager()
			reporter := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
			ctx := compiler.New(tc.filename, mgr, reporter, ast.NewEnv(), 8)

			program := testutil.ParseSourceAndLowerToMIR(t, ctx, tc.filename, []byte(tc.input))
			for _, module := range program.Modules {
				for _, function := range module.Funcs {
					opt.BuildCFG(function)
				}
			}

			for _, module := range program.Modules {
				root, err := project.FindProjectRoot()
				if err != nil {
					t.Fatalf("failed to find project root: %v", err)
				}

				path := root + "/out/" + module.Name + ".s"
				asmFile, err := os.Create(path)
				if err != nil {
					t.Fatalf("failed to create assembly file: %v", err)
				}
				defer asmFile.Close()

				asm, err := Compile(module, riscv.NewRV64IMAFDTarget(), root+"/src/codegen/target/desc", CompileOptions{})
				if err != nil {
					t.Fatalf("Compile failed: %v", err)
				}
				if _, err := asmFile.WriteString(asm + "\n\n"); err != nil {
					t.Errorf("failed to write to assembly file: %v", err)
				}
			}
		})
	}
}
