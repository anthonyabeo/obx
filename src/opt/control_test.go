package opt

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/support/testutil"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func findProjectRoot(start string) (string, error) {
	dir, err := filepath.Abs(start)
	if err != nil {
		return "", err
	}
	const maxDepth = 100
	for i := 0; i < maxDepth; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			// reached filesystem root without finding go.mod
			break
		}
		dir = parent
	}
	return "", fmt.Errorf("go.mod not found from %s", start)
}

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
				return y
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
              printf("Final x: %d\n", x)
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
				END TestCall.`,
			filename: "procedure_and_call.obx",
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
					p[0] := 42
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
					printf("arr[0] x: %d\n", arr[0])
				END NewOpenArrayTest.
			`,
			filename: "new_open_array_test.obx",
		},
		{
			name: "SimpleCaseStatement",
			input: `
				MODULE CaseTest;
				VAR x, a: INTEGER;
				BEGIN
					x := 3;
					CASE x OF
						1, 3: a := 1 |
						5 .. 7: a := 2
					ELSE
						a := 0
					END
				END CaseTest.
			`,
			filename: "case_test.obx",
		},
		{
			name: "SetInOperation",
			input: `
		MODULE SetInTest;
		VAR s: SET;
		VAR x: INTEGER;
		BEGIN
			s := {1, 3, 5, 7, 9 .. 15};
			IF 10 IN s THEN
				x := 42
			ELSE
				x := 0
			END
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
					(* C := A + B; *)
					(* intersection *)
					(* C := A * B; *)
					(* difference *)
					C := A - B;
					IF 5 IN C THEN
						x := 100
					ELSE
						x := 0
					END
				END SetOpsTest.
				`,
			filename: "set_ops_test.obx",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			mgr := source.NewSourceManager()
			ctx := &diag.Context{
				FileName:              tc.filename,
				Content:               []byte(tc.input),
				Env:                   ast.NewEnv(),
				Source:                mgr,
				Reporter:              diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))),
				TargetMachineWordSize: 8,
			}

			program := testutil.ParseSourceAndLowerToMIR(t, ctx)

			projectRoot, err := findProjectRoot(".")
			if err != nil {
				t.Fatalf("could not locate project root: %v", err)
			}

			for _, module := range program.Modules {
				for _, function := range module.Funcs {
					BuildCFG(function)

					dot := function.OutputDOT()

					outDir, err := os.MkdirTemp(projectRoot, "cfg-*")
					if err != nil {
						t.Errorf("failed to create temp directory: %s", err)
						continue
					}

					dotFile := fmt.Sprintf("%s/%s.cfg.dot", outDir, function.FnName)
					pngFile := fmt.Sprintf("%s/%s.cfg.png", outDir, function.FnName)

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

func CFGAlpha() *obxir.Function {
	// Create blocks
	blk0 := &obxir.Block{ID: 0, Label: "B0"}
	blk1 := &obxir.Block{ID: 1, Label: "B1"}
	blk2 := &obxir.Block{ID: 2, Label: "B2"}
	blk3 := &obxir.Block{ID: 3, Label: "B3"}
	blk4 := &obxir.Block{ID: 4, Label: "B4"}
	blk5 := &obxir.Block{ID: 5, Label: "B5"}
	blk6 := &obxir.Block{ID: 6, Label: "B6"}
	blk7 := &obxir.Block{ID: 7, Label: "B7"}
	blk8 := &obxir.Block{ID: 8, Label: "B8"}

	// Connect successors
	blk0.Succs = map[int]*obxir.Block{blk1.ID: blk1}
	blk1.Succs = map[int]*obxir.Block{blk2.ID: blk2, blk5.ID: blk5}
	blk2.Succs = map[int]*obxir.Block{blk3.ID: blk3}
	blk3.Succs = map[int]*obxir.Block{blk4.ID: blk4, blk1.ID: blk1}
	blk4.Succs = map[int]*obxir.Block{}
	blk5.Succs = map[int]*obxir.Block{blk6.ID: blk6, blk8.ID: blk8}
	blk6.Succs = map[int]*obxir.Block{blk7.ID: blk7}
	blk7.Succs = map[int]*obxir.Block{blk3.ID: blk3}
	blk8.Succs = map[int]*obxir.Block{blk7.ID: blk7}

	// Connect predecessors
	blk0.Preds = map[int]*obxir.Block{}
	blk1.Preds = map[int]*obxir.Block{blk0.ID: blk0, blk3.ID: blk3}
	blk2.Preds = map[int]*obxir.Block{blk1.ID: blk1}
	blk3.Preds = map[int]*obxir.Block{blk2.ID: blk2, blk7.ID: blk7}
	blk4.Preds = map[int]*obxir.Block{blk3.ID: blk3}
	blk5.Preds = map[int]*obxir.Block{blk1.ID: blk1}
	blk6.Preds = map[int]*obxir.Block{blk5.ID: blk5}
	blk7.Preds = map[int]*obxir.Block{blk6.ID: blk6, blk8.ID: blk8}
	blk8.Preds = map[int]*obxir.Block{blk5.ID: blk5}

	fn := &obxir.Function{
		FnName: "Bar",
		Blocks: map[int]*obxir.Block{
			blk0.ID: blk0,
			blk1.ID: blk1,
			blk2.ID: blk2,
			blk3.ID: blk3,
			blk4.ID: blk4,
			blk5.ID: blk5,
			blk6.ID: blk6,
			blk7.ID: blk7,
			blk8.ID: blk8,
		},
		Entry: blk0,
		Dom:   obxir.NewDominatorTree(),
	}

	return fn
}

func CFGBravo() *obxir.Function {
	// Create blocks
	entry := &obxir.Block{ID: 1, Label: "Entry"}
	b1 := &obxir.Block{ID: 2, Label: "B1"}
	b2 := &obxir.Block{ID: 3, Label: "B2"}
	b3 := &obxir.Block{ID: 4, Label: "B3"}
	exit := &obxir.Block{ID: 5, Label: "Exit"}

	// Connect successors
	entry.Succs = map[int]*obxir.Block{b1.ID: b1}
	b1.Succs = map[int]*obxir.Block{b2.ID: b2, b3.ID: b3}
	b2.Succs = map[int]*obxir.Block{exit.ID: exit}
	b3.Succs = map[int]*obxir.Block{b2.ID: b2}

	// Connect predecessors
	b1.Preds = map[int]*obxir.Block{entry.ID: entry}
	b2.Preds = map[int]*obxir.Block{b1.ID: b1, b3.ID: b3}
	b3.Preds = map[int]*obxir.Block{b1.ID: b1}
	exit.Preds = map[int]*obxir.Block{b2.ID: b2}

	blocks := make(map[int]*obxir.Block)
	blocks[entry.ID] = entry
	blocks[b1.ID] = b1
	blocks[b2.ID] = b2
	blocks[b3.ID] = b3
	blocks[exit.ID] = exit

	fn := &obxir.Function{
		FnName: "Foo",
		Blocks: blocks,
		Entry:  entry,
	}

	return fn
}

func TestComputeDominators(t *testing.T) {
	tests := []struct {
		name string
		fxn  *obxir.Function
	}{
		{
			name: "eac-9.2.1",
			fxn:  CFGAlpha(),
		},
		{
			name: "demo-eg",
			fxn:  CFGBravo(),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if err := VizCFG(tt.fxn); err != nil {
				t.Errorf(err.Error())
			}

			dom := ComputeDominators(tt.fxn)
			PrintDominators(dom)
		})
	}
}

func TestImmediateDominators(t *testing.T) {
	tests := []struct {
		name string
		fxn  *obxir.Function
	}{
		{
			name: "eac-9.2.1",
			fxn:  CFGAlpha(),
		},
		{
			name: "demo-eg",
			fxn:  CFGBravo(),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dom := ComputeDominators(tt.fxn)
			idom := ImmediateDominators(tt.fxn, dom)
			printIDoms(idom)
		})
	}
}

func TestDominatorTree(t *testing.T) {
	// Create blocks
	entry := &obxir.Block{ID: 1, Label: "Entry"}
	b1 := &obxir.Block{ID: 2, Label: "B1"}
	b2 := &obxir.Block{ID: 3, Label: "B2"}
	b3 := &obxir.Block{ID: 4, Label: "B3"}
	exit := &obxir.Block{ID: 5, Label: "Exit"}

	// Connect successors
	entry.Succs = map[int]*obxir.Block{b1.ID: b1}
	b1.Succs = map[int]*obxir.Block{b2.ID: b2, b3.ID: b3}
	b2.Succs = map[int]*obxir.Block{exit.ID: exit}
	b3.Succs = map[int]*obxir.Block{b2.ID: b2}

	// Connect predecessors
	b1.Preds = map[int]*obxir.Block{entry.ID: entry}
	b2.Preds = map[int]*obxir.Block{b1.ID: b1, b3.ID: b3}
	b3.Preds = map[int]*obxir.Block{b1.ID: b1}
	exit.Preds = map[int]*obxir.Block{b2.ID: b2}

	blocks := make(map[int]*obxir.Block)
	blocks[entry.ID] = entry
	blocks[b1.ID] = b1
	blocks[b2.ID] = b2
	blocks[b3.ID] = b3
	blocks[exit.ID] = exit

	fn := &obxir.Function{
		FnName: "Foo",
		Blocks: blocks,
		Entry:  entry,
	}

	dom := ComputeDominators(fn)
	idom := ImmediateDominators(fn, dom)
	tree := DominatorTree(idom)

	printDomTree(fn.Entry, tree, 0)
}

func TestComputeDF(t *testing.T) {
	tests := []struct {
		name string
		fxn  *obxir.Function
	}{
		{
			name: "eac-9.2.1",
			fxn:  CFGAlpha(),
		},
		{
			name: "demo-eg",
			fxn:  CFGBravo(),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			dom := ComputeDominators(tt.fxn)
			idom := ImmediateDominators(tt.fxn, dom)
			df := ComputeDF(tt.fxn, idom)

			fmt.Println("\nDominance Frontiers:")
			for _, b := range tt.fxn.Blocks {
				fmt.Printf("  %s: {", b.Label)
				for i, f := range df[b] {
					if i > 0 {
						fmt.Print(", ")
					}
					fmt.Print(f.Label)
				}
				fmt.Println("}")
			}
		})
	}
}
