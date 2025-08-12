package opt

import (
	"fmt"
	"os"
	"os/exec"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/cmd/cli"
	"github.com/anthonyabeo/obx/src/ir/mir"
	pprint "github.com/anthonyabeo/obx/src/pprint/mir"
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

			program := pprint.ParseSourceAndLowerToMIR(t, ctx)

			for _, module := range program.Modules {
				for _, function := range module.Funcs {
					BuildCFG(function)

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

func CFGAlpha() *mir.Function {
	// Create blocks
	blk0 := &mir.Block{ID: 0, Label: "B0"}
	blk1 := &mir.Block{ID: 1, Label: "B1"}
	blk2 := &mir.Block{ID: 2, Label: "B2"}
	blk3 := &mir.Block{ID: 3, Label: "B3"}
	blk4 := &mir.Block{ID: 4, Label: "B4"}
	blk5 := &mir.Block{ID: 5, Label: "B5"}
	blk6 := &mir.Block{ID: 6, Label: "B6"}
	blk7 := &mir.Block{ID: 7, Label: "B7"}
	blk8 := &mir.Block{ID: 8, Label: "B8"}

	// Connect successors
	blk0.Succs = map[int]*mir.Block{blk1.ID: blk1}
	blk1.Succs = map[int]*mir.Block{blk2.ID: blk2, blk5.ID: blk5}
	blk2.Succs = map[int]*mir.Block{blk3.ID: blk3}
	blk3.Succs = map[int]*mir.Block{blk4.ID: blk4, blk1.ID: blk1}
	blk4.Succs = map[int]*mir.Block{}
	blk5.Succs = map[int]*mir.Block{blk6.ID: blk6, blk8.ID: blk8}
	blk6.Succs = map[int]*mir.Block{blk7.ID: blk7}
	blk7.Succs = map[int]*mir.Block{blk3.ID: blk3}
	blk8.Succs = map[int]*mir.Block{blk7.ID: blk7}

	// Connect predecessors
	blk0.Preds = map[int]*mir.Block{}
	blk1.Preds = map[int]*mir.Block{blk0.ID: blk0, blk3.ID: blk3}
	blk2.Preds = map[int]*mir.Block{blk1.ID: blk1}
	blk3.Preds = map[int]*mir.Block{blk2.ID: blk2, blk7.ID: blk7}
	blk4.Preds = map[int]*mir.Block{blk3.ID: blk3}
	blk5.Preds = map[int]*mir.Block{blk1.ID: blk1}
	blk6.Preds = map[int]*mir.Block{blk5.ID: blk5}
	blk7.Preds = map[int]*mir.Block{blk6.ID: blk6, blk8.ID: blk8}
	blk8.Preds = map[int]*mir.Block{blk5.ID: blk5}

	fn := &mir.Function{
		Name: "Bar",
		Blocks: map[int]*mir.Block{
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
		Dom:   mir.NewDominatorTree(),
	}

	return fn
}

func CFGBravo() *mir.Function {
	// Create blocks
	entry := &mir.Block{ID: 1, Label: "Entry"}
	b1 := &mir.Block{ID: 2, Label: "B1"}
	b2 := &mir.Block{ID: 3, Label: "B2"}
	b3 := &mir.Block{ID: 4, Label: "B3"}
	exit := &mir.Block{ID: 5, Label: "Exit"}

	// Connect successors
	entry.Succs = map[int]*mir.Block{b1.ID: b1}
	b1.Succs = map[int]*mir.Block{b2.ID: b2, b3.ID: b3}
	b2.Succs = map[int]*mir.Block{exit.ID: exit}
	b3.Succs = map[int]*mir.Block{b2.ID: b2}

	// Connect predecessors
	b1.Preds = map[int]*mir.Block{entry.ID: entry}
	b2.Preds = map[int]*mir.Block{b1.ID: b1, b3.ID: b3}
	b3.Preds = map[int]*mir.Block{b1.ID: b1}
	exit.Preds = map[int]*mir.Block{b2.ID: b2}

	blocks := make(map[int]*mir.Block)
	blocks[entry.ID] = entry
	blocks[b1.ID] = b1
	blocks[b2.ID] = b2
	blocks[b3.ID] = b3
	blocks[exit.ID] = exit

	fn := &mir.Function{
		Name:      "Foo",
		Blocks:    blocks,
		Entry:     entry,
		TempMap:   nil,
		SSAInfo:   nil,
		Dom:       nil,
		Constants: nil,
	}

	return fn
}

func TestComputeDominators(t *testing.T) {
	tests := []struct {
		name string
		fxn  *mir.Function
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
		fxn  *mir.Function
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
	entry := &mir.Block{ID: 1, Label: "Entry"}
	b1 := &mir.Block{ID: 2, Label: "B1"}
	b2 := &mir.Block{ID: 3, Label: "B2"}
	b3 := &mir.Block{ID: 4, Label: "B3"}
	exit := &mir.Block{ID: 5, Label: "Exit"}

	// Connect successors
	entry.Succs = map[int]*mir.Block{b1.ID: b1}
	b1.Succs = map[int]*mir.Block{b2.ID: b2, b3.ID: b3}
	b2.Succs = map[int]*mir.Block{exit.ID: exit}
	b3.Succs = map[int]*mir.Block{b2.ID: b2}

	// Connect predecessors
	b1.Preds = map[int]*mir.Block{entry.ID: entry}
	b2.Preds = map[int]*mir.Block{b1.ID: b1, b3.ID: b3}
	b3.Preds = map[int]*mir.Block{b1.ID: b1}
	exit.Preds = map[int]*mir.Block{b2.ID: b2}

	blocks := make(map[int]*mir.Block)
	blocks[entry.ID] = entry
	blocks[b1.ID] = b1
	blocks[b2.ID] = b2
	blocks[b3.ID] = b3
	blocks[exit.ID] = exit

	fn := &mir.Function{
		Name:      "Foo",
		Blocks:    blocks,
		Entry:     entry,
		TempMap:   nil,
		SSAInfo:   nil,
		Dom:       nil,
		Constants: nil,
	}

	dom := ComputeDominators(fn)
	idom := ImmediateDominators(fn, dom)
	tree := DominatorTree(idom)

	printDomTree(fn.Entry, tree, 0)
}

func TestComputeDF(t *testing.T) {
	tests := []struct {
		name string
		fxn  *mir.Function
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
