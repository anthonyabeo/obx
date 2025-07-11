package lir

import (
	"bytes"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/google/go-cmp/cmp"
)

func TestMIRToLIR_Lowering(t *testing.T) {
	tests := []struct {
		name     string
		mir      *mir.Module
		expected string // expected LIR output
	}{
		{
			name: "simple assignment",
			mir: &mir.Module{
				Name: "Main",
				Procedures: []*mir.Procedure{
					{
						Name:   "Init",
						Params: []*mir.Param{},
						//Locals: []mir.Decl{
						//	{Name: "x", Type: mir.Int},
						//	{Name: "y", Type: mir.Int},
						//},
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.AssignInst{
										Target: &mir.Variable{Name: "x"},
										Value:  &mir.IntConst{Value: 42},
									},
									&mir.BinaryInst{
										Dst:   &mir.Variable{Name: "y"},
										Op:    "+",
										Left:  &mir.Variable{Name: "x"},
										Right: &mir.IntConst{Value: 8},
									},
								},
							},
						},
					},
				},
			},
			expected: `module Main {

proc Init()
entry:
  mov %x $42
  add %y, %x, $8
}

`,
		},
		{
			name: "return statement",
			mir: &mir.Module{
				Name: "Math",
				Procedures: []*mir.Procedure{
					{
						Name: "Double",
						Params: []*mir.Param{
							{Name: "n", Type: mir.Int32Type},
						},
						Result: mir.Int32Type,
						//Locals: []mir.Decl{
						//	{Name: "r", Type: mir.Int},
						//},
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.BinaryInst{
										Dst:   &mir.Variable{Name: "r"},
										Op:    "+",
										Left:  &mir.Variable{Name: "n"},
										Right: &mir.Variable{Name: "n"},
									},
									&mir.ReturnInst{
										Result: &mir.Variable{Name: "r"},
									},
								},
							},
						},
					},
				},
			},
			expected: `module Math {

proc Double(n: i32) -> i32
entry:
  add %r, %n, %n
  ret %r
}

`,
		},
		{
			name: "SimpleAddition",
			mir: &mir.Module{
				Name: "Main",
				Procedures: []*mir.Procedure{
					{
						Name: "Init",
						Params: []*mir.Param{
							{Name: "a", Type: mir.Int32Type},
							{Name: "b", Type: mir.Int32Type},
						},
						Result: mir.Int32Type,
						Locals: []mir.Decl{
							&mir.VarDecl{Name: "res", Type: mir.Int32Type},
						},
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.BinaryInst{
										Dst:   &mir.Variable{Name: "res"},
										Op:    "+",
										Left:  &mir.Variable{Name: "a"},
										Right: &mir.Variable{Name: "b"},
									},
									&mir.ReturnInst{
										Result: &mir.Variable{Name: "res"},
									},
								},
							},
						},
					},
				},
			},
			expected: `module Main {

proc Init(a: i32, b: i32) -> i32
  locals: res: i32
entry:
  add %res, %a, %b
  ret %res
}

`,
		},
		{
			name: "LoopWithExit",
			mir: &mir.Module{
				Name: "Main",
				Procedures: []*mir.Procedure{
					{
						Name: "LoopTest",
						Locals: []mir.Decl{
							&mir.VarDecl{Name: "x", Type: mir.Int32Type},
						},
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.AssignInst{
										Target: &mir.Variable{Name: "x"},
										Value:  &mir.IntConst{Value: 0},
									},
									&mir.JumpInst{Target: "loop"},
								},
							},
							{
								Label: "loop",
								Instrs: []mir.Inst{
									&mir.CmpInst{
										Dst: &mir.Variable{Name: "cond"},
										Op:  Lt,
										X:   &mir.Variable{Name: "x"},
										Y:   &mir.IntConst{Value: 10},
									},
									&mir.CondBrInst{
										Cond:       &mir.Variable{Name: "cond"},
										TrueLabel:  "body",
										FalseLabel: "exit",
									},
								},
							},
							{
								Label: "body",
								Instrs: []mir.Inst{
									&mir.BinaryInst{
										Dst:   &mir.Variable{Name: "x"},
										Op:    "+",
										Left:  &mir.Variable{Name: "x"},
										Right: &mir.IntConst{Value: 1},
									},
									&mir.JumpInst{Target: "loop"},
								},
							},
							{
								Label: "exit",
								Instrs: []mir.Inst{
									&mir.ReturnInst{},
								},
							},
						},
					},
				},
			},
			expected: `module Main {

proc LoopTest()
  locals: x: i32
entry:
  mov %x $0
  jmp %loop
loop:
  %cond = cmp.lt %x, $10
  br %cond, label %body, label %exit
body:
  add %x, %x, $1
  jmp %loop
exit:
  ret
}

`,
		},
		{
			name: "ProcedureCall",
			mir: &mir.Module{
				Name: "Main",
				Procedures: []*mir.Procedure{
					{
						Name: "MainProc",
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.ProcCallInst{
										Callee: &mir.Variable{Name: "PrintInt"},
										Args:   []mir.Operand{&mir.IntConst{Value: 42}},
									},
									&mir.ReturnInst{},
								},
							},
						},
					},
				},
			},
			expected: `module Main {

proc MainProc()
entry:
  call %PrintInt($42)
  ret
}

`,
		},
		{
			name: "GlobalReturn",
			mir: &mir.Module{
				Name: "Main",
				Globals: []mir.Decl{
					&mir.VarDecl{Name: "g", Type: mir.Int32Type},
				},
				Procedures: []*mir.Procedure{
					{
						Name:   "GetG",
						Result: mir.Int32Type,
						Blocks: []*mir.Block{
							{
								Label: "entry",
								Instrs: []mir.Inst{
									&mir.ReturnInst{
										Result: &mir.Variable{Name: "g"},
									},
								},
							},
						},
					},
				},
			},
			expected: `module Main {
  globals:
    VAR g: i32


proc GetG() -> i32
entry:
  ret %g
}

`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gen := NewGenerator()
			lirMod := gen.Generate(&mir.Program{[]*mir.Module{tt.mir}})

			var buf bytes.Buffer
			if err := FormatProgram(&buf, lirMod); err != nil {
				t.Errorf("FormatProgram returned an error: %s", err)
			}

			got := buf.String()
			if diff := cmp.Diff(tt.expected, got); diff != "" {
				t.Errorf("MIR → LIR mismatch (-want +got):\n%s", diff)
			}
		})
	}
}
