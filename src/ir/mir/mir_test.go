package mir

import (
	"github.com/anthonyabeo/obx/src/ir"
	"os"
	"testing"
)

func TestOutputMIRPrograms(t *testing.T) {
	t0 := Temp{Name: "t0"}
	prog := &Program{
		Modules: []*Module{
			{
				Name:    "main",
				IsEntry: true,
				Decl: []Decl{
					&Function{
						Name:   "Add",
						Result: Int32Type,
						Params: []*Param{
							{Name: "a", Type: Int32Type},
							{Name: "b", Type: Int32Type},
						},
						Blocks: []*Block{
							{
								Label: "entry",
								Inst: []Inst{
									&AssignInst{
										Target: t0,
										Value: &Binary{
											Op:    ir.Add,
											Left:  &Variable{Name: "a", Typ: Int32Type},
											Right: &Variable{Name: "b", Typ: Int32Type},
											Typ:   Int32Type,
										},
									},
									&ReturnInst{Result: t0},
								},
							},
						},
					},
				},
			},
		},
	}

	//fn := prog.Modules[0].Decl[0].(*Function)
	//result := FormatProgram(prog)
	if err := WriteProgram(os.Stdout, prog); err != nil {
		t.Error(err)
	}
	//if result != "fn Add(a: i32, b: i32) -> i32 {\n    return a + b;\n}\n" {
	//	t.Errorf("unexpected output: %s", result)
	//}
}
