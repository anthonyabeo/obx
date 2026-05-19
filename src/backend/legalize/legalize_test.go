package legalize

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

func TestRunInsertsAbiReturnMove(t *testing.T) {
	tests := []struct {
		name    string
		target  string
		wantOp  string
		wantRet string
	}{
		{name: "riscv", target: target.RV64IMAFDName, wantOp: "addi", wantRet: "a0"},
		{name: "arm64", target: target.Arm64Name, wantOp: "mov", wantRet: "x0"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tgt, err := target.Lookup(tc.target)
			if err != nil {
				t.Fatalf("Lookup(%s) failed: %v", tc.target, err)
			}

			prog := mir.NewProgram()
			mod := mir.NewModule("m")
			fn := mir.NewFunction("f", mir.NewScalarType("i64", 8))
			blk := mir.NewBlock(0, "entry")
			v := mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8))
			blk.AddInstr(&mir.MachineInstr{Op: "nop"})
			blk.SetTerminator(&mir.MachineTerm{Op: "ret", Srcs: []mir.Operand{v}})
			fn.AddBlock(blk)
			mod.AddFunction(fn)
			prog.AddModule(mod)

			out, err := Run(prog, tgt)
			if err != nil {
				t.Fatalf("Run failed: %v", err)
			}
			if out != prog {
				t.Fatalf("Run should operate in place")
			}
			if len(blk.Instrs) != 2 {
				t.Fatalf("len(blk.Instrs) = %d, want 2", len(blk.Instrs))
			}
			mi, ok := blk.Instrs[1].(*mir.MachineInstr)
			if !ok {
				t.Fatalf("blk.Instrs[1] is %T, want *mir.MachineInstr", blk.Instrs[1])
			}
			if mi.Op != tc.wantOp {
				t.Fatalf("mi.Op = %q, want %q", mi.Op, tc.wantOp)
			}
			if len(mi.Dsts) != 1 || mi.Dsts[0] == nil || mi.Dsts[0].Name != tc.wantRet {
				t.Fatalf("mi.Dsts = %#v, want %q", mi.Dsts, tc.wantRet)
			}
			if tc.target == target.Arm64Name {
				if len(mi.Srcs) != 1 || mi.Srcs[0].String() != "v0" {
					t.Fatalf("mi.Srcs = %#v", mi.Srcs)
				}
				return
			}
			if len(mi.Srcs) != 2 || mi.Srcs[0].String() != "v0" || mi.Srcs[1].String() != "0" {
				t.Fatalf("mi.Srcs = %#v", mi.Srcs)
			}
			mt, ok := blk.Term.(*mir.MachineTerm)
			if !ok {
				t.Fatalf("blk.Term is %T, want *mir.MachineTerm", blk.Term)
			}
			if mt.Op != "ret" || len(mt.Srcs) != 0 {
				t.Fatalf("return terminator was not normalized: %#v", mt)
			}
		})
	}
}

