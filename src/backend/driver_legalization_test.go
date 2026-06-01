package backend

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	btarget "github.com/anthonyabeo/obx/src/backend/target"
)

func TestPipelineDriverLegalizationRetargetsReturnMove(t *testing.T) {
	driver := NewPipelineDriver(btarget.NewRISCV64Target())

	src := mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8))
	dst := mir.NewRegister("a0", mir.PhysicalReg, mir.NewScalarType("i64", 8))

	prog := mir.NewProgram()
	mod := mir.NewModule("m")
	fn := mir.NewFunction("f", mir.NewScalarType("i64", 8))
	blk := mir.NewBlock(0, "entry")
	blk.AddInstr(&mir.MoveInstr{Dst: dst, Src: src})
	blk.SetTerminator(&mir.MachineTerm{Op: "ret"})
	fn.AddBlock(blk)
	mod.AddFunction(fn)
	prog.AddModule(mod)

	out, err := driver.Legalization(prog)
	if err != nil {
		t.Fatalf("Legalization failed: %v", err)
	}

	outBlk := out.Modules[0].Functions[0].Blocks[0]
	if len(outBlk.Instrs) != 1 {
		t.Fatalf("len(outBlk.Instrs) = %d, want 1", len(outBlk.Instrs))
	}
	mi, ok := outBlk.Instrs[0].(*mir.MachineInstr)
	if !ok {
		t.Fatalf("outBlk.Instrs[0] is %T, want *mir.MachineInstr", outBlk.Instrs[0])
	}
	if mi.Op != "addi" {
		t.Fatalf("mi.Op = %q, want addi", mi.Op)
	}
	if len(mi.Dsts) != 1 || mi.Dsts[0] == nil || mi.Dsts[0].Name != "a0" {
		t.Fatalf("return dst = %#v, want a0", mi.Dsts)
	}
	if len(mi.Srcs) != 2 {
		t.Fatalf("len(mi.Srcs) = %d, want 2", len(mi.Srcs))
	}
	if got := mi.Srcs[0].String(); got != "v0" {
		t.Fatalf("mi.Srcs[0] = %q, want v0", got)
	}
	if got := mi.Srcs[1].String(); got != "0" {
		t.Fatalf("mi.Srcs[1] = %q, want 0", got)
	}

	mt, ok := outBlk.Term.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("outBlk.Term is %T, want *mir.MachineTerm", outBlk.Term)
	}
	if mt.Op != "ret" || len(mt.Srcs) != 0 {
		t.Fatalf("return terminator was not normalized: %#v", mt)
	}
}
