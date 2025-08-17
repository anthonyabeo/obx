package opt

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

func TestConstantFold(t *testing.T) {

	entry := mir.NewBlock("entry")
	tt := mir.NewBlock("tt")
	f := mir.NewBlock("f")
	j := mir.NewBlock("j")
	exit := mir.NewBlock("exit")

	connect(entry, tt)
	connect(entry, f)
	connect(tt, j)
	connect(f, j)
	connect(j, exit)

	a0 := &mir.Temp{ID: "a.0"}
	c := &mir.IntegerConst{Value: 1}
	x1 := &mir.Temp{ID: "x.1"}
	x2 := &mir.Temp{ID: "x.2"}
	y0 := &mir.Temp{ID: "y.0"}

	entry.Instrs = []mir.Instr{
		&mir.BinaryInst{Op: mir.ADD, Target: a0, Left: &mir.IntegerConst{Value: 2}, Right: &mir.IntegerConst{Value: 3}},
		&mir.CondBrInst{Cond: c, TrueLabel: tt.Label, FalseLabel: f.Label},
	}
	tt.Instrs = []mir.Instr{
		&mir.BinaryInst{Op: mir.ADD, Target: x1, Left: a0, Right: c},
		&mir.JumpInst{Target: j.Label},
	}
	f.Instrs = []mir.Instr{
		&mir.BinaryInst{Op: mir.ADD, Target: x2, Left: a0, Right: &mir.IntegerConst{Value: 2}},
		&mir.JumpInst{Target: j.Label},
	}
	j.Instrs = []mir.Instr{
		&mir.PhiInst{Target: y0, Args: []*mir.PHIArg{&mir.PHIArg{Value: x1, Block: tt}, &mir.PHIArg{Value: x2, Block: f}}},
		&mir.JumpInst{Target: exit.Label},
	}

	fn := &mir.Function{
		Name:   "demo",
		Entry:  entry,
		Blocks: map[int]*mir.Block{entry.ID: entry, tt.ID: tt, f.ID: f, j.ID: j, exit.ID: exit},
	}

	pm := NewPassManager()
	pm.Add(ConstantFold{})

	// Run to fixed point
	pm.RunFixedPoint(fn, 10)
}

func connect(p, s *mir.Block) {
	p.Succs[s.ID] = s
	s.Preds[p.ID] = p
}
