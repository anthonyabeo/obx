package opt

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/obxir"
)

func TestConstantFold(t *testing.T) {

	entry := obxir.NewBlock("entry")
	tt := obxir.NewBlock("tt")
	f := obxir.NewBlock("f")
	j := obxir.NewBlock("j")
	exit := obxir.NewBlock("exit")

	connect(entry, tt)
	connect(entry, f)
	connect(tt, j)
	connect(f, j)
	connect(j, exit)

	a0 := &obxir.Temp{Ident: "a.0"}
	c := &obxir.IntegerLit{LitValue: 1}
	x1 := &obxir.Temp{Ident: "x.1"}
	x2 := &obxir.Temp{Ident: "x.2"}
	y0 := &obxir.Temp{Ident: "y.0"}

	entry.Instrs = []obxir.Instr{
		&obxir.BinaryInst{Op: obxir.ADD, Target: a0, Left: &obxir.IntegerLit{LitValue: 2}, Right: &obxir.IntegerLit{LitValue: 3}},
		&obxir.CondBrInst{Cond: c, TrueLabel: tt.Label, FalseLabel: f.Label},
	}
	tt.Instrs = []obxir.Instr{
		&obxir.BinaryInst{Op: obxir.ADD, Target: x1, Left: a0, Right: c},
		&obxir.JumpInst{Target: j.Label},
	}
	f.Instrs = []obxir.Instr{
		&obxir.BinaryInst{Op: obxir.ADD, Target: x2, Left: a0, Right: &obxir.IntegerLit{LitValue: 2}},
		&obxir.JumpInst{Target: j.Label},
	}
	j.Instrs = []obxir.Instr{
		&obxir.PhiInst{Target: y0, Args: []*obxir.PHIArg{&obxir.PHIArg{Value: x1, Block: tt}, &obxir.PHIArg{Value: x2, Block: f}}},
		&obxir.JumpInst{Target: exit.Label},
	}

	fn := &obxir.Function{
		FnName: "demo",
		Entry:  entry,
		Blocks: map[int]*obxir.Block{entry.ID: entry, tt.ID: tt, f.ID: f, j.ID: j, exit.ID: exit},
	}

	pm := NewPassManager()
	pm.Add(ConstantFold{})

	// Run to fixed point
	pm.RunFixedPoint(fn, 10)
}

func connect(p, s *obxir.Block) {
	p.Succs[s.ID] = s
	s.Preds[p.ID] = p
}
