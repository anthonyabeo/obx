package lower

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/ir/minir"
)

func TestLowerFunction_AllocaRecordType(t *testing.T) {
	recTy := minir.NewRecordType("Time.DateTime", []minir.RecordField{
		{Name: "sec", Type: minir.I32(), Offset: 0},
		{Name: "nsec", Type: minir.I32(), Offset: 4},
		{Name: "epoch", Type: minir.I64(), Offset: 8},
	})

	addr := minir.NewTemp("dt", minir.Ptr(recTy))
	addr.IsAddr = true

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	ret := &minir.ReturnInst{}
	entry.Instrs = []minir.Instr{
		&minir.AllocaInst{Dst: addr, AllocType: recTy},
		ret,
	}
	entry.Term = ret

	fn := &minir.Function{
		FnName: "alloca_record",
		Entry:  entry,
		Exit:   entry,
		Blocks: map[int]*minir.Block{0: entry},
	}

	lowered, err := LowerFunction(fn, nil)
	if err != nil {
		t.Fatalf("LowerFunction failed: %v", err)
	}

	b := lowered.BlockByLabel("entry")
	if b == nil {
		t.Fatalf("lowered block not found")
	}
	if len(b.Instrs) == 0 {
		t.Fatalf("expected lowered instructions")
	}

	alloc, ok := b.Instrs[0].(*mir.AllocaInstr)
	if !ok {
		t.Fatalf("expected first lowered instruction to be alloca, got %T", b.Instrs[0])
	}
	if alloc.Size != 16 {
		t.Fatalf("alloca size = %d, want 16", alloc.Size)
	}
}
