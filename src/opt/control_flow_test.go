package opt

import (
	"github.com/anthonyabeo/obx/src/translate/ir"
	"testing"
)

func TestComputingExtendedBasicBlocks(t *testing.T) {
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	b5 := ir.NewBasicBlock("B5")
	b6 := ir.NewBasicBlock("B6")
	b7 := ir.NewBasicBlock("B7")
	exit := ir.NewBasicBlock("exit")

	cfg := &ir.ControlFlowGraph{
		Entry: nil,
		Exit:  nil,
		Nodes: map[string]*ir.BasicBlock{
			"B1":    b1,
			"B2":    b2,
			"B3":    b3,
			"B4":    b4,
			"B5":    b5,
			"B6":    b6,
			"B7":    b7,
			"entry": entry,
			"exit":  exit,
		},
		Succ: map[string][]string{
			"entry": {"B1"},
			"B1":    {"B2", "B3"},
			"B2":    {"B4"},
			"B3":    {"B4"},
			"B4":    {"B5", "B6"},
			"B5":    {"B7"},
			"B6":    {"exit", "B1"},
			"B7":    {"exit", "B5"},
			"exit":  {},
		},
		Pred: map[string][]string{
			"entry": {},
			"B1":    {"B6", "entry"},
			"B2":    {"B1"},
			"B3":    {"B1"},
			"B4":    {"B2", "B3"},
			"B5":    {"B7", "B4"},
			"B6":    {"B4"},
			"B7":    {"B5"},
			"exit":  {"B6", "B7"},
		},
	}

	tests := []struct {
		root string
		blks []string
	}{
		{"entry", []string{"entry"}},
		{"B1", []string{"B1", "B2", "B2"}},
		{"B4", []string{"B4", "B6"}},
		{"B5", []string{"B5", "B7"}},
		{"exit", []string{"exit"}},
	}

	extBBs := ComputeAllExtBB(cfg, entry)
	for _, tt := range tests {
		ebb, found := extBBs[tt.root]
		if !found {
			t.Errorf("no EBB found for root '%s'", tt.root)
		}

		if len(tt.blks) != len(ebb) {
			t.Errorf("expected an EBB of size %d, got %d", len(tt.blks), len(ebb))
		}

		for i := 0; i < len(tt.blks); i++ {
			if _, exist := ebb[tt.blks[i]]; !exist {
				t.Errorf("expected %s to be in the EBB of root %s. It doesn't", tt.blks[i], tt.root)
			}
		}
	}
}
