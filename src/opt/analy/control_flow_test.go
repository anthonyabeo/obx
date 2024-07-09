package analy

import (
	"github.com/anthonyabeo/obx/src/translate/tacil"
	"testing"
)

func TestComputingExtendedBasicBlocks(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b0 := tacil.NewBasicBlock("B0")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	b4 := tacil.NewBasicBlock("B4")
	b5 := tacil.NewBasicBlock("B5")
	b6 := tacil.NewBasicBlock("B6")

	cfg := tacil.NewCFG()
	cfg.Entry = entry

	cfg.Nodes.Add(entry, b0, b1, b2, b3, b4, b5, b6)

	cfg.AddSucc("entry", b0)
	cfg.AddSucc("B0", b1, b3)
	cfg.AddSucc("B1", b2)
	cfg.AddSucc("B2", b0)
	cfg.AddSucc("B3", b4, b6)
	cfg.AddSucc("B4", b5)
	cfg.AddSucc("B5", b2)
	cfg.AddSucc("B6", b5)

	cfg.AddPred("B0")
	cfg.AddPred("B1", b0)
	cfg.AddPred("B2", b1, b5)
	cfg.AddPred("B3", b0)
	cfg.AddPred("B4", b3)
	cfg.AddPred("B5", b6, b4)
	cfg.AddPred("B6", b3)

	tests := []struct {
		root string
		blks []*tacil.BasicBlock
	}{
		{"B0", []*tacil.BasicBlock{b0, b1, b3, b4, b6}},
		{"B5", []*tacil.BasicBlock{b5}},
		{"B2", []*tacil.BasicBlock{b2}},
	}

	extBBs := ExtendedBasicBlocks(cfg, b0)
	for _, tt := range tests {
		ebb, found := extBBs[tt.root]
		if !found {
			t.Errorf("no EBB found for root '%s'", tt.root)
		}

		if len(tt.blks) != ebb.Size() {
			t.Errorf("expected an EBB of size %d, got %d", len(tt.blks), ebb.Size())
		}

		for i := 0; i < len(tt.blks); i++ {
			if !ebb.Contains(tt.blks[i]) {
				t.Errorf("expected %s to be in the EBB of root %s. It doesn't", tt.blks[i], tt.root)

			}
		}
	}

	entry = tacil.NewBasicBlock("entry")
	b1 = tacil.NewBasicBlock("B1")
	b2 = tacil.NewBasicBlock("B2")
	b3 = tacil.NewBasicBlock("B3")
	b4 = tacil.NewBasicBlock("B4")
	b5 = tacil.NewBasicBlock("B5")
	b6 = tacil.NewBasicBlock("B6")
	b7 := tacil.NewBasicBlock("B7")
	exit := tacil.NewBasicBlock("exit")

	cfg = tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)

	cfg.AddSucc("entry", b1)
	cfg.AddSucc("B1", b2, b3)
	cfg.AddSucc("B2", b4)
	cfg.AddSucc("B3", b4)
	cfg.AddSucc("B4", b5, b6)
	cfg.AddSucc("B5", b7)
	cfg.AddSucc("B6", exit, b1)
	cfg.AddSucc("B7", exit, b5)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", b6, entry)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b1)
	cfg.AddPred("B4", b2, b3)
	cfg.AddPred("B5", b7, b4)
	cfg.AddPred("B6", b4)
	cfg.AddPred("B7", b5)
	cfg.AddPred("exit", b6, b7)

	tests = []struct {
		root string
		blks []*tacil.BasicBlock
	}{
		{"entry", []*tacil.BasicBlock{entry}},
		{"B1", []*tacil.BasicBlock{b1, b2, b3}},
		{"B4", []*tacil.BasicBlock{b4, b6}},
		{"B5", []*tacil.BasicBlock{b5, b7}},
		{"exit", []*tacil.BasicBlock{exit}},
	}

	extBBs = ExtendedBasicBlocks(cfg, entry)
	for _, tt := range tests {
		ebb, found := extBBs[tt.root]
		if !found {
			t.Errorf("no EBB found for root '%s'", tt.root)
		}

		if len(tt.blks) != ebb.Size() {
			t.Errorf("expected an EBB of size %d, got %d", len(tt.blks), ebb.Size())
		}

		for i := 0; i < len(tt.blks); i++ {
			if !ebb.Contains(tt.blks[i]) {
				t.Errorf("expected %s to be in the EBB of root %s. It doesn't", tt.blks[i], tt.root)

			}
		}
	}
}

func TestComputeDominance(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	b4 := tacil.NewBasicBlock("B4")
	b5 := tacil.NewBasicBlock("B5")
	b6 := tacil.NewBasicBlock("B6")
	b7 := tacil.NewBasicBlock("B7")
	exit := tacil.NewBasicBlock("exit")

	cfg := tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)

	cfg.AddSucc("entry", b1)
	cfg.AddSucc("B1", b2, b3)
	cfg.AddSucc("B2", b4)
	cfg.AddSucc("B3", b4)
	cfg.AddSucc("B4", b5, b6)
	cfg.AddSucc("B5", b7)
	cfg.AddSucc("B6", exit, b1)
	cfg.AddSucc("B7", exit, b5)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", b6, entry)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b1)
	cfg.AddPred("B4", b2, b3)
	cfg.AddPred("B5", b7, b4)
	cfg.AddPred("B6", b4)
	cfg.AddPred("B7", b5)
	cfg.AddPred("exit", b6, b7)

	tests := []struct {
		name   string
		blocks []*tacil.BasicBlock
	}{
		{"entry", []*tacil.BasicBlock{entry}},
		{"B1", []*tacil.BasicBlock{entry, b1}},
		{"B2", []*tacil.BasicBlock{entry, b1, b2}},
		{"B3", []*tacil.BasicBlock{entry, b1, b3}},
		{"B4", []*tacil.BasicBlock{entry, b1, b4}},
		{"B5", []*tacil.BasicBlock{entry, b1, b4, b5}},
		{"B6", []*tacil.BasicBlock{entry, b1, b4, b6}},
		{"B7", []*tacil.BasicBlock{entry, b1, b4, b5, b7}},
		{"exit", []*tacil.BasicBlock{entry, b1, b4, exit}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != dom[tt.name].Size() {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), dom[tt.name].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := dom[tt.name]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DOM['%s'] to contain '%s'", tt.name, tt.blocks[i])
			}
		}
	}
}

func TestComputeDominance2(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	exit := tacil.NewBasicBlock("exit")

	cfg := tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)

	cfg.AddSucc("entry", b1, b2)
	cfg.AddSucc("B1", b3)
	cfg.AddSucc("B2", b3)
	cfg.AddSucc("B3", exit)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", entry)
	cfg.AddPred("B2", entry)
	cfg.AddPred("B3", b1, b2)
	cfg.AddPred("exit", b3)

	tests := []struct {
		name   string
		blocks []*tacil.BasicBlock
	}{
		{"entry", []*tacil.BasicBlock{entry}},
		{"B1", []*tacil.BasicBlock{entry, b1}},
		{"B2", []*tacil.BasicBlock{entry, b2}},
		{"B3", []*tacil.BasicBlock{entry, b3}},
		{"exit", []*tacil.BasicBlock{entry, b3, exit}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != dom[tt.name].Size() {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), dom[tt.name].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := dom[tt.name]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected %s to be in the DOM of %s. It doesn't", tt.blocks[i], tt.name)

			}
		}
	}
}

func TestComputeDominance3(t *testing.T) {
	b0 := tacil.NewBasicBlock("B0")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	b4 := tacil.NewBasicBlock("B4")
	b5 := tacil.NewBasicBlock("B5")
	b6 := tacil.NewBasicBlock("B6")
	b7 := tacil.NewBasicBlock("B7")
	b8 := tacil.NewBasicBlock("B8")

	cfg := tacil.NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)

	cfg.AddSucc("B0", b1)
	cfg.AddSucc("B1", b2, b5)
	cfg.AddSucc("B2", b3)
	cfg.AddSucc("B3", b1, b4)
	cfg.AddSucc("B4")
	cfg.AddSucc("B5", b6, b8)
	cfg.AddSucc("B6", b7)
	cfg.AddSucc("B7", b3)
	cfg.AddSucc("B8", b7)

	cfg.AddPred("B0")
	cfg.AddPred("B1", b0, b3)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b2, b7)
	cfg.AddPred("B4", b3)
	cfg.AddPred("B5", b1)
	cfg.AddPred("B6", b5)
	cfg.AddPred("B7", b6, b8)
	cfg.AddPred("B8", b5)

	tests := []struct {
		name   string
		blocks []*tacil.BasicBlock
	}{
		{"B0", []*tacil.BasicBlock{b0}},
		{"B1", []*tacil.BasicBlock{b0, b1}},
		{"B2", []*tacil.BasicBlock{b0, b1, b2}},
		{"B3", []*tacil.BasicBlock{b0, b1, b3}},
		{"B4", []*tacil.BasicBlock{b0, b1, b3, b4}},
		{"B5", []*tacil.BasicBlock{b0, b1, b5}},
		{"B6", []*tacil.BasicBlock{b0, b1, b5, b6}},
		{"B7", []*tacil.BasicBlock{b0, b1, b5, b7}},
		{"B8", []*tacil.BasicBlock{b0, b1, b5, b8}},
	}

	Dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != Dom[tt.name].Size() {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), Dom[tt.name].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := Dom[tt.name]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DOM['%s'] to contain '%s'", tt.name, tt.blocks[i])
			}
		}
	}
}

func TestComputeImmediateDominance(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	exit := tacil.NewBasicBlock("exit")

	cfg := tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)

	cfg.AddSucc("entry", b1, b2)
	cfg.AddSucc("B1", b3)
	cfg.AddSucc("B2", b3)
	cfg.AddSucc("B3", exit)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", entry)
	cfg.AddPred("B2", entry)
	cfg.AddPred("B3", b1, b2)
	cfg.AddPred("exit", b3)

	dom := Dominance(cfg)
	iDom := ImmDominator(cfg, dom)

	tests := []struct {
		name string
		blk  string
	}{
		{"B1", "entry"},
		{"B2", "entry"},
		{"B3", "entry"},
		{"exit", "B3"},
	}

	for _, tt := range tests {
		if _, found := iDom[tt.name]; !found {
			t.Errorf("no value for IDom(%s)", tt.name)
			continue
		}

		if tt.blk != iDom[tt.name].Name() {
			t.Errorf("expected IDom(%s) --> %s; got %s instead",
				tt.name, tt.blk, iDom[tt.name])
		}
	}
}

func TestComputeNaturalLoop(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	exit := tacil.NewBasicBlock("exit")

	cfg := tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)

	cfg.AddSucc("entry", b1, b2)
	cfg.AddSucc("B1", b3)
	cfg.AddSucc("B2", b3)
	cfg.AddSucc("B3", exit)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", entry)
	cfg.AddPred("B2", entry)
	cfg.AddPred("B3", b1, b2)
	cfg.AddPred("exit", b3)

	nat := NaturalLoop(cfg, b3, b1)

	tests := []*tacil.BasicBlock{b1, b2, b3}
	for _, tt := range tests {
		if !nat.Contains(tt) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestComputeNaturalLoop2(t *testing.T) {
	entry := tacil.NewBasicBlock("entry")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	b4 := tacil.NewBasicBlock("B4")
	b5 := tacil.NewBasicBlock("B5")
	b6 := tacil.NewBasicBlock("B6")
	b7 := tacil.NewBasicBlock("B7")
	exit := tacil.NewBasicBlock("exit")

	cfg := tacil.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)

	cfg.AddSucc("entry", b1)
	cfg.AddSucc("B1", b2, b3)
	cfg.AddSucc("B2", b4)
	cfg.AddSucc("B3", b4)
	cfg.AddSucc("B4", b5, b6)
	cfg.AddSucc("B5", b7)
	cfg.AddSucc("B6", exit, b1)
	cfg.AddSucc("B7", exit, b5)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", b6, entry)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b1)
	cfg.AddPred("B4", b2, b3)
	cfg.AddPred("B5", b7, b4)
	cfg.AddPred("B6", b4)
	cfg.AddPred("B7", b5)
	cfg.AddPred("exit", b6, b7)

	nat := NaturalLoop(cfg, b6, b1)
	tests := []*tacil.BasicBlock{b1, b2, b3, b4, b6}
	for _, tt := range tests {
		if !nat.Contains(tt) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}

	natLoop := NaturalLoop(cfg, b7, b5)
	tests = []*tacil.BasicBlock{b7, b5}
	for _, tt := range tests {
		if !natLoop.Contains(tt) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestDominanceFrontier(t *testing.T) {
	b0 := tacil.NewBasicBlock("B0")
	b1 := tacil.NewBasicBlock("B1")
	b2 := tacil.NewBasicBlock("B2")
	b3 := tacil.NewBasicBlock("B3")
	b4 := tacil.NewBasicBlock("B4")
	b5 := tacil.NewBasicBlock("B5")
	b6 := tacil.NewBasicBlock("B6")
	b7 := tacil.NewBasicBlock("B7")
	b8 := tacil.NewBasicBlock("B8")

	cfg := tacil.NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)

	cfg.AddSucc("B0", b1)
	cfg.AddSucc("B1", b2, b5)
	cfg.AddSucc("B2", b3)
	cfg.AddSucc("B3", b1, b4)
	cfg.AddSucc("B4")
	cfg.AddSucc("B5", b6, b8)
	cfg.AddSucc("B6", b7)
	cfg.AddSucc("B7", b3)
	cfg.AddSucc("B8", b7)

	cfg.AddPred("B0")
	cfg.AddPred("B1", b0, b3)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b2, b7)
	cfg.AddPred("B4", b3)
	cfg.AddPred("B5", b1)
	cfg.AddPred("B6", b5)
	cfg.AddPred("B7", b6, b8)
	cfg.AddPred("B8", b5)

	tests := []struct {
		name   string
		blocks []*tacil.BasicBlock
	}{
		{"B0", []*tacil.BasicBlock{}},
		{"B1", []*tacil.BasicBlock{b1}},
		{"B2", []*tacil.BasicBlock{b3}},
		{"B3", []*tacil.BasicBlock{b1}},
		{"B4", []*tacil.BasicBlock{}},
		{"B5", []*tacil.BasicBlock{b3}},
		{"B6", []*tacil.BasicBlock{b7}},
		{"B7", []*tacil.BasicBlock{b3}},
		{"B8", []*tacil.BasicBlock{b7}},
	}

	DF := DominanceFrontier(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != DF[tt.name].Size() {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), DF[tt.name].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exist := DF[tt.name]
			if !exist || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DF of '%s' to contain '%s'", tt.name, tt.blocks[i])
			}
		}
	}

}
