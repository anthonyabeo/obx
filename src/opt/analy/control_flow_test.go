package analy

import (
	"testing"

	"github.com/anthonyabeo/obx/src/translate/ir"
)

func TestComputingExtendedBasicBlocks(t *testing.T) {
	entry := ir.NewBasicBlock("entry")
	b0 := ir.NewBasicBlock("B0")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	b5 := ir.NewBasicBlock("B5")
	b6 := ir.NewBasicBlock("B6")

	cfg := &ir.ControlFlowGraph{
		Entry: b0,
		Exit:  nil,
		Nodes: map[string]*ir.BasicBlock{
			"entry": entry,
			"B0":    b0,
			"B1":    b1,
			"B2":    b2,
			"B3":    b3,
			"B4":    b4,
			"B5":    b5,
			"B6":    b6,
		},
		Succ: map[string][]string{
			"entry": {"B0"},
			"B0":    {"B1", "B3"},
			"B1":    {"B2"},
			"B2":    {"B0"},
			"B3":    {"B4", "B6"},
			"B4":    {"B5"},
			"B5":    {"B2"},
			"B6":    {"B5"},
		},
		Pred: map[string][]string{
			"B0": {},
			"B1": {"B0"},
			"B2": {"B1", "B5"},
			"B3": {"B0"},
			"B4": {"B3"},
			"B5": {"B6", "B4"},
			"B6": {"B3"},
		},
	}

	tests := []struct {
		root string
		blks []string
	}{
		{"B0", []string{"B0", "B1", "B3", "B4", "B6"}},
		{"B5", []string{"B5"}},
		{"B2", []string{"B2"}},
	}

	extBBs := ExtendedBasicBlocks(cfg, b0)
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

	entry = ir.NewBasicBlock("entry")
	b1 = ir.NewBasicBlock("B1")
	b2 = ir.NewBasicBlock("B2")
	b3 = ir.NewBasicBlock("B3")
	b4 = ir.NewBasicBlock("B4")
	b5 = ir.NewBasicBlock("B5")
	b6 = ir.NewBasicBlock("B6")
	b7 := ir.NewBasicBlock("B7")
	exit := ir.NewBasicBlock("exit")

	cfg = &ir.ControlFlowGraph{
		Entry: entry,
		Exit:  exit,
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

	tests = []struct {
		root string
		blks []string
	}{
		{"entry", []string{"entry"}},
		{"B1", []string{"B1", "B2", "B3"}},
		{"B4", []string{"B4", "B6"}},
		{"B5", []string{"B5", "B7"}},
		{"exit", []string{"exit"}},
	}

	extBBs = ExtendedBasicBlocks(cfg, entry)
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

func TestComputeDominance(t *testing.T) {
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
		Entry: entry,
		Exit:  exit,
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
		name   string
		blocks []string
	}{
		{"entry", []string{"entry"}},
		{"B1", []string{"entry", "B1"}},
		{"B2", []string{"entry", "B1", "B2"}},
		{"B3", []string{"entry", "B1", "B3"}},
		{"B4", []string{"entry", "B1", "B4"}},
		{"B5", []string{"entry", "B1", "B4", "B5"}},
		{"B6", []string{"entry", "B1", "B4", "B6"}},
		{"B7", []string{"entry", "B1", "B4", "B5", "B7"}},
		{"exit", []string{"entry", "B1", "B4", "exit"}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != len(dom[tt.name]) {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), len(dom[tt.name]))
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
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	exit := ir.NewBasicBlock("exit")

	cfg := &ir.ControlFlowGraph{
		Entry: entry,
		Exit:  exit,
		Nodes: map[string]*ir.BasicBlock{
			"B1":    b1,
			"B2":    b2,
			"B3":    b3,
			"entry": entry,
			"exit":  exit,
		},
		Succ: map[string][]string{
			"entry": {"B1", "B2"},
			"B1":    {"B3"},
			"B2":    {"B3"},
			"B3":    {"exit"},
			"exit":  {},
		},
		Pred: map[string][]string{
			"entry": {},
			"B1":    {"entry"},
			"B2":    {"entry"},
			"B3":    {"B1", "B2"},
			"exit":  {"B3"},
		},
	}

	tests := []struct {
		name string
		blks []string
	}{
		{"entry", []string{"entry"}},
		{"B1", []string{"entry", "B1"}},
		{"B2", []string{"entry", "B2"}},
		{"B3", []string{"entry", "B3"}},
		{"exit", []string{"entry", "B3", "exit"}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blks) != len(dom[tt.name]) {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blks), len(dom[tt.name]))
		}

		for i := 0; i < len(tt.blks); i++ {
			if _, exist := dom[tt.blks[i]]; !exist {
				t.Errorf("expected %s to be in the DOM of %s. It doesn't", tt.blks[i], tt.name)
			}
		}
	}

}

func TestComputeDominance3(t *testing.T) {
	b0 := ir.NewBasicBlock("B0")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	b5 := ir.NewBasicBlock("B5")
	b6 := ir.NewBasicBlock("B6")
	b7 := ir.NewBasicBlock("B7")
	b8 := ir.NewBasicBlock("B8")

	cfg := &ir.ControlFlowGraph{
		Entry: b0,
		Exit:  b4,
		Nodes: map[string]*ir.BasicBlock{
			"B0": b0,
			"B1": b1,
			"B2": b2,
			"B3": b3,
			"B4": b4,
			"B5": b5,
			"B6": b6,
			"B7": b7,
			"B8": b8,
		},
		Succ: map[string][]string{
			"B0": {"B1"},
			"B1": {"B2", "B5"},
			"B2": {"B3"},
			"B3": {"B4", "B1"},
			"B4": {},
			"B5": {"B6", "B8"},
			"B6": {"B7"},
			"B7": {"B3"},
			"B8": {"B7"},
		},
		Pred: map[string][]string{
			"B0": {},
			"B1": {"B0", "B3"},
			"B2": {"B1"},
			"B3": {"B2", "B7"},
			"B4": {"B3"},
			"B5": {"B1"},
			"B6": {"B5"},
			"B7": {"B6", "B8"},
			"B8": {"B5"},
		},
	}

	tests := []struct {
		name   string
		blocks []string
	}{
		{"B0", []string{"B0"}},
		{"B1", []string{"B0", "B1"}},
		{"B2", []string{"B0", "B1", "B2"}},
		{"B3", []string{"B0", "B1", "B3"}},
		{"B4", []string{"B0", "B1", "B3", "B4"}},
		{"B5", []string{"B0", "B1", "B5"}},
		{"B6", []string{"B0", "B1", "B5", "B6"}},
		{"B7", []string{"B0", "B1", "B5", "B7"}},
		{"B8", []string{"B0", "B1", "B5", "B8"}},
	}

	Dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != len(Dom[tt.name]) {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), len(Dom[tt.name]))
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
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	exit := ir.NewBasicBlock("exit")

	cfg := &ir.ControlFlowGraph{
		Entry: entry,
		Exit:  exit,
		Nodes: map[string]*ir.BasicBlock{
			"B1":    b1,
			"B2":    b2,
			"B3":    b3,
			"entry": entry,
			"exit":  exit,
		},
		Succ: map[string][]string{
			"entry": {"B1", "B2"},
			"B1":    {"B3"},
			"B2":    {"B3"},
			"B3":    {"exit"},
			"exit":  {},
		},
		Pred: map[string][]string{
			"entry": {},
			"B1":    {"entry"},
			"B2":    {"entry"},
			"B3":    {"B1", "B2"},
			"exit":  {"B3"},
		},
	}

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
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	exit := ir.NewBasicBlock("exit")

	cfg := &ir.ControlFlowGraph{
		Entry: entry,
		Exit:  exit,
		Nodes: map[string]*ir.BasicBlock{
			"B1":    b1,
			"B2":    b2,
			"B3":    b3,
			"entry": entry,
			"exit":  exit,
		},
		Succ: map[string][]string{
			"entry": {"B1"},
			"B1":    {"B2"},
			"B2":    {"B2", "B3"},
			"B3":    {"exit", "B1"},
			"exit":  {},
		},
		Pred: map[string][]string{
			"entry": {},
			"B1":    {"entry", "B3"},
			"B2":    {"B1", "B2"},
			"B3":    {"B2"},
			"exit":  {"B3"},
		},
	}

	nat := NaturalLoop(cfg, "B3", "B1")

	tests := []string{"B1", "B2", "B3"}
	for _, tt := range tests {
		if nat[tt] == nil || nat[tt].Name() != tt {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestComputeNaturalLoop2(t *testing.T) {
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
		Entry: entry,
		Exit:  exit,
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

	nat := NaturalLoop(cfg, "B6", "B1")
	tests := []string{"B1", "B2", "B3", "B4", "B6"}
	for _, tt := range tests {
		if nat[tt] == nil || nat[tt].Name() != tt {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}

	natLoop := NaturalLoop(cfg, "B7", "B5")
	tests = []string{"B7", "B5"}
	for _, tt := range tests {
		if natLoop[tt] == nil || natLoop[tt].Name() != tt {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestDominanceFrontier(t *testing.T) {
	b0 := ir.NewBasicBlock("B0")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	b5 := ir.NewBasicBlock("B5")
	b6 := ir.NewBasicBlock("B6")
	b7 := ir.NewBasicBlock("B7")
	b8 := ir.NewBasicBlock("B8")

	cfg := &ir.ControlFlowGraph{
		Entry: b0,
		Exit:  b4,
		Nodes: map[string]*ir.BasicBlock{
			"B0": b0,
			"B1": b1,
			"B2": b2,
			"B3": b3,
			"B4": b4,
			"B5": b5,
			"B6": b6,
			"B7": b7,
			"B8": b8,
		},
		Succ: map[string][]string{
			"B0": {"B1"},
			"B1": {"B2", "B5"},
			"B2": {"B3"},
			"B3": {"B4", "B1"},
			"B4": {},
			"B5": {"B6", "B8"},
			"B6": {"B7"},
			"B7": {"B3"},
			"B8": {"B7"},
		},
		Pred: map[string][]string{
			"B0": {},
			"B1": {"B0", "B3"},
			"B2": {"B1"},
			"B3": {"B2", "B7"},
			"B4": {"B3"},
			"B5": {"B1"},
			"B6": {"B5"},
			"B7": {"B6", "B8"},
			"B8": {"B5"},
		},
	}

	tests := []struct {
		name   string
		blocks []string
	}{
		{"B0", []string{}},
		{"B1", []string{"B1"}},
		{"B2", []string{"B3"}},
		{"B3", []string{"B1"}},
		{"B4", []string{}},
		{"B5", []string{"B3"}},
		{"B6", []string{"B7"}},
		{"B7", []string{"B3"}},
		{"B8", []string{"B7"}},
	}

	DF := DominanceFrontier(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != len(DF[tt.name]) {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), len(DF[tt.name]))
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exist := DF[tt.name]
			if !exist || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DF of '%s' to contain '%s'", tt.name, tt.blocks[i])
			}
		}
	}

}
