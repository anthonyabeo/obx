package analy

import (
	"testing"

	"github.com/anthonyabeo/obx/src/translate/ir"
)

func TestIterativeDataFlow(t *testing.T) {
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	b5 := ir.NewBasicBlock("B5")
	b6 := ir.NewBasicBlock("B6")
	exit := ir.NewBasicBlock("exit")

	cfg := ir.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, exit)

	cfg.AddSucc("entry", b1)
	cfg.AddSucc("B1", b2, b3)
	cfg.AddSucc("B2", exit)
	cfg.AddSucc("B3", b4)
	cfg.AddSucc("B4", b5, b6)
	cfg.AddSucc("B5", exit)
	cfg.AddSucc("B6", b4)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", entry)
	cfg.AddPred("B2", b1)
	cfg.AddPred("B3", b1)
	cfg.AddPred("B4", b3, b6)
	cfg.AddPred("B5", b4)
	cfg.AddPred("B6", b4)
	cfg.AddPred("exit", b2, b5)

	tests := []struct {
		name string
		out  ir.BitVector
	}{
		{"entry", 0b00000000},
		{"B1", 0b00000000},
		{"B2", 0b11100000},
		{"B3", 0b11100000},
		{"B4", 0b11111111},
		{"B5", 0b11111111},
		{"B6", 0b11111111},
		{"exit", 0b11111111},
	}

	Init := ir.BitVector(0)
	FlowFunctions := map[string]func(ir.BitVector) ir.BitVector{
		"entry": func(vec ir.BitVector) ir.BitVector { return vec },
		"B1":    func(vec ir.BitVector) ir.BitVector { return 0b11100000 | vec },
		"B2":    func(vec ir.BitVector) ir.BitVector { return vec },
		"B3":    func(vec ir.BitVector) ir.BitVector { return 0b00010000 | vec },
		"B4":    func(vec ir.BitVector) ir.BitVector { return vec },
		"B5":    func(vec ir.BitVector) ir.BitVector { return vec },
		"B6":    func(vec ir.BitVector) ir.BitVector { return 0b00001111 | vec },
	}

	DFIn := IterativeDataFlow(cfg, FlowFunctions, Init)

	for _, tt := range tests {
		if DFIn[tt.name] != tt.out {
			t.Errorf("error for '%s'. Expected '%b', got '%b'", tt.name, tt.out, DFIn[tt.name])
		}
	}

}

func TestIterativeDataflowDragonBook(t *testing.T) {
	entry := ir.NewBasicBlock("entry")
	b1 := ir.NewBasicBlock("B1")
	b2 := ir.NewBasicBlock("B2")
	b3 := ir.NewBasicBlock("B3")
	b4 := ir.NewBasicBlock("B4")
	exit := ir.NewBasicBlock("exit")

	cfg := ir.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, exit)

	cfg.AddSucc("entry", b1)
	cfg.AddSucc("B1", b2)
	cfg.AddSucc("B2", b3, b4)
	cfg.AddSucc("B3", b4)
	cfg.AddSucc("B4", exit)
	cfg.AddSucc("exit")

	cfg.AddPred("entry")
	cfg.AddPred("B1", entry)
	cfg.AddPred("B2", b1, b4)
	cfg.AddPred("B3", b2)
	cfg.AddPred("B4", b2, b3)
	cfg.AddPred("exit", b4)

	tests := []struct {
		name string
		out  ir.BitVector
	}{
		{"entry", 0b00000000},
		{"B1", 0b0_1110000},
		{"B2", 0b0_001_1110},
		{"B3", 0b0_000_1110},
		{"B4", 0b0_001_0111},
		{"exit", 0b0_001_0111},
	}

	GEN := map[string]ir.BitVector{
		"entry": 0,
		"B1":    0b01110000,
		"B2":    0b00001100,
		"B3":    0b00000010,
		"B4":    0b00000001,
		"exit":  0,
	}
	KILL := map[string]ir.BitVector{
		"entry": 0,
		"B1":    0b00001111,
		"B2":    0b01100001,
		"B3":    0b00010000,
		"B4":    0b01001000,
		"exit":  0,
	}

	_, OUT := IterativeDataflowDragonBook(cfg, GEN, KILL)
	for _, tt := range tests {
		if OUT[tt.name] != tt.out {
			t.Errorf("error for '%s'. Expected '%b', got '%b'", tt.name, tt.out, OUT[tt.name])
		}
	}
}
