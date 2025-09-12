package bud

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

// Node denoted an IR tree node. A MIR instruction/Value will be converted
// to a Node for pattern matching.
type Node struct {
	Dst  *Node  // blank for instructions without a destination register
	Op   string // e.g., "load", "add"
	Args []*Node
	Val  *Value // leaf payload (reg or imm); nil for interior ops
}

type ValueKind int

const (
	KindNone ValueKind = iota
	KindGPR
	KindFPR
	KindImm
	KindLabel
	KindReloc
	KindMem
	KindGlobal
)

type Value struct {
	Kind   ValueKind
	Reg    Reg
	Imm    int
	Label  string
	Reloc  Reloc
	Mem    Mem
	Global Global
}

type Global struct {
	Name string
}

type Reg struct {
	Name string
	Mode string
	Kind string
}

type Reloc struct {
	Fxn    string
	Symbol string
}

func (r Reloc) String() string {
	return fmt.Sprintf("%%s($%s)", r.Fxn, r.Symbol)
}

type Mem struct {
	Base  Reg
	Offs  int
	Reloc Reloc
}

// PatMIRInst convert a MIR Instruction into a bud.Node for matching against
// the target instruction tree patterns
func PatMIRInst(ins mir.Instr) *Node {
	switch inst := ins.(type) {
	case *mir.BinaryInst:
		dst := patMIRValue(inst.Target)
		left := patMIRValue(inst.Left)
		right := patMIRValue(inst.Right)
		op := patOp(inst.Op)

		return &Node{Dst: dst,
			Op:   op,
			Args: []*Node{left, right},
		}
	//case *mir.LoadInst:
	default:
		panic(fmt.Sprintf("patMIRInst: unexpected inst type: %T", inst))
	}
}

func patMIRValue(value mir.Value) *Node {
	switch val := value.(type) {
	case *mir.Temp:
		switch val.Type().(type) {
		case *mir.FloatType:
			return &Node{Val: &Value{Kind: KindFPR, Reg: Reg{Name: val.ID}}}
		default:
			return &Node{Val: &Value{Kind: KindGPR, Reg: Reg{Name: val.ID}}}
		}

	case *mir.IntegerConst:
		return &Node{
			Val: &Value{Kind: KindImm, Imm: int(val.Value)},
		}
	default:
		panic(fmt.Sprintf("invalid mir.Value: '%s'", val))
	}
}

func patOp(op mir.InstrOp) string {
	switch op {
	case mir.ADD:
		return "add"
	case mir.LD:
		return "load"
	default:
		panic(fmt.Sprintf("invalid mir.InstrOp: '%s'", op))

	}
}
