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
	KindSymbol
)

type SymbolKind int

const (
	ParamSK SymbolKind = iota
	GlobalSK
	LocalSK
)

type Value struct {
	Kind   ValueKind
	Reg    Reg
	Imm    int
	Label  string
	Reloc  Reloc
	Mem    Mem
	Symbol Symbol
}

type Symbol struct {
	Name  string
	Kind  SymbolKind
	Size  int
	Align int
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

		return &Node{
			Dst:  dst,
			Op:   op,
			Args: []*Node{left, right},
		}
	case *mir.LoadInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   "load",
			Args: []*Node{patMIRValue(inst.Addr)},
		}
	case *mir.StoreInst:
		return &Node{
			Op: "store",
			Args: []*Node{
				patMIRValue(inst.Val),
				patMIRValue(inst.Addr),
			},
		}
	case *mir.CmpInst:
		return &Node{
			Dst: patMIRValue(inst.Target),
			Op:  patOp(inst.Op),
			Args: []*Node{
				patMIRValue(inst.Left),
				patMIRValue(inst.Right),
			},
		}
	case *mir.CondBrInst:
		return &Node{
			Op: "br",
			Args: []*Node{
				patMIRValue(inst.Cond),
				{Val: &Value{Kind: KindLabel, Label: inst.TrueLabel}},
				{Val: &Value{Kind: KindLabel, Label: inst.FalseLabel}},
			},
		}
	case *mir.JumpInst:
		return &Node{
			Op: "jmp",
			Args: []*Node{
				{Val: &Value{Kind: KindLabel, Label: inst.Target}},
			},
		}
	case *mir.UnaryInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   patOp(inst.Op),
			Args: []*Node{patMIRValue(inst.Operand)},
		}
	case *mir.ReturnInst:
		var args []*Node
		if inst.Result != nil {
			args = append(args, patMIRValue(inst.Result))
		}

		return &Node{
			Op:   "ret",
			Args: args,
		}
	case *mir.MovInst:
		return &Node{
			Op:   "mov",
			Args: []*Node{patMIRValue(inst.Target), patMIRValue(inst.Value)},
		}
	case *mir.CallInst:
		var dst *Node
		if inst.Target != nil {
			dst = patMIRValue(inst.Target)
		}
		return &Node{
			Dst:  dst,
			Op:   "call",
			Args: append([]*Node{{Val: &Value{Kind: KindLabel, Label: inst.Callee}}} /*, args...*/),
		}
	case *mir.Arg:
		return &Node{
			Op: "argument",
			Args: []*Node{
				patMIRValue(inst.Value),
				{Val: &Value{Kind: KindImm, Imm: inst.Index}},
			},
		}
	default:
		panic(fmt.Sprintf("patMIRInst: unexpected inst type: %T", inst))
	}
}

func patMIRValue(value mir.Value) *Node {
	switch val := value.(type) {
	case *mir.Local:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.BName,
			Kind: LocalSK,
			Size: val.Size,
		}}}
	case *mir.Param:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.BName,
			Kind: ParamSK,
			Size: val.Size,
		}}}
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
	case *mir.Global:
		return &Node{
			Val: &Value{Kind: KindSymbol, Symbol: Symbol{
				Name: val.NameStr,
				Kind: GlobalSK,
				Size: val.Size,
			}},
		}
	case *mir.Mem:
		base := patMIRValue(val.Base)

		return &Node{
			Val: &Value{
				Kind: KindMem,
				Mem: Mem{
					Base: base.Val.Reg,
					Offs: val.Offs,
				},
			},
		}
	default:
		panic(fmt.Sprintf("invalid mir.Value: '%s'", val))
	}
}

func patOp(op mir.InstrOp) string {
	switch op {
	case mir.ADD:
		return "add"
	case mir.SUB:
		return "sub"
	case mir.DIV:
		return "div"
	case mir.MUL:
		return "mul"
	case mir.LD:
		return "load"
	case mir.EQ:
		return "eq"
	case mir.NE:
		return "ne"
	case mir.LT:
		return "lt"
	case mir.LE:
		return "le"
	case mir.GT:
		return "gt"
	case mir.GE:
		return "ge"
	case mir.NEG:
		return "neg"
	case mir.NOT:
		return "not"
	default:
		panic(fmt.Sprintf("invalid mir.InstrOp: '%s'", op))

	}
}
