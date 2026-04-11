package bud

import (
	"fmt"

	obxir2 "github.com/anthonyabeo/obx/src/ir/obxir"
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
	KindString
)

type SymbolKind int

const (
	ParamSK SymbolKind = iota
	GlobalSK
	LocalSK
	ConstSK
)

type Value struct {
	Kind   ValueKind
	Reg    Reg
	Imm    int64
	Label  string
	Reloc  Reloc
	Mem    Mem
	Symbol Symbol
	Str    String
}

type String struct {
	Name  string
	Value string
}

type Symbol struct {
	Name      string
	Kind      SymbolKind
	Size      int
	Ty        obxir2.Type
	ParamKind string // if symbol is param, VAR/IN/VALUE
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
	return fmt.Sprintf("%%%s($%s)", r.Fxn, r.Symbol)
}

type Mem struct {
	Base  Reg
	Offs  int
	Reloc Reloc
}

// PatMIRInst convert a MIR Instruction into a bud.Node for matching against
// the target instruction tree patterns
func PatMIRInst(ins obxir2.Instr) *Node {
	switch inst := ins.(type) {
	case *obxir2.BinaryInst:
		dst := patMIRValue(inst.Target)
		left := patMIRValue(inst.Left)
		right := patMIRValue(inst.Right)
		op := patOp(inst.Op)

		return &Node{
			Dst:  dst,
			Op:   op,
			Args: []*Node{left, right},
		}
	case *obxir2.LoadInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   "load",
			Args: []*Node{patMIRValue(inst.Addr)},
		}
	case *obxir2.StoreInst:
		return &Node{
			Op: "store",
			Args: []*Node{
				patMIRValue(inst.Val),
				patMIRValue(inst.Addr),
			},
		}
	case *obxir2.ICmpInst:
		return &Node{
			Dst: patMIRValue(inst.Target),
			Op:  patOp(inst.Op),
			Args: []*Node{
				patMIRValue(inst.Left),
				patMIRValue(inst.Right),
			},
		}
	case *obxir2.FCmpInst:
		return &Node{
			Dst: patMIRValue(inst.Target),
			Op:  patOp(inst.Op),
			Args: []*Node{
				patMIRValue(inst.Left),
				patMIRValue(inst.Right),
			},
		}
	case *obxir2.CondBrInst:
		return &Node{
			Op: "br",
			Args: []*Node{
				patMIRValue(inst.Cond),
				{Val: &Value{Kind: KindLabel, Label: inst.TrueLabel}},
				{Val: &Value{Kind: KindLabel, Label: inst.FalseLabel}},
			},
		}
	case *obxir2.JumpInst:
		return &Node{
			Op: "jmp",
			Args: []*Node{
				{Val: &Value{Kind: KindLabel, Label: inst.Target}},
			},
		}
	case *obxir2.UnaryInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   patOp(inst.Op),
			Args: []*Node{patMIRValue(inst.Operand)},
		}
	case *obxir2.ReturnInst:
		var args []*Node
		if inst.Result != nil {
			args = append(args, patMIRValue(inst.Result))
		}

		return &Node{
			Op:   "ret",
			Args: args,
		}
	case *obxir2.MoveInst:
		return &Node{
			Op:   "mov",
			Args: []*Node{patMIRValue(inst.Target), patMIRValue(inst.Value)},
		}
	case *obxir2.CallInst:
		var dst *Node
		if inst.Target != nil {
			dst = patMIRValue(inst.Target)
		}
		return &Node{
			Dst:  dst,
			Op:   "call",
			Args: append([]*Node{{Val: &Value{Kind: KindLabel, Label: inst.Callee}}} /*, args...*/),
		}
	case *obxir2.Arg:
		return &Node{
			Op: "argument",
			Args: []*Node{
				patMIRValue(inst.Value),
				{Val: &Value{Kind: KindImm, Imm: int64(inst.Index)}},
			},
		}
	case *obxir2.AddrOf:
		return &Node{
			Op:   "addr",
			Dst:  patMIRValue(inst.Target),
			Args: []*Node{patMIRValue(inst.Addr)},
		}
	case *obxir2.HaltInst:
		return &Node{
			Op:   "halt",
			Args: []*Node{patMIRValue(inst.Code)},
		}
	default:
		panic(fmt.Sprintf("patMIRInst: unexpected inst type: %T", inst))
	}
}

func patMIRValue(value obxir2.Value) *Node {
	switch val := value.(type) {
	case *obxir2.NamedConst:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.Ident,
			Kind: ConstSK,
			Size: val.Size,
			Ty:   val.Typ,
		}}}
	case *obxir2.Local:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.Ident,
			Kind: LocalSK,
			Size: val.Size,
			Ty:   val.Typ,
		}}}
	case *obxir2.Param:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name:      val.Ident,
			Kind:      ParamSK,
			Size:      val.Size,
			Ty:        val.Typ,
			ParamKind: val.Kind,
		}}}
	case *obxir2.Temp:
		switch val.Type().(type) {
		case *obxir2.FloatType:
			return &Node{Val: &Value{Kind: KindFPR, Reg: Reg{Name: val.Ident}}}
		default:
			return &Node{Val: &Value{Kind: KindGPR, Reg: Reg{Name: val.Ident}}}
		}
	case *obxir2.IntegerLit:
		return &Node{
			Val: &Value{Kind: KindImm, Imm: int64(val.LitValue)},
		}
	case *obxir2.StrLit:
		return &Node{Val: &Value{Kind: KindString, Str: String{
			Name:  val.LitName,
			Value: val.LitValue,
		}}}
	case *obxir2.GlobalVariable:
		return &Node{
			Val: &Value{Kind: KindSymbol, Symbol: Symbol{
				Name: val.Ident,
				Kind: GlobalSK,
				Size: val.Size,
				Ty:   val.Typ,
			}},
		}
	case *obxir2.Mem:
		base := patMIRValue(val.Base)

		return &Node{
			Op: "add",
			Args: []*Node{
				base,
				{Val: &Value{Kind: KindImm, Imm: val.Offs}},
			},
		}
	default:
		panic(fmt.Sprintf("invalid obxir.Value: '%s'", val))
	}
}

func patOp(op obxir2.InstrOp) string {
	switch op {
	case obxir2.ADD:
		return "add"
	case obxir2.SUB:
		return "sub"
	case obxir2.RDIV:
		return "rdiv"
	case obxir2.IDIV:
		return "idiv"
	case obxir2.MUL:
		return "mul"
	case obxir2.LD:
		return "load"
	case obxir2.EQ:
		return "eq"
	case obxir2.NE:
		return "ne"
	case obxir2.LT:
		return "lt"
	case obxir2.LE:
		return "le"
	case obxir2.GT:
		return "gt"
	case obxir2.GE:
		return "ge"
	case obxir2.NEG:
		return "neg"
	case obxir2.NOT:
		return "not"
	case obxir2.LSHL:
		return "lshl"
	case obxir2.LSHR:
		return "lshr"
	case obxir2.ASHR:
		return "ashr"
	case obxir2.AND:
		return "and"
	case obxir2.OR:
		return "or"
	case obxir2.FNEG:
		return "fneg"
	case obxir2.FEQ:
		return "feq"
	case obxir2.FLT:
		return "flt"
	case obxir2.FLE:
		return "fle"
	case obxir2.FGT:
		return "fgt"
	case obxir2.FGE:
		return "fge"
	default:
		panic(fmt.Sprintf("invalid obxir.InstrOp: '%s'", op))
	}
}
