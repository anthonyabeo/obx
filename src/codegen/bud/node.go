package bud

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/obxir"
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
	Ty        obxir.Type
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
func PatMIRInst(ins obxir.Instr) *Node {
	switch inst := ins.(type) {
	case *obxir.BinaryInst:
		dst := patMIRValue(inst.Target)
		left := patMIRValue(inst.Left)
		right := patMIRValue(inst.Right)
		op := patOp(inst.Op)

		return &Node{
			Dst:  dst,
			Op:   op,
			Args: []*Node{left, right},
		}
	case *obxir.LoadInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   "load",
			Args: []*Node{patMIRValue(inst.Addr)},
		}
	case *obxir.StoreInst:
		return &Node{
			Op: "store",
			Args: []*Node{
				patMIRValue(inst.Val),
				patMIRValue(inst.Addr),
			},
		}
	case *obxir.ICmpInst:
		return &Node{
			Dst: patMIRValue(inst.Target),
			Op:  patOp(inst.Op),
			Args: []*Node{
				patMIRValue(inst.Left),
				patMIRValue(inst.Right),
			},
		}
	case *obxir.FCmpInst:
		return &Node{
			Dst: patMIRValue(inst.Target),
			Op:  patOp(inst.Op),
			Args: []*Node{
				patMIRValue(inst.Left),
				patMIRValue(inst.Right),
			},
		}
	case *obxir.CondBrInst:
		return &Node{
			Op: "br",
			Args: []*Node{
				patMIRValue(inst.Cond),
				{Val: &Value{Kind: KindLabel, Label: inst.TrueLabel}},
				{Val: &Value{Kind: KindLabel, Label: inst.FalseLabel}},
			},
		}
	case *obxir.JumpInst:
		return &Node{
			Op: "jmp",
			Args: []*Node{
				{Val: &Value{Kind: KindLabel, Label: inst.Target}},
			},
		}
	case *obxir.UnaryInst:
		return &Node{
			Dst:  patMIRValue(inst.Target),
			Op:   patOp(inst.Op),
			Args: []*Node{patMIRValue(inst.Operand)},
		}
	case *obxir.ReturnInst:
		var args []*Node
		if inst.Result != nil {
			args = append(args, patMIRValue(inst.Result))
		}

		return &Node{
			Op:   "ret",
			Args: args,
		}
	case *obxir.MoveInst:
		return &Node{
			Op:   "mov",
			Args: []*Node{patMIRValue(inst.Target), patMIRValue(inst.Value)},
		}
	case *obxir.CallInst:
		var dst *Node
		if inst.Target != nil {
			dst = patMIRValue(inst.Target)
		}
		var calleeNode *Node
		if inst.Callee != nil {
			calleeNode = patMIRValue(inst.Callee)
		} else {
			calleeNode = &Node{Val: &Value{Kind: KindLabel, Label: ""}}
		}
		return &Node{
			Dst:  dst,
			Op:   "call",
			Args: []*Node{calleeNode},
		}
	case *obxir.Arg:
		return &Node{
			Op: "argument",
			Args: []*Node{
				patMIRValue(inst.Value),
				{Val: &Value{Kind: KindImm, Imm: int64(inst.Index)}},
			},
		}
	case *obxir.AddrOf:
		return &Node{
			Op:   "addr",
			Dst:  patMIRValue(inst.Target),
			Args: []*Node{patMIRValue(inst.Addr)},
		}
	case *obxir.HaltInst:
		return &Node{
			Op:   "halt",
			Args: []*Node{patMIRValue(inst.Code)},
		}
	default:
		panic(fmt.Sprintf("patMIRInst: unexpected inst type: %T", inst))
	}
}

func patMIRValue(value obxir.Value) *Node {
	switch val := value.(type) {
	case *obxir.NamedConst:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.Ident,
			Kind: ConstSK,
			Size: val.Size,
			Ty:   val.Typ,
		}}}
	case *obxir.Local:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name: val.Ident,
			Kind: LocalSK,
			Size: val.Size,
			Ty:   val.Typ,
		}}}
	case *obxir.Param:
		return &Node{Val: &Value{Kind: KindSymbol, Symbol: Symbol{
			Name:      val.Ident,
			Kind:      ParamSK,
			Size:      val.Size,
			Ty:        val.Typ,
			ParamKind: val.Kind,
		}}}
	case *obxir.Temp:
		switch val.Type().(type) {
		case *obxir.FloatType:
			return &Node{Val: &Value{Kind: KindFPR, Reg: Reg{Name: val.Ident}}}
		default:
			return &Node{Val: &Value{Kind: KindGPR, Reg: Reg{Name: val.Ident}}}
		}
	case *obxir.IntegerLit:
		return &Node{
			Val: &Value{Kind: KindImm, Imm: int64(val.LitValue)},
		}
	case *obxir.StrLit:
		return &Node{Val: &Value{Kind: KindString, Str: String{
			Name:  val.LitName,
			Value: val.LitValue,
		}}}
	case *obxir.GlobalVariable:
		return &Node{
			Val: &Value{Kind: KindSymbol, Symbol: Symbol{
				Name: val.Ident,
				Kind: GlobalSK,
				Size: val.Size,
				Ty:   val.Typ,
			}},
		}
	case *obxir.Function:
		// Represent functions as labels for direct calls
		return &Node{Val: &Value{Kind: KindLabel, Label: val.FnName}}
	case *obxir.Mem:
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

func patOp(op obxir.InstrOp) string {
	switch op {
	case obxir.ADD:
		return "add"
	case obxir.SUB:
		return "sub"
	case obxir.RDIV:
		return "rdiv"
	case obxir.IDIV:
		return "idiv"
	case obxir.MUL:
		return "mul"
	case obxir.LD:
		return "load"
	case obxir.EQ:
		return "eq"
	case obxir.NE:
		return "ne"
	case obxir.LT:
		return "lt"
	case obxir.LE:
		return "le"
	case obxir.GT:
		return "gt"
	case obxir.GE:
		return "ge"
	case obxir.NEG:
		return "neg"
	case obxir.NOT:
		return "not"
	case obxir.LSHL:
		return "lshl"
	case obxir.LSHR:
		return "lshr"
	case obxir.ASHR:
		return "ashr"
	case obxir.AND:
		return "and"
	case obxir.OR:
		return "or"
	case obxir.FNEG:
		return "fneg"
	case obxir.FEQ:
		return "feq"
	case obxir.FLT:
		return "flt"
	case obxir.FLE:
		return "fle"
	case obxir.FGT:
		return "fgt"
	case obxir.FGE:
		return "fge"
	default:
		panic(fmt.Sprintf("invalid obxir.InstrOp: '%s'", op))
	}
}
