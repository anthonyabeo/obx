package dsl

import (
	"bytes"
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

type MachineDesc struct {
	Header *Header
	Rules  []*Rule
}

type Header struct {
	Fields map[string][]string
}

type Var struct {
	Kind string // e.g. GPR, imm
	Name string // e.g. $rd, $ofs
}

func (v Var) String() string { return fmt.Sprintf("%s:$%s", v.Kind, v.Name) }

// Expr denotes Pattern tree (variables are leaves with Var != "")
type Expr struct {
	Op   string
	Args []*Expr
	Var  string // e.g. "$rs1", "$ofs" (only for variable leaves)
}

func (e Expr) String() string {
	if len(e.Args) == 0 {
		return e.Op
	}
	var buf bytes.Buffer
	buf.WriteString(e.Op)
	buf.WriteString("(")
	for i, arg := range e.Args {
		if i > 0 {
			buf.WriteString(", ")
		}
		buf.WriteString(arg.String())
	}
	buf.WriteString(")")
	return buf.String()
}

type Rule struct {
	Name         string
	Out          Var
	In           []Var
	Temps        []Var
	Pattern      *Expr
	Instructions []string
	Predicates   []Predicate
	Cost         int
}

func (r *Rule) CheckPredicates(env map[string]*Value) bool {
	for _, pred := range r.Predicates {
		fn, ok := predicateRegistry[pred.Name]
		if !ok {
			return false // unknown predicate
		}

		result := fn(pred.Args, env)
		if pred.Negated {
			return !result
		}
		return result
	}
	return true
}

func (r *Rule) String() string {
	var buf bytes.Buffer
	buf.WriteString(fmt.Sprintf("rule %s {\n", r.Name))
	buf.WriteString("  out   ")
	buf.WriteString(fmt.Sprintf("%s:$%s", r.Out.Kind, r.Out.Name))
	buf.WriteString(";\n")
	if len(r.In) > 0 {
		buf.WriteString("  in    ")
		for i, v := range r.In {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(fmt.Sprintf("%s:$%s", v.Kind, v.Name))
		}
		buf.WriteString(";\n")
	}
	if len(r.Temps) > 0 {
		buf.WriteString("  temps ")
		for i, v := range r.Temps {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(fmt.Sprintf("%s:$%s", v.Kind, v.Name))
		}
		buf.WriteString(";\n")
	}
	buf.WriteString(fmt.Sprintf("  pattern %s;\n", r.Pattern.String()))
	if len(r.Instructions) > 0 {
		buf.WriteString("  asm {\n")
		for _, inst := range r.Instructions {
			buf.WriteString(fmt.Sprintf("    \"%s\";\n", inst))
		}
		buf.WriteString("  }\n")
	}
	buf.WriteString(fmt.Sprintf("  cost %d;\n", r.Cost))
	if len(r.Predicates) > 0 {
		buf.WriteString("  cond ")
		for i, pred := range r.Predicates {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(pred.String())
		}
		buf.WriteString(";\n")
	}
	buf.WriteString("}")
	return buf.String()
}

type Predicate struct {
	Negated bool
	Name    string
	Args    []string
}

func (p Predicate) String() string {
	var buf bytes.Buffer
	if p.Negated {
		buf.WriteString("!")
	}
	buf.WriteString(p.Name)
	buf.WriteString("(")

	for _, arg := range p.Args {
		buf.WriteString(arg)
	}

	buf.WriteString(")")

	return buf.String()
}

type PredicateFunc func(args []string, env map[string]*Value) bool

var predicateRegistry = map[string]PredicateFunc{}

func RegisterPredicate(name string, fn PredicateFunc) {
	predicateRegistry[name] = fn
}

func init() {
	RegisterPredicate("SImmFits12", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}

		return val.Imm >= -2048 && val.Imm <= 2047
	})

	RegisterPredicate("UImmFits12", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= 4095
	})

	RegisterPredicate("UImmFits20", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= (1<<20)-1
	})

	RegisterPredicate("ShamtFits6", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= 63
	})

	RegisterPredicate("IsEven", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}
		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}
		return val.Imm%2 == 0
	})

	RegisterPredicate("isAligned8", func(args []string, env map[string]*Value) bool {
		if len(args) != 1 {
			return false
		}
		val, ok := env[args[0]]
		if !ok || val.Kind != KindImm {
			return false
		}
		return val.Imm%8 == 0
	})
}

// Node denoted an IR tree node. A MIR instruction/Value will be converted
// to a Node for pattern matching.
type Node struct {
	Dst  *Node  // blank for instructions without a destination register
	Op   string // e.g., "load", "add"
	Args []*Node
	Val  *Value // leaf payload (reg or imm); nil for interior ops
}

// A leaf value bound at match-time
type ValueKind int

const (
	KindNone ValueKind = iota
	KindGPR
	KindFPR
	KindImm
	KindLabel
)

type Value struct {
	Kind  ValueKind
	Reg   string // e.g., "x5" (virtual or physical)
	Imm   int
	Label string
}

// PatMIRInst convert a MIR Instruction into a dsl.Node for matching against
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
		return &Node{
			Val: &Value{Kind: KindGPR, Reg: val.ID},
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
