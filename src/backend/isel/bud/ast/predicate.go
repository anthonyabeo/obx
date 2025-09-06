package ast

import (
	"bytes"

	"github.com/anthonyabeo/obx/src/backend/isel/bud"
)

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

type PredicateFunc func(args []string, env map[string]*bud.Value) bool

var predicateRegistry = map[string]PredicateFunc{}

func RegisterPredicate(name string, fn PredicateFunc) {
	predicateRegistry[name] = fn
}

func init() {
	RegisterPredicate("SImmFits12", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}

		return val.Imm >= -2048 && val.Imm <= 2047
	})

	RegisterPredicate("UImmFits12", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= 4095
	})

	RegisterPredicate("UImmFits20", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= (1<<20)-1
	})

	RegisterPredicate("ShamtFits6", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}

		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}

		return val.Imm >= 0 && val.Imm <= 63
	})

	RegisterPredicate("IsEven", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}
		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}
		return val.Imm%2 == 0
	})

	RegisterPredicate("isAligned8", func(args []string, env map[string]*bud.Value) bool {
		if len(args) != 1 {
			return false
		}
		val, ok := env[args[0]]
		if !ok || val.Kind != bud.KindImm {
			return false
		}
		return val.Imm%8 == 0
	})
}
