package ast

import (
	"bytes"
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/isel/bud"
)

type Rule struct {
	Name         string
	Out          Operand
	In           []Operand
	Temps        []Operand
	Phys         []Operand
	Pattern      *Pattern
	Instructions []Instr
	Predicates   []*Predicate
	Cost         int
}

func (r *Rule) CheckPredicates(env map[string]*bud.Value) bool {
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
	buf.WriteString(r.Out.String())
	buf.WriteString(";\n")
	if len(r.In) > 0 {
		buf.WriteString("  in    ")
		for i, v := range r.In {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(v.String())
		}
		buf.WriteString(";\n")
	}
	if len(r.Temps) > 0 {
		buf.WriteString("  temps ")
		for i, v := range r.Temps {
			if i > 0 {
				buf.WriteString(", ")
			}
			buf.WriteString(v.String())
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
