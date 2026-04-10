package isel

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/codegen/asm"
	"github.com/anthonyabeo/obx/src/codegen/bud"
	"github.com/anthonyabeo/obx/src/codegen/bud/ast"
)

type Selector struct {
	Rules   []*ast.Rule
	tempIdx int // monotonic counter for unique temp register names
}

func NewSelector(rules []*ast.Rule) *Selector {
	return &Selector{Rules: rules}
}

func (s *Selector) Select(pat *bud.Node) []*asm.Instr {
	res := s.selectBest(pat)
	if res == nil {
		panic(fmt.Sprintf("no match found for IR: %v", pat))
	}

	res.Binding(res.Bind, &s.tempIdx)

	return res.Emit()
}

func (s *Selector) selectBest(pat *bud.Node) *MatchResult {
	var best *MatchResult
	bestCost := int(^uint(0) >> 1) // max int

	for _, rule := range s.Rules {
		env := map[string]*bud.Value{}
		classes := make(map[string]ast.Operand, len(rule.In)+1+len(rule.Temps))
		for _, v := range rule.In {
			classes[v.Desc()] = v
		}

		if rule.Out != nil {
			classes[rule.Out.Desc()] = rule.Out
		}

		for _, v := range rule.Temps {
			classes[v.Desc()] = v
		}

		if match(rule.Pattern, pat, env, classes) && rule.CheckPredicates(env) && rule.Cost < bestCost {
			best = &MatchResult{Rule: rule, Pattern: pat, Bind: env}
			bestCost = rule.Cost
		} else if rule.IsComm && len(pat.Args) == 2 {
			// try swapped children for commutative ops
			pat.Args[0], pat.Args[1] = pat.Args[1], pat.Args[0]
			if match(rule.Pattern, pat, env, classes) && rule.CheckPredicates(env) && rule.Cost < bestCost {
				best = &MatchResult{Rule: rule, Pattern: pat, Bind: env}
				bestCost = rule.Cost
			}
			pat.Args[0], pat.Args[1] = pat.Args[1], pat.Args[0]
		}
	}

	return best
}
