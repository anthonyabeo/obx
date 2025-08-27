package isel

import (
	"github.com/anthonyabeo/obx/src/backend/isel/dsl"
	"github.com/anthonyabeo/obx/src/backend/ralloc"
)

type Selector struct {
	Rules []*dsl.Rule
	alloc ralloc.RegAlloc
}

func NewSelector(rules []*dsl.Rule, alloc ralloc.RegAlloc) *Selector {
	return &Selector{Rules: rules, alloc: alloc}
}

func (s *Selector) Select(pat *dsl.Node) []string {
	res := s.selectBest(pat)
	if res == nil {
		panic("no match found")
	}

	res.BindTempsAndOuts(res.Bind, s.alloc)

	return res.Emit()
}

func (s *Selector) selectBest(pat *dsl.Node) *MatchResult {
	var best *MatchResult
	bestCost := int(^uint(0) >> 1) // max int

	for _, rule := range s.Rules {
		env := map[string]*dsl.Value{}
		classes := make(map[string]string, len(rule.In)+1+len(rule.Temps))
		for _, v := range rule.In {
			classes[v.Name] = v.Kind
		}

		classes[rule.Out.Name] = rule.Out.Kind

		for _, v := range rule.Temps {
			classes[v.Name] = v.Kind
		}

		if match(rule.Pattern, pat, env, classes) && rule.CheckPredicates(env) {
			if rule.Cost < bestCost {
				best = &MatchResult{Rule: rule, Pattern: pat, Bind: env}
				bestCost = rule.Cost
			}
		}
	}

	return best
}
