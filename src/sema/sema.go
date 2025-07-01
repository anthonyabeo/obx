package sema

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

type Sema struct {
	ctx *report.Context
	obx *ast.OberonX
}

func NewSema(ctx *report.Context, obx *ast.OberonX) *Sema {
	return &Sema{ctx: ctx, obx: obx}
}

func (s *Sema) Validate() {
	for _, unit := range s.obx.Units {
		s.ctx.Env.SetCurrentScope(s.ctx.Env.ModuleScope(unit.Name()))

		// validate and annotate loops for EXIT support:
		loop := NewFlowControlAnalyzer(s.ctx)
		loop.Analyse(unit)

		resolve := NewNameResolver(s.ctx)
		resolve.Resolve(unit)

		checker := NewTypeChecker(s.ctx)
		checker.TypeCheck(unit)
	}
}
