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
	resolve := NewNameResolver(s.ctx)
	checker := NewTypeChecker(s.ctx)
	flow := NewFlowChecker(s.ctx)

	for _, unit := range s.obx.Units {
		s.ctx.Env.SetCurrentScope(s.ctx.Env.ModuleScope(unit.Name()))

		resolve.Resolve(unit)
		checker.TypeCheck(unit)
		flow.Analyse(unit)
	}
}
