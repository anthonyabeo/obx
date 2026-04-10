// Package testutil provides shared test helpers for compiler pipeline tests.
package testutil

import (
	"testing"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/mir"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ParseSourceAndLowerToMIR parses ctx.Content, runs semantic analysis, lowers
// through HIR and returns the resulting MIR program. It calls t.Fatal on any
// parse or semantic error.
func ParseSourceAndLowerToMIR(t *testing.T, ctx *diag.Context) *mir.Program {
	t.Helper()

	p := parser.NewParser(ctx)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("Parser errors")
	}

	obx := ast.NewOberonX()
	obx.AddUnit(unit)

	s := sema.NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("semantics errors")
	}

	gen := desugar.NewGenerator(ctx, obx)
	HIRProgram := gen.Generate()

	builder := mir.NewIRBuilder(ctx)
	return builder.Build(HIRProgram)
}

