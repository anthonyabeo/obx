// Package testutil provides shared test helpers for compiler pipeline tests.
package testutil

import (
	"testing"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
)

// ParseSourceAndLowerToMIR parses fileName/content, runs semantic analysis,
// lowers through HIR and returns the resulting MIR program. It calls t.Fatal
// on any parse or semantic error.
func ParseSourceAndLowerToMIR(t *testing.T, ctx *compiler.Context, fileName string, content []byte) *obxir.Program {
	t.Helper()

	p := parser.NewParser(ctx, fileName, content)
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

	gen := desugar.NewGenerator(obx)
	HIRProgram := gen.Generate()

	builder := obxir.NewIRBuilder(ctx.Target.WordSize)
	return builder.Build(HIRProgram)
}
