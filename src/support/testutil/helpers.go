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

// SourceUnit is a (fileName, content) pair used by ParseMultipleSourcesAndLowerToMIR.
type SourceUnit struct {
	FileName string
	Content  []byte
}

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

// ParseMultipleSourcesAndLowerToMIR parses each SourceUnit in order (e.g.
// DEFINITION files before their importing MODULEs), runs a single semantic
// analysis pass over all units together, and lowers the result to MIR.
// It calls t.Fatal on any parse or semantic error.
func ParseMultipleSourcesAndLowerToMIR(t *testing.T, ctx *compiler.Context, units []SourceUnit) *obxir.Program {
	t.Helper()

	obx := ast.NewOberonX()

	for _, u := range units {
		p := parser.NewParser(ctx, u.FileName, u.Content)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatalf("parse errors in %s", u.FileName)
		}
		obx.AddUnit(unit)
	}

	s := sema.NewSema(ctx, obx)
	s.Validate()

	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		t.Fatalf("semantic errors")
	}

	gen := desugar.NewGenerator(obx)
	HIRProgram := gen.Generate()

	builder := obxir.NewIRBuilder(ctx.Target.WordSize)
	return builder.Build(HIRProgram)
}

