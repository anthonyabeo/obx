package ast

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// Module
// --------------
type Module struct {
	BName string
	EName string

	MetaParams []*MetaSection
	ImportList []*Import
	DeclSeq    []Declaration
	StmtSeq    []Statement

	Env   *Environment
	edges map[string]CompilationUnit

	Pos *report.Position
	Rng *report.Range
}

func NewModule(pos *report.Position) *Module {
	return &Module{Pos: pos, edges: map[string]CompilationUnit{}}
}

func (m *Module) Edges() map[string]CompilationUnit         { return m.edges }
func (m *Module) addEdge(name string, unit CompilationUnit) { m.edges[name] = unit }
func (m *Module) Name() string                              { return m.BName }
func (m *Module) ListImport() []*Import                     { return m.ImportList }
func (m *Module) Accept(vst Visitor) any                    { return vst.VisitModule(m) }
func (m *Module) String() string                            { panic("implement me") }
func (m *Module) Position() *report.Position                { return m.Pos }
func (m *Module) Range() *report.Range                      { return m.Rng }

type MetaSection struct {
	Mode    token.Kind
	Ids     []string
	TyConst Type

	Pos *report.Position
	Rng *report.Range
}

func (ms *MetaSection) Accept(vst Visitor) any     { return vst.VisitMetaSection(ms) }
func (ms *MetaSection) String() string             { panic("implement me") }
func (ms *MetaSection) Position() *report.Position { return ms.Pos }
func (ms *MetaSection) Range() *report.Range       { return ms.Rng }

// Definition
// -------------------
type Definition struct {
	BName string
	EName string

	ImportList []*Import
	DeclSeq    []Declaration

	edges map[string]CompilationUnit

	Pos *report.Position
	Rng *report.Range
}

func NewDefinition(pos *report.Position) *Definition {
	return &Definition{Pos: pos}
}

func (def *Definition) Edges() map[string]CompilationUnit         { return def.edges }
func (def *Definition) addEdge(name string, unit CompilationUnit) { def.edges[name] = unit }
func (def *Definition) Accept(vst Visitor) any                    { return vst.VisitDefinition(def) }
func (def *Definition) Name() string                              { return def.BName }
func (def *Definition) ListImport() []*Import                     { return def.ImportList }
func (def *Definition) String() string                            { panic("implement me") }
func (def *Definition) Position() *report.Position                { return def.Pos }
func (def *Definition) Range() *report.Range                      { return def.Rng }
