package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

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
}

func NewModule() *Module {
	return &Module{edges: map[string]CompilationUnit{}}
}

func (m *Module) Edges() map[string]CompilationUnit         { return m.edges }
func (m *Module) addEdge(name string, unit CompilationUnit) { m.edges[name] = unit }
func (m *Module) Name() string                              { return m.BName }
func (m *Module) ListImport() []*Import                     { return m.ImportList }
func (m *Module) Accept(vst Visitor)                        { vst.VisitModule(m) }
func (m *Module) String() string                            { panic("implement me") }

type MetaSection struct {
	Mode    token.Kind
	Ids     []string
	TyConst Type
}

// Definition
// -------------------
type Definition struct {
	BName string
	EName string

	ImportList []*Import
	DeclSeq    []Declaration

	edges map[string]CompilationUnit
}

func NewDefinition() *Definition {
	return &Definition{}
}

func (def *Definition) Edges() map[string]CompilationUnit         { return def.edges }
func (def *Definition) addEdge(name string, unit CompilationUnit) { def.edges[name] = unit }
func (def *Definition) Accept(vst Visitor)                        { vst.VisitDefinition(def) }
func (def *Definition) Name() string                              { return def.BName }
func (def *Definition) ListImport() []*Import                     { return def.ImportList }
func (def *Definition) String() string                            { panic("implement me") }
