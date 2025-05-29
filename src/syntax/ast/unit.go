package ast

import (
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

	StartOffset int
	EndOffset   int
}

func NewModule(pos int, env *Environment) *Module {
	return &Module{StartOffset: pos, edges: map[string]CompilationUnit{}, Env: env}
}

// func (m *Module) Edges() map[string]CompilationUnit         { return m.edges }
// func (m *Module) addEdge(name string, unit CompilationUnit) { m.edges[name] = unit }
func (m *Module) Name() string           { return m.BName }
func (m *Module) Imports() []*Import     { return m.ImportList }
func (m *Module) Accept(vst Visitor) any { return vst.VisitModule(m) }
func (m *Module) String() string         { panic("implement me") }
func (m *Module) Pos() int               { return m.StartOffset }
func (m *Module) End() int               { return m.EndOffset }

type MetaSection struct {
	Mode    token.Kind
	Ids     []*IdentifierDef
	TyConst Type

	StartOffset int
	EndOffset   int
}

func (ms *MetaSection) Accept(vst Visitor) any { return vst.VisitMetaSection(ms) }
func (ms *MetaSection) String() string         { panic("implement me") }
func (ms *MetaSection) Pos() int               { return ms.StartOffset }
func (ms *MetaSection) End() int               { return ms.EndOffset }

// Definition
// -------------------
type Definition struct {
	BName string
	EName string

	ImportList []*Import
	DeclSeq    []Declaration

	edges map[string]CompilationUnit

	StartOffset int
	EndOffset   int
}

func NewDefinition(pos int) *Definition {
	return &Definition{StartOffset: pos}
}

// func (def *Definition) Edges() map[string]CompilationUnit         { return def.edges }
// func (def *Definition) addEdge(name string, unit CompilationUnit) { def.edges[name] = unit }
func (def *Definition) Accept(vst Visitor) any { return vst.VisitDefinition(def) }
func (def *Definition) Name() string           { return def.BName }
func (def *Definition) Imports() []*Import     { return def.ImportList }
func (def *Definition) String() string         { panic("implement me") }
func (def *Definition) Pos() int               { return def.StartOffset }
func (def *Definition) End() int               { return def.EndOffset }
