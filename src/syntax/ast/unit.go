package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

// Module
// --------------
type Module struct {
	pos   *token.Position
	BName *Ident
	EName *Ident

	MetaParams []*MetaSection
	ImportList []*Import
	DeclSeq    []Declaration
	StmtSeq    []Statement

	edges map[string]Unit
}

func NewModule(pos *token.Position) *Module {
	return &Module{pos: pos, edges: map[string]Unit{}}
}

func (m *Module) Edges() map[string]Unit         { return m.edges }
func (m *Module) addEdge(name string, unit Unit) { m.edges[name] = unit }
func (m *Module) Name() string                   { return m.BName.Name }
func (m *Module) ListImport() []*Import          { return m.ImportList }
func (m *Module) Accept(vst Visitor)             { vst.VisitModule(m) }
func (m *Module) Pos() *token.Position           { return m.pos }
func (m *Module) End() *token.Position           { panic("implement me") }
func (m *Module) String() string                 { panic("implement me") }

type MetaSection struct {
	Mode    token.Token
	Ids     []*Ident
	TyConst Type
}

// Definition
// -------------------
type Definition struct {
	pos   *token.Position
	BName *Ident
	EName *Ident

	ImportList []*Import
	DeclSeq    []Declaration

	edges map[string]Unit
}

func NewDefinition(pos *token.Position) *Definition {
	return &Definition{pos: pos}
}

func (def *Definition) Edges() map[string]Unit         { return def.edges }
func (def *Definition) addEdge(name string, unit Unit) { def.edges[name] = unit }
func (def *Definition) Pos() *token.Position           { return def.pos }
func (def *Definition) End() *token.Position           { panic("implement me") }
func (def *Definition) Accept(vst Visitor)             { vst.VisitDefinition(def) }
func (def *Definition) Name() string                   { return def.BName.Name }
func (def *Definition) ListImport() []*Import          { return def.ImportList }
func (def *Definition) String() string                 { panic("implement me") }
