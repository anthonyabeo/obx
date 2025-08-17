package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

// Module
// --------------
type Module struct {
	BName   string
	EName   string
	IsEntry bool

	MetaParams []*MetaSection
	ImportList []*Import
	DeclSeq    []Declaration
	StmtSeq    []Statement

	StartOffset int
	EndOffset   int
}

func (m *Module) Name() string           { return m.BName }
func (m *Module) Imports() []*Import     { return m.ImportList }
func (m *Module) Accept(vst Visitor) any { return vst.VisitModule(m) }
func (m *Module) String() string         { panic("implement me") }
func (m *Module) Pos() int               { return m.StartOffset }
func (m *Module) End() int               { return m.EndOffset }
func (m *Module) Children() []Node {
	children := make([]Node, 0)
	for _, param := range m.MetaParams {
		children = append(children, param)
	}

	for _, i := range m.ImportList {
		children = append(children, i)
	}

	for _, declaration := range m.DeclSeq {
		children = append(children, declaration)
	}

	for _, statement := range m.StmtSeq {
		children = append(children, statement)
	}

	return children
}

type MetaSection struct {
	Mode    token.Kind
	Ids     []*Identifier
	TyConst Type

	StartOffset int
	EndOffset   int
}

func (ms *MetaSection) Accept(vst Visitor) any { return vst.VisitMetaSection(ms) }
func (ms *MetaSection) String() string         { panic("implement me") }
func (ms *MetaSection) Pos() int               { return ms.StartOffset }
func (ms *MetaSection) End() int               { return ms.EndOffset }
func (ms *MetaSection) Children() []Node {
	children := []Node{ms.TyConst}
	for _, id := range ms.Ids {
		children = append(children, id)
	}

	return children
}

// Definition
// -------------------
type Definition struct {
	BName      string
	EName      string
	ImportList []*Import
	DeclSeq    []Declaration

	StartOffset int
	EndOffset   int
}

func (def *Definition) Accept(vst Visitor) any { return vst.VisitDefinition(def) }
func (def *Definition) Name() string           { return def.BName }
func (def *Definition) Imports() []*Import     { return def.ImportList }
func (def *Definition) String() string         { panic("implement me") }
func (def *Definition) Pos() int               { return def.StartOffset }
func (def *Definition) End() int               { return def.EndOffset }
func (def *Definition) Children() []Node {
	children := make([]Node, 0)

	for _, i := range def.ImportList {
		children = append(children, i)
	}

	for _, declaration := range def.DeclSeq {
		children = append(children, declaration)
	}

	return children
}
