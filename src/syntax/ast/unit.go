package ast

import "github.com/anthonyabeo/obx/src/syntax/token"

type UnitKind int

const (
	Invalid UnitKind = iota

	MOD
	DEF
)

type Unit interface {
	Name() string
	Kind() UnitKind
	Accept(Visitor)
}

// Module
// --------------
type Module struct {
	MPos       *token.Position
	BeginName  *Ident
	EndName    *Ident
	MetaParams []*MetaSection
	ImportList []*Import
	DeclSeq    []Declaration
	StmtSeq    []Statement
}

func (m *Module) Accept(vst Visitor)   { vst.VisitModule(m) }
func (m *Module) Name() string         { return m.BeginName.Name }
func (m *Module) Kind() UnitKind       { return MOD }
func (m *Module) Pos() *token.Position { return m.MPos }

type MetaSection struct {
	Mode    token.Token
	Ids     []*Ident
	TyConst Type
}

// Definition
// -------------------
type Definition struct {
	Def        *token.Position
	BeginName  *Ident
	EndName    *Ident
	ImportList []*Import
	DeclSeq    []Declaration
}

func (def *Definition) Accept(vst Visitor) { vst.VisitDefinition(def) }
func (def *Definition) Name() string       { return def.BeginName.Name }
func (def *Definition) Kind() UnitKind     { return DEF }
