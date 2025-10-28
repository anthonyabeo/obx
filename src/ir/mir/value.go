package mir

import (
	"fmt"
)

type Value interface {
	Name() string
	BaseName() string
	String() string
	Type() Type
}

type Constant interface {
	Value
	Const()
	Value() any
}

var (
	True  = &IntegerLit{LitValue: 1, Signed: true, Bits: 1, Typ: Int1Type}
	False = &IntegerLit{LitValue: 0, Signed: true, Bits: 1, Typ: Int1Type}
)

type (
	Temp struct {
		ID     string
		BName  string
		Typ    Type
		Offset int
		Size   int
	}

	Local struct {
		ID     string
		BName  string
		Typ    Type
		Offset int
		Size   int
	}

	Param struct {
		ID     string
		BName  string
		Typ    Type
		Offset int
		Size   int
	}

	NamedConst struct {
		ID         string
		ConstValue Value
		Typ        Type
		Offset     int
		Size       int
	}

	IntegerLit struct {
		LitValue uint64
		Signed   bool
		Bits     uint
		Typ      Type
	}

	FloatLit struct {
		LitValue float64
		Bits     uint
		Typ      Type
	}

	CharLit struct {
		LitValue []rune
		Typ      Type
	}

	StrLit struct {
		LitName  string
		LitValue string
		Typ      Type
	}

	Global struct {
		NameStr string
		BName   string
		Kind    string
		Typ     Type
		Value   any
		Offset  int
		Size    int
	}

	AddrOf struct {
		Obj Value
	}

	Mem struct {
		Base Value // Address of the memory location
		Offs int
	}
)

func (m Mem) Name() string     { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) BaseName() string { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) String() string   { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) Type() Type       { return m.Base.Type() }

func (o *Temp) Type() Type       { return o.Typ }
func (o *Temp) Name() string     { return o.ID }
func (o *Temp) BaseName() string { return o.BName }
func (o *Temp) String() string   { return o.ID }

func (o *Local) Type() Type       { return o.Typ }
func (o *Local) Name() string     { return o.ID }
func (o *Local) BaseName() string { return o.BName }
func (o *Local) String() string   { return o.ID }

func (o *Param) Type() Type       { return o.Typ }
func (o *Param) Name() string     { return o.ID }
func (o *Param) BaseName() string { return o.BName }
func (o *Param) String() string   { return o.ID }

func (o NamedConst) Type() Type       { return o.Typ }
func (o NamedConst) Name() string     { return o.ID }
func (o NamedConst) BaseName() string { return o.ID }
func (o NamedConst) String() string   { return o.ID }
func (o NamedConst) Const()           {}
func (o NamedConst) Value() any       { return o.ConstValue }

func (o IntegerLit) Const()           {}
func (o IntegerLit) Type() Type       { return o.Typ }
func (o IntegerLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o IntegerLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o IntegerLit) String() string   { return fmt.Sprintf("%d", o.LitValue) }
func (o IntegerLit) Value() any       { return o.LitValue }

func (o FloatLit) Const()           {}
func (o FloatLit) Type() Type       { return o.Typ }
func (o FloatLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o FloatLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o FloatLit) String() string   { return fmt.Sprintf("%f", o.LitValue) }
func (o FloatLit) Value() any       { return o.LitValue }

func (o CharLit) Const()           {}
func (o CharLit) Type() Type       { return o.Typ }
func (o CharLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) String() string   { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) Value() any       { return o.LitValue }

func (StrLit) Const()             {}
func (o StrLit) Type() Type       { return o.Typ }
func (o StrLit) Name() string     { return o.LitName }
func (o StrLit) BaseName() string { return o.LitName }
func (o StrLit) String() string   { return o.LitName }
func (o StrLit) Value() any       { return o.LitValue }

func (o *Global) Type() Type          { return o.Typ }
func (o *Global) Name() string        { return "@" + o.NameStr }
func (o *Global) BaseName() string    { return o.BName }
func (o *Global) SetName(name string) { o.NameStr = name }
func (o *Global) String() string      { return "@" + o.NameStr }

func (a AddrOf) Type() Type       { return &PointerType{Ref: a.Obj.Type()} }
func (a AddrOf) Name() string     { return "&" + a.Obj.Name() }
func (a AddrOf) BaseName() string { return "&" + a.Obj.Name() }
func (a AddrOf) String() string   { return "&" + a.Obj.Name() }
