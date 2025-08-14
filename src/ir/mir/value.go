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
}

var (
	True  = &IntegerConst{Value: 1, Signed: true, Bits: 1, Typ: Int1Type}
	False = &IntegerConst{Value: 0, Signed: true, Bits: 1, Typ: Int1Type}
)

type (
	Temp struct {
		ID     string
		BName  string
		Typ    Type
		Offset int
		Size   int
	}

	Const struct {
		ID     string
		Value  Value
		Typ    Type
		Offset int
		Size   int
	}

	IntegerConst struct {
		Value  uint64
		Signed bool
		Bits   uint
		Typ    Type
	}

	FloatConst struct {
		Value float64
		Bits  uint
		Typ   Type
	}

	CharConst struct {
		Value []rune
		Typ   Type
	}

	StrConst struct {
		Value string
		Typ   Type
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

	Addr struct {
		Src Value
	}
)

func (o *Temp) Type() Type       { return o.Typ }
func (o *Temp) Name() string     { return o.ID }
func (o *Temp) BaseName() string { return o.BName }
func (o *Temp) String() string   { return o.ID }

func (o Const) Type() Type       { return o.Typ }
func (o Const) Name() string     { return o.ID }
func (o Const) BaseName() string { return o.ID }
func (o Const) String() string   { return o.ID }

func (o IntegerConst) Const()           {}
func (o IntegerConst) Type() Type       { return o.Typ }
func (o IntegerConst) Name() string     { return fmt.Sprintf("%v", o.Value) }
func (o IntegerConst) BaseName() string { return fmt.Sprintf("%v", o.Value) }
func (o IntegerConst) String() string   { return fmt.Sprintf("%d", o.Value) }

func (o FloatConst) Const()           {}
func (o FloatConst) Type() Type       { return o.Typ }
func (o FloatConst) Name() string     { return fmt.Sprintf("%v", o.Value) }
func (o FloatConst) BaseName() string { return fmt.Sprintf("%v", o.Value) }
func (o FloatConst) String() string   { return fmt.Sprintf("%f", o.Value) }

func (o CharConst) Const()           {}
func (o CharConst) Type() Type       { return o.Typ }
func (o CharConst) Name() string     { return fmt.Sprintf("%v", o.Value) }
func (o CharConst) BaseName() string { return fmt.Sprintf("%v", o.Value) }
func (o CharConst) String() string   { return fmt.Sprintf("%v", o.Value) }

func (StrConst) Const()             {}
func (o StrConst) Type() Type       { return o.Typ }
func (o StrConst) Name() string     { return fmt.Sprintf("%v", o.Value) }
func (o StrConst) BaseName() string { return fmt.Sprintf("%v", o.Value) }
func (o StrConst) String() string   { return fmt.Sprintf("%s", o.Value) }

func (o *Global) Type() Type          { return o.Typ }
func (o *Global) Name() string        { return o.NameStr }
func (o *Global) BaseName() string    { return o.BName }
func (o *Global) SetName(name string) { o.NameStr = name }
func (o *Global) String() string      { return o.NameStr }

func (a Addr) Type() Type       { return &PointerType{Ref: a.Src.Type()} }
func (a Addr) Name() string     { return "&" + a.Src.Name() }
func (a Addr) BaseName() string { return "&" + a.Src.Name() }
func (a Addr) String() string   { return "&" + a.Src.Name() }
