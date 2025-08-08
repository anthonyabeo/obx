package mir

import (
	"fmt"
)

type Value interface {
	value()
	Name() string
	String() string
	Type() Type
}

var (
	True  = IntegerConst{Value: 1, Signed: true, Bits: 1, Typ: Int1Type}
	False = IntegerConst{Value: 0, Signed: true, Bits: 1, Typ: Int1Type}
)

type (
	Temp struct {
		ID      string
		SrcName string
		Typ     Type
		Offset  int
		Size    int
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
		Kind    string
		Typ     Type
		Value   any
		Offset  int
		Size    int
	}

	Addr struct {
		Src *Temp
	}
)

func (Temp) value()         {}
func (Const) value()        {}
func (IntegerConst) value() {}
func (FloatConst) value()   {}
func (CharConst) value()    {}
func (StrConst) value()     {}
func (Global) value()       {}

func (o Temp) Type() Type         { return o.Typ }
func (o Const) Type() Type        { return o.Typ }
func (o IntegerConst) Type() Type { return o.Typ }
func (o FloatConst) Type() Type   { return o.Typ }
func (o CharConst) Type() Type    { return o.Typ }
func (o StrConst) Type() Type     { return o.Typ }
func (o Global) Type() Type       { return o.Typ }

func (o Temp) Name() string         { return o.ID }
func (o Const) Name() string        { return o.ID }
func (o IntegerConst) Name() string { return fmt.Sprintf("%v", o.Value) }
func (o FloatConst) Name() string   { return fmt.Sprintf("%v", o.Value) }
func (o CharConst) Name() string    { return fmt.Sprintf("%v", o.Value) }
func (o StrConst) Name() string     { return fmt.Sprintf("%v", o.Value) }
func (o Global) Name() string       { return o.NameStr }

func (o Temp) String() string         { return o.ID }
func (o Const) String() string        { return o.ID }
func (o IntegerConst) String() string { return fmt.Sprintf("%d %s", o.Value, o.Typ) }
func (o FloatConst) String() string   { return fmt.Sprintf("%f %s", o.Value, o.Typ) }
func (o CharConst) String() string    { return fmt.Sprintf("%v %s", o.Value, o.Typ) }
func (o StrConst) String() string     { return fmt.Sprintf("%s %s", o.Value, o.Typ) }
func (o Global) String() string       { return o.NameStr }

func (a *Addr) value()         {}
func (a *Addr) Type() Type     { return &PointerType{Ref: a.Src.Type()} }
func (a *Addr) Name() string   { return "&" + a.Src.ID }
func (a *Addr) String() string { return "&" + a.Src.ID }
