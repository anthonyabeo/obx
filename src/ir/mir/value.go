package mir

import (
	"fmt"
)

type Value interface {
	Name() string
	BaseName() string
	String() string
	Type() Type
	IsMem() bool
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
		Ident    string
		OrigName string
		Typ      Type
		Size     int
	}

	Local struct {
		Ident    string
		OrigName string
		Typ      Type
		Size     int
	}

	Param struct {
		Ident    string
		OrigName string
		Kind     string
		Typ      Type
		Size     int
	}

	NamedConst struct {
		Ident      string
		OrigName   string
		ConstValue Value
		Typ        Type
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

	GlobalVariable struct {
		OrigName string
		Ident    string
		Typ      Type
		Size     int
	}

	Mem struct {
		Base Value // Address of the memory location
		Offs int64
	}
)

func (m Mem) Name() string     { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) BaseName() string { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) String() string   { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m Mem) Type() Type       { return m.Base.Type() }
func (m Mem) IsMem() bool      { return true }

func (o *Temp) Type() Type       { return o.Typ }
func (o *Temp) Name() string     { return o.Ident }
func (o *Temp) BaseName() string { return o.OrigName }
func (o *Temp) String() string   { return o.Ident }
func (o *Temp) IsMem() bool      { return false }

func (o *Local) Type() Type       { return o.Typ }
func (o *Local) Name() string     { return o.Ident }
func (o *Local) BaseName() string { return o.OrigName }
func (o *Local) String() string   { return o.Ident }
func (o *Local) IsMem() bool      { return true }

func (o *Param) Type() Type       { return o.Typ }
func (o *Param) Name() string     { return o.Ident }
func (o *Param) BaseName() string { return o.OrigName }
func (o *Param) String() string   { return o.Ident }
func (o *Param) IsMem() bool {
	if o.Kind == "VAR" {
		return true
	}
	return false
}

func (o NamedConst) Type() Type       { return o.Typ }
func (o NamedConst) Name() string     { return o.Ident }
func (o NamedConst) BaseName() string { return o.OrigName }
func (o NamedConst) String() string   { return o.Ident }
func (o NamedConst) Const()           {}
func (o NamedConst) Value() any       { return o.ConstValue }
func (o NamedConst) IsMem() bool      { return false }

func (o IntegerLit) Const()           {}
func (o IntegerLit) Type() Type       { return o.Typ }
func (o IntegerLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o IntegerLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o IntegerLit) String() string   { return fmt.Sprintf("%d", o.LitValue) }
func (o IntegerLit) Value() any       { return o.LitValue }
func (o IntegerLit) IsMem() bool      { return false }

func (o FloatLit) Const()           {}
func (o FloatLit) Type() Type       { return o.Typ }
func (o FloatLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o FloatLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o FloatLit) String() string   { return fmt.Sprintf("%f", o.LitValue) }
func (o FloatLit) Value() any       { return o.LitValue }
func (o FloatLit) IsMem() bool      { return false }

func (o CharLit) Const()           {}
func (o CharLit) Type() Type       { return o.Typ }
func (o CharLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) String() string   { return fmt.Sprintf("%v", o.LitValue) }
func (o CharLit) Value() any       { return o.LitValue }
func (o CharLit) IsMem() bool      { return false }

func (StrLit) Const()             {}
func (o StrLit) Type() Type       { return o.Typ }
func (o StrLit) Name() string     { return o.LitName }
func (o StrLit) BaseName() string { return o.LitName }
func (o StrLit) String() string   { return o.LitName }
func (o StrLit) Value() any       { return o.LitValue }
func (o StrLit) IsMem() bool      { return false }

func (v *GlobalVariable) Type() Type          { return v.Typ }
func (v *GlobalVariable) Name() string        { return "@" + v.Ident }
func (v *GlobalVariable) BaseName() string    { return v.OrigName }
func (v *GlobalVariable) SetName(name string) { v.Ident = name }
func (v *GlobalVariable) String() string      { return "@" + v.Ident }
func (v *GlobalVariable) IsMem() bool         { return true }
