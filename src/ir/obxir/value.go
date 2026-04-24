package obxir

import "fmt"

// Value is implemented by every obxir value: temporaries, locals, parameters,
// global variables, constants, and memory references.
type Value interface {
	Name() string
	BaseName() string
	String() string
	Type() Type
	IsMem() bool
}

// Constant is a Value that is known at compile time.
type Constant interface {
	Value
	Const()
	Value() any
}

// TypeValue wraps a Type so it can be used as a Value in the small number of
// instructions that need a type operand (IS type tests, SIZE, MIN, MAX builtins).
type TypeValue struct {
	Ty Type
}

func (t *TypeValue) Name() string     { return t.Ty.String() }
func (t *TypeValue) BaseName() string { return t.Ty.String() }
func (t *TypeValue) String() string   { return t.Ty.String() }
func (t *TypeValue) Type() Type       { return t.Ty }
func (t *TypeValue) IsMem() bool      { return false }

// Well-known boolean constants.
var (
	True  = &IntegerLit{LitValue: 1, Signed: true, Bits: 1, Typ: Int1Type}
	False = &IntegerLit{LitValue: 0, Signed: true, Bits: 1, Typ: Int1Type}
)

// ─── Value types ──────────────────────────────────────────────────────────

type (
	// Temp is an SSA temporary (virtual register).
	Temp struct {
		Ident    string
		OrigName string
		Typ      Type
		Size     int
	}

	// Local is a named local variable (addressable).
	Local struct {
		Ident    string
		OrigName string
		Typ      Type
		Size     int
	}

	// Param is a formal parameter.
	Param struct {
		Ident    string
		OrigName string
		Kind     string // "VALUE", "VAR", "IN"
		Typ      Type
		Size     int
	}

	// NamedConst is a declared constant with a name.
	NamedConst struct {
		Ident      string
		OrigName   string
		ConstValue Value
		Typ        Type
		Size       int
	}

	// IntegerLit is an integer constant literal.
	IntegerLit struct {
		LitValue uint64
		Signed   bool
		Bits     uint
		Typ      Type
	}

	// FloatLit is a floating-point constant literal.
	FloatLit struct {
		LitValue float64
		Bits     uint
		Typ      Type
	}

	// CharLit is a character constant literal.
	CharLit struct {
		LitValue []rune
		Typ      Type
	}

	// StrLit is a string constant literal.
	StrLit struct {
		LitName  string
		LitValue string
		Typ      Type
	}

	// GlobalVariable is a module-level variable.
	GlobalVariable struct {
		OrigName string
		Ident    string
		Typ      Type
		Size     int
	}

	// Mem represents a memory address: Base + Offs.
	Mem struct {
		Base Value
		Offs int64
	}
)

// Uint32ArrayConst represents a compile-time array of uint32 values.
type Uint32ArrayConst struct {
	Ident  string
	Values []uint32
	Typ    Type
}

// FuncPtrArrayConst represents a compile-time array of function pointer
// symbol names (module-level function-pointer table). Each entry is the
// mangled symbol name; an empty string represents a NULL/0 entry.
type FuncPtrArrayConst struct {
	Ident     string
	FuncNames []string
	Typ       Type
}

// RTTIConst is a small POD describing RTTI payloads: pointer to name and size.
type RTTIConst struct {
	Ident   string
	NameSym string // symbol name of the string for the type name
	Size    uint64
	Typ     Type
}

// --- Uint32ArrayConst methods ---
func (o *Uint32ArrayConst) Const()           {}
func (o *Uint32ArrayConst) Type() Type       { return o.Typ }
func (o *Uint32ArrayConst) Name() string     { return o.Ident }
func (o *Uint32ArrayConst) BaseName() string { return o.Ident }
func (o *Uint32ArrayConst) String() string   { return o.Ident }
func (o *Uint32ArrayConst) Value() any       { return o.Values }
func (o *Uint32ArrayConst) IsMem() bool      { return false }

// --- FuncPtrArrayConst methods ---
func (o *FuncPtrArrayConst) Const()           {}
func (o *FuncPtrArrayConst) Type() Type       { return o.Typ }
func (o *FuncPtrArrayConst) Name() string     { return o.Ident }
func (o *FuncPtrArrayConst) BaseName() string { return o.Ident }
func (o *FuncPtrArrayConst) String() string   { return o.Ident }
func (o *FuncPtrArrayConst) Value() any       { return o.FuncNames }
func (o *FuncPtrArrayConst) IsMem() bool      { return false }

// --- RTTIConst methods ---
func (o *RTTIConst) Const()           {}
func (o *RTTIConst) Type() Type       { return o.Typ }
func (o *RTTIConst) Name() string     { return o.Ident }
func (o *RTTIConst) BaseName() string { return o.Ident }
func (o *RTTIConst) String() string   { return o.Ident }
func (o *RTTIConst) Value() any {
	return struct {
		Name string
		Size uint64
	}{o.NameSym, o.Size}
}
func (o *RTTIConst) IsMem() bool { return false }

// ─── Temp ─────────────────────────────────────────────────────────────────

func (o *Temp) Type() Type       { return o.Typ }
func (o *Temp) Name() string     { return o.Ident }
func (o *Temp) BaseName() string { return o.OrigName }
func (o *Temp) String() string   { return o.Ident }
func (o *Temp) IsMem() bool      { return false }

// ─── Local ────────────────────────────────────────────────────────────────

func (o *Local) Type() Type       { return o.Typ }
func (o *Local) Name() string     { return o.Ident }
func (o *Local) BaseName() string { return o.OrigName }
func (o *Local) String() string   { return o.Ident }
func (o *Local) IsMem() bool      { return true }

// ─── Param ────────────────────────────────────────────────────────────────

func (o *Param) Type() Type       { return o.Typ }
func (o *Param) Name() string     { return o.Ident }
func (o *Param) BaseName() string { return o.OrigName }
func (o *Param) String() string   { return o.Ident }
func (o *Param) IsMem() bool      { return o.Kind == "VAR" || o.Kind == "IN" }

// ─── NamedConst ───────────────────────────────────────────────────────────

func (o *NamedConst) Type() Type       { return o.Typ }
func (o *NamedConst) Name() string     { return o.Ident }
func (o *NamedConst) BaseName() string { return o.OrigName }
func (o *NamedConst) String() string   { return o.Ident }
func (o *NamedConst) Const()           {}
func (o *NamedConst) Value() any       { return o.ConstValue }
func (o *NamedConst) IsMem() bool      { return false }

// ─── IntegerLit ───────────────────────────────────────────────────────────

func (o *IntegerLit) Const()           {}
func (o *IntegerLit) Type() Type       { return o.Typ }
func (o *IntegerLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o *IntegerLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o *IntegerLit) String() string   { return fmt.Sprintf("%d", o.LitValue) }
func (o *IntegerLit) Value() any       { return o.LitValue }
func (o *IntegerLit) IsMem() bool      { return false }

// ─── FloatLit ─────────────────────────────────────────────────────────────

func (o *FloatLit) Const()           {}
func (o *FloatLit) Type() Type       { return o.Typ }
func (o *FloatLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o *FloatLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o *FloatLit) String() string   { return fmt.Sprintf("%f", o.LitValue) }
func (o *FloatLit) Value() any       { return o.LitValue }
func (o *FloatLit) IsMem() bool      { return false }

// ─── CharLit ──────────────────────────────────────────────────────────────

func (o *CharLit) Const()           {}
func (o *CharLit) Type() Type       { return o.Typ }
func (o *CharLit) Name() string     { return fmt.Sprintf("%v", o.LitValue) }
func (o *CharLit) BaseName() string { return fmt.Sprintf("%v", o.LitValue) }
func (o *CharLit) String() string   { return fmt.Sprintf("%v", o.LitValue) }
func (o *CharLit) Value() any       { return o.LitValue }
func (o *CharLit) IsMem() bool      { return false }

// ─── StrLit ───────────────────────────────────────────────────────────────

func (o *StrLit) Const()           {}
func (o *StrLit) Type() Type       { return o.Typ }
func (o *StrLit) Name() string     { return o.LitName }
func (o *StrLit) BaseName() string { return o.LitName }
func (o *StrLit) String() string   { return o.LitName }
func (o *StrLit) Value() any       { return o.LitValue }
func (o *StrLit) IsMem() bool      { return false }

// ─── GlobalVariable ───────────────────────────────────────────────────────

func (v *GlobalVariable) Type() Type          { return v.Typ }
func (v *GlobalVariable) Name() string        { return "@" + v.Ident }
func (v *GlobalVariable) BaseName() string    { return v.OrigName }
func (v *GlobalVariable) SetName(name string) { v.Ident = name }
func (v *GlobalVariable) String() string      { return "@" + v.Ident }
func (v *GlobalVariable) IsMem() bool         { return true }

// ─── Mem ──────────────────────────────────────────────────────────────────

func (m *Mem) Name() string     { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m *Mem) BaseName() string { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m *Mem) String() string   { return fmt.Sprintf("[%s + %d]", m.Base.Name(), m.Offs) }
func (m *Mem) Type() Type       { return m.Base.Type() }
func (m *Mem) IsMem() bool      { return true }
