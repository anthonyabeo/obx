package ir

type Type interface {
	String() string
	ty()
}

var (
	VoidType *Void

	Int8Type  *Int8
	Int32Type *Int32
)

func init() {
	VoidType = &Void{}

	Int8Type = &Int8{numBits: 8}
	Int32Type = &Int32{numBits: 32}
}

func GetVoidType() *Void   { return VoidType }
func GetInt32Type() *Int32 { return Int32Type }
func GetInt8Type() *Int8   { return Int8Type }

// IntegerType ...
// ----------------------
type IntegerType interface {
	Type
	BitWidth() uint
}

func CreateIntegerType(numBits uint) IntegerType {
	switch numBits {
	case 1:
	case 8:
		return Int8Type
	case 16:
	case 32:
		return Int32Type
	case 64:

	}

	return nil
}

// Int32 ...
// ----------------------
type Int32 struct {
	numBits uint
}

func (Int32) ty()              {}
func (Int32) String() string   { return "i32" }
func (i Int32) BitWidth() uint { return i.numBits }

// Int8 ...
// ----------------------
type Int8 struct {
	numBits uint
}

func (Int8) ty()              {}
func (Int8) String() string   { return "i8" }
func (i Int8) BitWidth() uint { return i.numBits }

// PointerType ...
// --------------------
type PointerType struct {
	elemType Type
}

func (PointerType) ty()              {}
func (PointerType) String() string   { return "ptr" }
func (p PointerType) ElemType() Type { return p.elemType }

func CreatePointerType(ty Type) *PointerType {
	return &PointerType{ty}
}

// Void ...
// ---------------
type Void struct{}

func (Void) ty()            {}
func (Void) String() string { return "void" }

// FunctionType ...
// ---------------------
type FunctionType struct {
	varArgs bool
	retType Type
	args    []Argument
}

func (f FunctionType) IsVarArg() bool      { return f.varArgs }
func (f FunctionType) NumArgs() int        { return len(f.args) }
func (f FunctionType) ArgType(i uint) Type { return f.args[i].Type() }
func (f FunctionType) ReturnType() Type    { return f.retType }
func (f FunctionType) String() string      { panic("implement me") }
func (FunctionType) ty()                   {}

// LabelType ...
// ---------------------
type LabelType struct {
	name string
}

func (l LabelType) String() string { return l.name }
func (l LabelType) ty()            {}
