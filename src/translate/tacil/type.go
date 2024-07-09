package tacil

import "fmt"

type Type interface {
	String() string
	ty()
	IsIntegerTy() bool
	IsPtrTy() bool
	IsFuncTy() bool
	IsVoidTy() bool
}

var (
	VoidType *Void

	Int1Type  *Int1
	Int16Type *Int16
	Int8Type  *Int8
	Int32Type *Int32
	Int64Type *Int64
)

func init() {
	VoidType = &Void{}

	Int1Type = &Int1{numBits: 1}
	Int8Type = &Int8{numBits: 8}
	Int16Type = &Int16{numBits: 16}
	Int32Type = &Int32{numBits: 32}
	Int64Type = &Int64{numBits: 64}
}

// IntegerType ...
// ----------------------
type IntegerType interface {
	Type
	BitWidth() uint
}

func GetIntegerType(numBits uint) IntegerType {
	switch numBits {
	case 1:
		return Int1Type
	case 8:
		return Int8Type
	case 16:
		return Int16Type
	case 32:
		return Int32Type
	case 64:
		return Int64Type
	default:
		panic(fmt.Sprintf("cannot integer type with bit-width of size'%d'", numBits))
	}
}

// Int1 ...
// ----------------------
type Int1 struct {
	numBits uint
}

func (Int1) IsIntegerTy() bool { return true }
func (Int1) IsPtrTy() bool     { return false }
func (Int1) IsFuncTy() bool    { return false }
func (Int1) IsVoidTy() bool    { return false }
func (Int1) ty()               {}
func (Int1) String() string    { return "i1" }
func (i Int1) BitWidth() uint  { return i.numBits }

// Int32 ...
// ----------------------
type Int32 struct {
	numBits uint
}

func (Int32) IsIntegerTy() bool { return true }
func (Int32) IsPtrTy() bool     { return false }
func (Int32) IsFuncTy() bool    { return false }
func (Int32) IsVoidTy() bool    { return false }
func (Int32) ty()               {}
func (Int32) String() string    { return "i32" }
func (i Int32) BitWidth() uint  { return i.numBits }

// Int8 ...
// ----------------------
type Int8 struct {
	numBits uint
}

func (Int8) IsIntegerTy() bool { return true }
func (Int8) IsPtrTy() bool     { return false }
func (Int8) IsFuncTy() bool    { return false }
func (Int8) IsVoidTy() bool    { return false }
func (Int8) ty()               {}
func (Int8) String() string    { return "i8" }
func (i Int8) BitWidth() uint  { return i.numBits }

// Int64 ...
// ----------------------
type Int64 struct {
	numBits uint
}

func (Int64) IsIntegerTy() bool { return true }
func (Int64) IsPtrTy() bool     { return false }
func (Int64) IsFuncTy() bool    { return false }
func (Int64) IsVoidTy() bool    { return false }
func (Int64) ty()               {}
func (Int64) String() string    { return "i64" }
func (i Int64) BitWidth() uint  { return i.numBits }

// Int16 ...
// ----------------------
type Int16 struct {
	numBits uint
}

func (Int16) IsIntegerTy() bool { return true }
func (Int16) IsPtrTy() bool     { return false }
func (Int16) IsFuncTy() bool    { return false }
func (Int16) IsVoidTy() bool    { return false }
func (Int16) ty()               {}
func (Int16) String() string    { return "i16" }
func (i Int16) BitWidth() uint  { return i.numBits }

// PointerType ...
// --------------------
type PointerType struct {
	elemTy Type
}

func (PointerType) IsIntegerTy() bool { return false }
func (PointerType) IsPtrTy() bool     { return true }
func (PointerType) IsFuncTy() bool    { return false }
func (PointerType) IsVoidTy() bool    { return false }
func (PointerType) ty()               {}
func (PointerType) String() string    { return "ptr" }
func (p PointerType) ElemType() Type  { return p.elemTy }

func CreatePointerType(ty Type) *PointerType {
	return &PointerType{ty}
}

// Void ...
// ---------------
type Void struct{}

func (Void) IsIntegerTy() bool { return false }
func (Void) IsPtrTy() bool     { return false }
func (Void) IsFuncTy() bool    { return false }
func (Void) IsVoidTy() bool    { return true }
func (Void) ty()               {}
func (Void) String() string    { return "void" }

// FunctionType ...
// ---------------------
type FunctionType struct {
	varArgs bool
	retTy   Type
	args    []Type
}

func CreateFunctionType(args []Type, retTy Type, varArgs bool) *FunctionType {
	return &FunctionType{
		varArgs,
		retTy,
		args,
	}
}

func (f FunctionType) IsVarArg() bool      { return f.varArgs }
func (f FunctionType) NumArgs() int        { return len(f.args) }
func (f FunctionType) ArgType(i uint) Type { return f.args[i] }
func (f FunctionType) ReturnType() Type    { return f.retTy }
func (f FunctionType) String() string      { panic("implement me") }
func (FunctionType) ty()                   {}
func (FunctionType) IsIntegerTy() bool     { return false }
func (FunctionType) IsPtrTy() bool         { return false }
func (FunctionType) IsFuncTy() bool        { return true }
func (FunctionType) IsVoidTy() bool        { return false }

// LabelType ...
// ---------------------
type LabelType struct {
	name string
}

func (LabelType) IsIntegerTy() bool { return false }
func (LabelType) IsPtrTy() bool     { return false }
func (LabelType) IsFuncTy() bool    { return false }
func (LabelType) IsVoidTy() bool    { return false }
func (l LabelType) String() string  { return l.name }
func (LabelType) ty()               {}
