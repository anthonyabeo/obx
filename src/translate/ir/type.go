package ir

type Type interface {
	String() string
	llvmType()
}

var LLVMVoidType *VoidType

func init() {
	LLVMVoidType = &VoidType{}
}

func GetVoidType() *VoidType {
	return LLVMVoidType
}

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
		return &Int8{numBits}
	case 16:
	case 32:
		return &Int32{numBits}
	case 64:

	}

	return nil
}

// Int32 ...
// ----------------------
type Int32 struct {
	numBits uint
}

func (i Int32) llvmType()      {}
func (i Int32) String() string { return "i32" }
func (i Int32) BitWidth() uint { return i.numBits }

// Int8 ...
// ----------------------
type Int8 struct {
	numBits uint
}

func (i Int8) llvmType()      {}
func (i Int8) String() string { return "i8" }
func (i Int8) BitWidth() uint { return i.numBits }

// PointerType ...
// --------------------
type PointerType struct {
	ty Type
}

func (p PointerType) String() string {
	return "ptr"
}

func (p PointerType) llvmType() {}

func CreatePointerType(ty Type) *PointerType {
	return &PointerType{ty}
}

// VoidType ...
// ---------------
type VoidType struct {
}

func (p VoidType) String() string {
	return "void"
}

func (VoidType) llvmType() {}
