package hir

import "fmt"

type ConstValue interface {
	Expr
	isConst()
}

type (
	IntConst struct {
		Value int64
		Ty    Type
	}

	RealConst struct {
		Value float64
		Ty    Type
	}

	BoolConst struct {
		Value bool
		Ty    Type
	}

	CharConst struct {
		Value rune
		Ty    Type
	}

	StringConst struct {
		Value string
		Ty    Type
	}

	SetConst struct {
		Elements []int
		Ty       Type
	}

	NilConst struct{}
)

func (*IntConst) isConst()    {}
func (*RealConst) isConst()   {}
func (*BoolConst) isConst()   {}
func (*CharConst) isConst()   {}
func (*StringConst) isConst() {}
func (*SetConst) isConst()    {}
func (*NilConst) isConst()    {}

func (*IntConst) isExpr()    {}
func (*RealConst) isExpr()   {}
func (*BoolConst) isExpr()   {}
func (*CharConst) isExpr()   {}
func (*StringConst) isExpr() {}
func (*SetConst) isExpr()    {}
func (*NilConst) isExpr()    {}

func (*IntConst) Type() Type    { return nil }
func (*RealConst) Type() Type   { return nil }
func (*BoolConst) Type() Type   { return nil }
func (*CharConst) Type() Type   { return nil }
func (*StringConst) Type() Type { return nil }
func (*SetConst) Type() Type    { return nil }
func (*NilConst) Type() Type    { return nil }

func (i *IntConst) String() string  { return fmt.Sprintf("%d", i.Value) }
func (*RealConst) String() string   { panic("not implemented") }
func (b *BoolConst) String() string { return fmt.Sprintf("%t", b.Value) }
func (c *CharConst) String() string { return string(c.Value) }
func (*StringConst) String() string { panic("not implemented") }
func (*SetConst) String() string    { panic("not implemented") }
func (*NilConst) String() string    { panic("not implemented") }

type Op string

const (
	// Binary operators
	Add Op = "+"
	Sub Op = "-"
	Mul Op = "*"
	Div Op = "DIV"
	Mod Op = "MOD"
	And Op = "AND"
	Or  Op = "OR"
	EQ  Op = "="
	NEQ Op = "#"
	LT  Op = "<"
	GT  Op = ">"
	LE  Op = "<="
	GE  Op = ">="
	Not Op = "~"
)
