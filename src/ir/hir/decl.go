package hir

import "fmt"

type VarDecl struct {
	Name string
	Type Type
}

type ConstDecl struct {
	Name  string
	Type  Type
	Value ConstValue
}

type TypeDecl struct {
	Name string
	Type Type
}

func (*VarDecl) isDecl()   {}
func (*ConstDecl) isDecl() {}
func (*TypeDecl) isDecl()  {}

func (v *VarDecl) String() string   { return fmt.Sprintf("%s: %s", v.Name, v.Type) }
func (c *ConstDecl) String() string { return fmt.Sprintf("%s = %s", c.Name, c.Value) }
func (t *TypeDecl) String() string  { return fmt.Sprintf("%s = %s", t.Name, t.Type) }
