package ast

// Attribute represents a single FFI attribute: a name followed by zero or more
// constant-expression values.
//
//	dll "libm"    → Attribute{Name: "dll",     Values: [BasicLit{"libm"}]}
//	prefix "OBX_" → Attribute{Name: "prefix",  Values: [BasicLit{"OBX_"}]}
//	varargs        → Attribute{Name: "varargs", Values: nil}
type Attribute struct {
	Name   string
	Values []Expression

	StartOffset int
	EndOffset   int
}

// AttributeList is the bracket-enclosed list '[' attr { ',' attr } ']' that
// appears on external library DEFINITION modules and on external procedure
// headings inside those modules.
type AttributeList struct {
	Attrs []Attribute

	StartOffset int
	EndOffset   int
}

// DLLName returns the string value of the "dll" attribute, or "" if absent.
func (al *AttributeList) DLLName() string {
	return al.stringAttr("dll")
}

// Prefix returns the string value of the "prefix" attribute, or "" if absent.
func (al *AttributeList) Prefix() string {
	return al.stringAttr("prefix")
}

// Alias returns the string value of the "alias" attribute, or "" if absent.
func (al *AttributeList) Alias() string {
	return al.stringAttr("alias")
}

// HasVarArgs reports whether the "varargs" attribute is present.
func (al *AttributeList) HasVarArgs() bool {
	if al == nil {
		return false
	}
	for _, a := range al.Attrs {
		if a.Name == "varargs" {
			return true
		}
	}
	return false
}

func (al *AttributeList) stringAttr(name string) string {
	if al == nil {
		return ""
	}
	for _, a := range al.Attrs {
		if a.Name == name && len(a.Values) > 0 {
			if lit, ok := a.Values[0].(*BasicLit); ok {
				// Strip surrounding quotes that the scanner leaves on string literals.
				s := lit.Val
				if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
					return s[1 : len(s)-1]
				}
				return s
			}
		}
	}
	return ""
}
