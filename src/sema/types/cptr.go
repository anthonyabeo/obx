package types

import (
	"fmt"
	"strings"
)

// ── C-interop semantic types (§12) ────────────────────────────────────────────

// CPointerType is the semantic type for CPOINTER TO T (C-interop pointer).
// When Base is VoidType the pointer represents CPOINTER TO VOID (void*).
type CPointerType struct {
	Base Type
}

func (p *CPointerType) String() string { return fmt.Sprintf("CPOINTER TO %s", p.Base.String()) }
func (p *CPointerType) Width() int     { return 8 }
func (p *CPointerType) Alignment() int { return 8 }
func (p *CPointerType) Equals(other Type) bool {
	o, ok := other.(*CPointerType)
	if !ok {
		return false
	}
	return p.Base.Equals(o.Base)
}

// CStructType is the semantic type for a CSTRUCT declaration.
type CStructType struct {
	Fields []*Field
}

func (r *CStructType) String() string {
	var parts []string
	for _, f := range r.Fields {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Name, f.Type.String()))
	}
	return fmt.Sprintf("CSTRUCT { %s }", strings.Join(parts, "; "))
}

func (r *CStructType) Alignment() int {
	maxAlign := 1
	for _, f := range r.Fields {
		if a := f.Type.Alignment(); a > maxAlign {
			maxAlign = a
		}
	}
	return maxAlign
}

func (r *CStructType) Width() int {
	offset := 0
	for _, f := range r.Fields {
		fw := f.Type.Width()
		if fw < 0 {
			return -1
		}
		if fa := f.Type.Alignment(); fa > 0 {
			offset = (offset + fa - 1) / fa * fa
		}
		offset += fw
	}
	if a := r.Alignment(); a > 0 {
		offset = (offset + a - 1) / a * a
	}
	return offset
}

func (r *CStructType) Equals(other Type) bool {
	o, ok := other.(*CStructType)
	if !ok || len(r.Fields) != len(o.Fields) {
		return false
	}
	for i := range r.Fields {
		if r.Fields[i].Name != o.Fields[i].Name || !r.Fields[i].Type.Equals(o.Fields[i].Type) {
			return false
		}
	}
	return true
}

func (r *CStructType) GetField(name string) *Field {
	for _, f := range r.Fields {
		if f.Name == name {
			return f
		}
	}
	return nil
}

// CUnionType is the semantic type for a CUNION declaration.
type CUnionType struct {
	Fields []*Field
}

func (u *CUnionType) String() string {
	var parts []string
	for _, f := range u.Fields {
		parts = append(parts, fmt.Sprintf("%s: %s", f.Name, f.Type.String()))
	}
	return fmt.Sprintf("CUNION { %s }", strings.Join(parts, "; "))
}

func (u *CUnionType) Alignment() int {
	maxAlign := 1
	for _, f := range u.Fields {
		if a := f.Type.Alignment(); a > maxAlign {
			maxAlign = a
		}
	}
	return maxAlign
}

// Width of a union is the maximum field width, rounded up to the union's alignment.
func (u *CUnionType) Width() int {
	maxWidth := 0
	for _, f := range u.Fields {
		fw := f.Type.Width()
		if fw < 0 {
			return -1
		}
		if fw > maxWidth {
			maxWidth = fw
		}
	}
	if a := u.Alignment(); a > 0 {
		maxWidth = (maxWidth + a - 1) / a * a
	}
	return maxWidth
}

func (u *CUnionType) Equals(other Type) bool {
	o, ok := other.(*CUnionType)
	if !ok || len(u.Fields) != len(o.Fields) {
		return false
	}
	for i := range u.Fields {
		if u.Fields[i].Name != o.Fields[i].Name || !u.Fields[i].Type.Equals(o.Fields[i].Type) {
			return false
		}
	}
	return true
}

func (u *CUnionType) GetField(name string) *Field {
	for _, f := range u.Fields {
		if f.Name == name {
			return f
		}
	}
	return nil
}

// CArrayType is the semantic type for a CARRAY declaration.
// Length is -1 for an open (length-less) CARRAY.
type CArrayType struct {
	Length int
	Elem   Type
}

func (a *CArrayType) String() string {
	if a.Length < 0 {
		return fmt.Sprintf("CARRAY OF %s", a.Elem.String())
	}
	return fmt.Sprintf("CARRAY %d OF %s", a.Length, a.Elem.String())
}

func (a *CArrayType) Width() int {
	if a.Length < 0 {
		return -1 // open CARRAY has unknown width
	}
	ew := a.Elem.Width()
	if ew < 0 {
		return -1
	}
	return a.Length * ew
}

func (a *CArrayType) Alignment() int { return a.Elem.Alignment() }

func (a *CArrayType) Equals(other Type) bool {
	o, ok := other.(*CArrayType)
	return ok && a.Length == o.Length && a.Elem.Equals(o.Elem)
}
