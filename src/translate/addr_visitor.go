package translate

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// AddrVisitor ...
type AddrVisitor struct {
	Visitor
}

func (l *AddrVisitor) VisitIdentifier(id *ast.Ident) {
	alloc := l.env.Lookup(id.Name).Alloca()
	if alloc == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.IRValue = alloc
}

func (l *AddrVisitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(l)
	if d.Selector != nil {
		d.Selector.Accept(l)
		d.IRValue = d.Selector.Value()
		return
	}

	d.IRValue = d.QualifiedIdent.Value()
}
