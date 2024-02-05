package translate

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// LValueVisitor ...
type LValueVisitor struct {
	Visitor
}

func (l *LValueVisitor) VisitIdentifier(id *ast.Ident) {
	alloc := l.env.Lookup(id.Name).Alloca()
	if alloc == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.IRValue = alloc
}

func (l *LValueVisitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(l)
	d.IRValue = d.QualifiedIdent.Value()
}
