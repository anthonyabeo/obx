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
	alloc, found := l.irSymbolTable[id.Name]
	if !found {
		panic(fmt.Sprintf("memory allocation for name '%s' not found", id.Name))
	}

	id.IRValue = alloc
}

func (l *LValueVisitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(l)
	d.IRValue = d.QualifiedIdent.Value()
}
