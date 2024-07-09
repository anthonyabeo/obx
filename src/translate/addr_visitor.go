package translate

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/translate/tacil"
)

// AddrVisitor ...
type AddrVisitor struct {
	Visitor
}

func (l *AddrVisitor) VisitIdentifier(id *ast.Ident) {
	sym := l.env.Lookup(id.Name)
	if sym == nil {
		panic(fmt.Sprintf("stack allocation for name '%s' not found", id.Name))
	}

	id.IRExpr = tacil.NewTemp(id.Name)
}

func (l *AddrVisitor) VisitDesignator(d *ast.Designator) {
	d.QualifiedIdent.Accept(l)
	if d.Selector == nil {
		d.IRExpr = d.QualifiedIdent.Value()
		return
	}

	switch d.Selector.(type) {
	case *ast.DotOp:
	case *ast.IndexOp:
	case *ast.PtrDref:
	case *ast.TypeGuard:
	}
}
