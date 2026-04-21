package directive

import (
	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

// ResolverFromContext returns a Resolver that looks up names in ctx.Directives
// first, then falls back to compile-time CONSTs visible in ctx.Env.
func ResolverFromContext(ctx *compiler.Context) Resolver {
	return func(name string) (any, bool) {
		if v, ok := ctx.Directives[name]; ok {
			return v, true
		}
		sym := ctx.Env.Lookup(name)
		if sym == nil || sym.Kind() != ast.ConstantSymbolKind {
			return nil, false
		}
		constSym := sym.(*ast.ConstantSymbol)
		return EvalConstExprValue(constSym.Value)
	}
}
