package minir

import (
	"crypto/md5"
	"fmt"
)

// internStringLiteral materializes a module-scope private GlobalConst for s and
// returns a GlobalRef to that symbol. Existing entries are reused per module.
func (l *Lowerer) internStringLiteral(s string, ty Type) Value {
	if l == nil || l.mod == nil {
		return NewConst(s, s, ty)
	}
	if l.stringGlobals == nil {
		l.stringGlobals = make(map[string]*GlobalConst)
	}
	if gc, ok := l.stringGlobals[s]; ok {
		return gc.Ref()
	}

	name := generateStringLiteralName(s, l.stringSeq)
	l.stringSeq++
	if ty == nil {
		ty = NewArrayType(len(s)+1, U16())
	}

	init := ConstStringTyped(name, s, ty)
	gc := &GlobalConst{Name: name, Ty: ty, Init: init, Linkage: PrivateLinkage}
	l.mod.Constants = append(l.mod.Constants, gc)
	_ = l.mod.SymTab.DefineCompat(name, gc.Ref())
	l.stringGlobals[s] = gc
	return gc.Ref()
}

// generateStringLiteralName keeps parity with backend naming pattern:
// _Lstr_<first4bytes_of_md5_hex>_<ordinal>
func generateStringLiteralName(s string, idx int) string {
	h := md5.Sum([]byte(s))
	prefix := fmt.Sprintf("%x", h[:4])
	return fmt.Sprintf("_Lstr_%s_%d", prefix, idx)
}
