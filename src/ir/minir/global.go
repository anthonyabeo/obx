package minir

import "fmt"

// Linkage describes the visibility of a global symbol across compilation units,
// mirroring LLVM's linkage model.
type Linkage int

const (
	// ExternalLinkage — symbol is visible outside the module (exported).
	ExternalLinkage Linkage = iota
	// InternalLinkage — symbol is local to the module but retains a name in
	// the object file (analogous to C static linkage).
	InternalLinkage
	// PrivateLinkage — symbol is local; the linker strips the name entirely.
	// Used for compiler-synthesised globals (string literals, init helpers, …).
	PrivateLinkage
)

func (lk Linkage) String() string {
	switch lk {
	case ExternalLinkage:
		return "external"
	case InternalLinkage:
		return "internal"
	case PrivateLinkage:
		return "private"
	default:
		return "unknown"
	}
}

// ── GlobalRef ─────────────────────────────────────────────────────────────────

// GlobalRef is the address of a module-scope symbol, analogous to "@name" in
// LLVM textual IR.  It implements Value and can be used directly as the Addr
// operand of LoadInst / StoreInst without first materializing it into a *Temp.
type GlobalRef struct {
	GlobalName string
	Ty         *PointerType // pointer to the global's element type
}

func (g *GlobalRef) Type() Type     { return g.Ty }
func (g *GlobalRef) String() string { return "@" + g.GlobalName }
func (g *GlobalRef) IsConst() bool  { return false }

// IsMem reports that a GlobalRef is an address operand, satisfying the
// isAddrValue check used by the verifier and memory instructions.
func (g *GlobalRef) IsMem() bool { return true }

// ── GlobalVar ─────────────────────────────────────────────────────────────────

// GlobalVar is a mutable, module-scope variable.
// Analogous to "@name = [linkage] global <type> <init>" in LLVM IR.
type GlobalVar struct {
	Name    string
	Ty      Type
	Init    *Constant // optional literal initialiser; nil → zero-initialised
	Linkage Linkage
	ref     *GlobalRef // lazily created, stable address value
}

// Ref returns the stable *GlobalRef representing this variable's address.
// Successive calls return the same pointer.
func (g *GlobalVar) Ref() *GlobalRef {
	if g.ref == nil {
		g.ref = &GlobalRef{GlobalName: g.Name, Ty: Ptr(g.Ty)}
	}
	return g.ref
}

// ── GlobalConst ───────────────────────────────────────────────────────────────

// GlobalConst is a read-only, module-scope constant.
// Analogous to "@name = [linkage] constant <type> <init>" in LLVM IR.
type GlobalConst struct {
	Name    string
	Ty      Type
	Init    *Constant // required; must be a literal constant value
	Linkage Linkage
	ref     *GlobalRef // lazily created, stable address value
}

// Ref returns the stable *GlobalRef representing this constant's address.
func (g *GlobalConst) Ref() *GlobalRef {
	if g.ref == nil {
		g.ref = &GlobalRef{GlobalName: g.Name, Ty: Ptr(g.Ty)}
	}
	return g.ref
}

// ── ExternalFunc ──────────────────────────────────────────────────────────────

// ExternalFunc is a declaration-only function — no body, only a signature.
// Analogous to "declare @name(...) -> T" in LLVM IR.
type ExternalFunc struct {
	Name    string
	Sig     *FunctionType
	Linkage Linkage
}

func (e *ExternalFunc) String() string {
	if e.Sig == nil {
		return fmt.Sprintf("declare @%s", e.Name)
	}
	params := make([]string, len(e.Sig.Params))
	for i, p := range e.Sig.Params {
		if p == nil {
			params[i] = "<nil>"
		} else {
			params[i] = p.String()
		}
	}
	res := "void"
	if e.Sig.Result != nil {
		res = e.Sig.Result.String()
	}
	return fmt.Sprintf("declare @%s(%s) -> %s", e.Name, joinStrings(params, ", "), res)
}

// joinStrings concatenates elems with sep (avoids importing strings in global.go).
func joinStrings(elems []string, sep string) string {
	if len(elems) == 0 {
		return ""
	}
	out := elems[0]
	for _, e := range elems[1:] {
		out += sep + e
	}
	return out
}
