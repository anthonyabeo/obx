package minir

import (
	"github.com/anthonyabeo/obx/src/ir/desugar"
)

// Module-scope lowering helpers live here so lower.go can stay focused on
// function bodies and statement/expression lowering.

// lowerGlobalVar lowers a module-scope variable declaration into a *GlobalVar
// and registers its *GlobalRef address in the module SymTab.
func (l *Lowerer) lowerGlobalVar(d *desugar.Variable) {
	vt := LowerType(d.Type)
	if vt == nil {
		vt = I32()
	}

	lk := InternalLinkage
	if d.IsExport {
		lk = ExternalLinkage
	}

	name := d.Name
	if d.Mangled != "" {
		name = d.Mangled
	}

	gv := &GlobalVar{Name: name, Ty: vt, Linkage: lk}
	l.mod.Globals = append(l.mod.Globals, gv)
	if err := l.mod.SymTab.Define(name, gv.Ref()); err != nil {
		l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
	}

	// Also, register the bare (unmangled) name when it differs.
	if d.Mangled != "" && d.Mangled != d.Name {
		if err := l.mod.SymTab.Define(d.Name, gv.Ref()); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	}
}

// lowerGlobalConst lowers a module-scope constant declaration into a
// *GlobalConst and registers the immediate *Constant value (not the Ref)
// in the module SymTab so that ConstantRef resolution inside functions returns
// the value directly without a load.
func (l *Lowerer) lowerGlobalConst(d *desugar.Constant) {
	vt := LowerType(d.Type)
	if vt == nil {
		vt = I32()
	}

	lk := InternalLinkage
	if d.IsExport {
		lk = ExternalLinkage
	}

	name := d.Name
	if d.Mangled != "" {
		name = d.Mangled
	}

	init := lowerConstant(d.Value)
	gc := &GlobalConst{Name: name, Ty: vt, Init: init, Linkage: lk}
	l.mod.Constants = append(l.mod.Constants, gc)

	if init != nil {
		if err := l.mod.SymTab.Define(name, init); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	} else {
		if err := l.mod.SymTab.Define(name, gc.Ref()); err != nil {
			l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
		}
	}

	if d.Mangled != "" && d.Mangled != d.Name {
		if init != nil {
			if err := l.mod.SymTab.Define(d.Name, init); err != nil {
				l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
			}
		} else {
			if err := l.mod.SymTab.Define(d.Name, gc.Ref()); err != nil {
				l.reportLoweringDiagnostic(err.Error(), d.StartOffset, d.EndOffset)
			}
		}
	}
}

// lowerExternalFunc lowers an FFI / external function declaration into an
// *ExternalFunc (signature only).
func (l *Lowerer) lowerExternalFunc(d *desugar.Function) {
	var params []Type
	for _, p := range d.Params {
		pt := LowerType(p.Typ)
		if pt == nil {
			pt = I32()
		}
		if p.Kind == desugar.VarParam || p.Kind == desugar.InParam {
			pt = Ptr(pt)
		}
		params = append(params, pt)
	}

	sig := &FunctionType{Params: params, Result: LowerType(d.Result)}
	cName := d.Mangled
	if cName == "" {
		cName = d.Name
	}

	ef := &ExternalFunc{
		Name:    d.FnName(),
		Sig:     sig,
		Linkage: ExternalLinkage,
		Attrs: &ExternalAttrs{
			CName:    cName,
			DLLName:  d.DLLName,
			Variadic: d.IsVarArgs,
			CallConv: "",
		},
	}

	l.mod.Externals = append(l.mod.Externals, ef)
}
