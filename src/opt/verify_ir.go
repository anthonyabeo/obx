package opt

import "github.com/anthonyabeo/obx/src/ir/obxir"

// VerifyIRPass is a read-only pass that checks structural and type-consistency
// invariants on the IR.  It always runs (included in every opt level).
// Errors are stored in PassContext under KeyVerifyIR.
const KeyVerifyIR = "verify.ir.errors"

type VerifyIRPass struct{}

func (VerifyIRPass) Name() string { return "verifyir" }

func (VerifyIRPass) Run(fn *obxir.Function, ctx *PassContext) *ChangeSet {
	cs := &ChangeSet{}
	errs := obxir.VerifyIR(fn)
	ctx.Put(KeyVerifyIR, errs)
	for _, e := range errs {
		cs.Notef("%s", e.Error())
	}
	// Verification never modifies the IR; reset changed so the pass manager
	// does not treat diagnostics as transformations.
	cs.changed = false
	return cs
}

