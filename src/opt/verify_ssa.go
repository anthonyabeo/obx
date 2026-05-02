package opt

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/obxir"
)

// KeyVerifySSA is the PassContext key under which VerifySSAPass stores its
// diagnostic slice.
const KeyVerifySSA = "verify.ssa.errors"

// VerifySSAPass checks SSA-specific invariants.  It is registered in every
// opt level; when fn.IsSSA is false it records a single informational error
// and returns immediately without scanning the body.
type VerifySSAPass struct{}

func (VerifySSAPass) Name() string { return "verifyssa" }

func (VerifySSAPass) Run(fn *obxir.Function, ctx *PassContext) *ChangeSet {
	cs := &ChangeSet{}
	errs := VerifySSA(fn)
	ctx.Put(KeyVerifySSA, errs)
	for _, e := range errs {
		cs.Notef("%s", e.Error())
	}
	cs.changed = false
	return cs
}

// VerifySSA performs a read-only SSA-correctness scan of fn, collecting all
// violations into the returned slice.  It never panics.
//
// Checks performed:
//  1. fn.IsSSA must be true.
//  2. Each Temp Ident is defined (appears as Def()) exactly once.
//  3. Each PhiInst has exactly len(b.Preds) arms, one per predecessor, with
//     no duplicate predecessor blocks.
//  4. (RPO use-before-def) Every *Temp use is preceded by its definition
//     when blocks are visited in reverse post-order.
func VerifySSA(fn *obxir.Function) []obxir.VerifyError {
	var errs []obxir.VerifyError

	add := func(block, instr, msg string) {
		errs = append(errs, obxir.VerifyError{
			Func:  fn.FnName,
			Block: block,
			Instr: instr,
			Msg:   msg,
		})
	}

	// ── 1. IsSSA guard ────────────────────────────────────────────────────

	if !fn.IsSSA {
		add("", "", "VerifySSA called on function where IsSSA=false; skipping SSA checks")
		return errs
	}

	// ── 2. Single-definition check ────────────────────────────────────────

	// defCount maps Temp.Ident → number of times it appears as Def().
	// defSite records the block+instr for the first definition for reporting.
	type defSite struct{ block, instr string }
	defCount := make(map[string]int)
	firstDef := make(map[string]defSite)

	for _, b := range fn.Blocks {
		for _, ins := range b.Instrs {
			d := ins.Def()
			if d == nil {
				continue
			}
			t, isTemp := d.(*obxir.Temp)
			if !isTemp {
				continue
			}
			defCount[t.Ident]++
			if defCount[t.Ident] == 1 {
				firstDef[t.Ident] = defSite{block: b.Label, instr: ins.String()}
			} else if defCount[t.Ident] == 2 {
				// Report on the second definition, referencing the first.
				first := firstDef[t.Ident]
				add(b.Label, ins.String(), fmt.Sprintf(
					"SSA violation: Temp %q defined more than once (first def at [%s] %q)",
					t.Ident, first.block, first.instr,
				))
			}
			// Suppress further errors for the same name beyond the second def.
		}
	}

	// ── 3. Phi arm count / uniqueness ─────────────────────────────────────

	for _, b := range fn.Blocks {
		for _, ins := range b.Instrs {
			phi, ok := ins.(*obxir.PhiInst)
			if !ok {
				break // phis are always first; any non-phi ends the phi region
			}

			wantArms := len(b.Preds)
			if len(phi.Args) != wantArms {
				add(b.Label, phi.String(), fmt.Sprintf(
					"PhiInst has %d arm(s) but block has %d predecessor(s)",
					len(phi.Args), wantArms,
				))
			}

			seen := make(map[int]bool) // block ID → already seen
			for _, arg := range phi.Args {
				if arg.Block == nil {
					continue // already caught by VerifyIR
				}
				if seen[arg.Block.ID] {
					add(b.Label, phi.String(), fmt.Sprintf(
						"PhiInst has duplicate arm for predecessor block %q",
						arg.Block.Label,
					))
				}
				seen[arg.Block.ID] = true
			}
		}
	}

	// ── 4. RPO use-before-def ────────────────────────────────────────────

	// Seed the defined set with params and locals.
	defined := make(map[string]bool)
	for _, p := range fn.Params {
		defined[p.Ident] = true
	}
	for _, l := range fn.Locals {
		defined[l.Ident] = true
	}

	rpo := fn.ReversePostOrder()
	for _, id := range rpo {
		b, ok := fn.Blocks[id]
		if !ok {
			continue
		}

		for _, ins := range b.Instrs {
			is := ins.String()

			// Check uses first (before recording the def).
			// Skip phi uses — phi operands come from predecessor blocks
			// and are legitimately "used before defined" in RPO order.
			if _, isPhi := ins.(*obxir.PhiInst); !isPhi {
				for _, u := range ins.Uses() {
					t, isTemp := u.(*obxir.Temp)
					if !isTemp {
						continue
					}
					if !defined[t.Ident] {
						add(b.Label, is, fmt.Sprintf(
							"use-before-def: Temp %q used before its definition (RPO order)",
							t.Ident,
						))
					}
				}
			}

			// Record the definition.
			if d := ins.Def(); d != nil {
				if t, isTemp := d.(*obxir.Temp); isTemp {
					defined[t.Ident] = true
				}
			}
		}
	}

	return errs
}

