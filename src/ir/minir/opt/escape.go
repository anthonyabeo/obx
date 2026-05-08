package miniropt

import "github.com/anthonyabeo/obx/src/ir/minir"

// ── Escape analysis ───────────────────────────────────────────────────────────
//
// An alloca is "promotable" to a scalar SSA value if and only if:
//   - Its allocated type is scalar (PrimitiveType or PointerType),
//   - Its address temp never reaches a "dangerous" use site, and
//   - Its address temp is read by at least one LoadInst.
//     Write-only allocas (stores but no loads) are intentionally left
//     untouched by Mem2Reg; they belong to a dedicated dead-store-elimination
//     pass, not to SSA promotion.
//
// Dangerous use sites are:
//   - GEPInst.Base      — pointer arithmetic implies aggregate access.
//   - CallInst.Args     — the callee may store the address.
//   - StoreInst.Val     — the address itself is being stored (escapes to memory).
//   - PhiInst arm value — the address flows through a φ; conservatively treated
//                         as an escape because the downstream use is untracked.
//
// TODO(sroa): RecordType and ArrayType allocas are intentionally excluded from
// promotion.  A future SROA (Scalar Replacement of Aggregates) pass should
// split them into per-field/per-element scalar allocas first, after which
// mem2reg can promote those scalar pieces individually.

// isScalarType reports whether ty can be held directly in a register
// (i.e., it fits in one machine word and is not an aggregate).
func isScalarType(ty minir.Type) bool {
	switch ty.(type) {
	case *minir.PrimitiveType, *minir.PointerType:
		return true
	}
	return false
}

// escapes reports whether the address temp dst (an alloca's Dst) is ever used
// in a position other than as the Addr of a LoadInst or StoreInst.
// If it is, the alloca cannot be safely promoted.
func escapes(fn *minir.Function, dst *minir.Temp) bool {
	for _, b := range fn.Blocks {
		for _, instr := range b.Instrs {
			switch ins := instr.(type) {
			case *minir.GEPInst:
				// Address arithmetic: can produce a derived pointer to a sub-element.
				if ins.Base == dst {
					return true
				}
			case *minir.CallInst:
				// Any argument position: the callee is allowed to retain the pointer.
				for _, arg := range ins.Args {
					if arg == dst {
						return true
					}
				}
			case *minir.StoreInst:
				// The address is the *value* being stored (not the store destination).
				if ins.Val == dst {
					return true
				}
			case *minir.PhiInst:
				// The address flows through a φ-node.
				for _, arm := range ins.Args {
					if arm.Val == dst {
						return true
					}
				}
			}
		}
	}
	return false
}

// hasLoad reports whether the address temp dst (an alloca's Dst) is used as
// the Addr operand of at least one LoadInst anywhere in fn.  An alloca with
// no loads is write-only: promoting it here would silently erase observable
// assignments, which is dead-store elimination, not Mem2Reg's responsibility.
func hasLoad(fn *minir.Function, dst *minir.Temp) bool {
	for _, b := range fn.Blocks {
		for _, instr := range b.Instrs {
			if ld, ok := instr.(*minir.LoadInst); ok {
				if ld.Addr == dst {
					return true
				}
			}
		}
	}
	return false
}

// collectPromotable returns all AllocaInst nodes in fn whose addresses are
// non-escaping, scalar-typed, and read by at least one LoadInst.
func collectPromotable(fn *minir.Function) []*minir.AllocaInst {
	var out []*minir.AllocaInst
	for _, b := range fn.Blocks {
		for _, instr := range b.Instrs {
			a, ok := instr.(*minir.AllocaInst)
			if !ok {
				continue
			}
			if !isScalarType(a.AllocType) {
				continue // aggregate — defer to SROA
			}
			if escapes(fn, a.Dst) {
				continue
			}
			if !hasLoad(fn, a.Dst) {
				continue // write-only alloca — leave for a dedicated DSE pass
			}
			out = append(out, a)
		}
	}
	return out
}
