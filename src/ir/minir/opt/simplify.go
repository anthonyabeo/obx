package miniropt

import "github.com/anthonyabeo/obx/src/ir/minir"

// AlgebraicSimplify rewrites instructions using algebraic identities and
// annihilator rules — no operand needs to be a constant on both sides.
// The pass iterates until no further simplification is possible.  Returns the
// total number of instructions rewritten across all iterations.
//
// Rules applied to BinaryInst:
//
//	add:  x+0  → x,  0+x  → x
//	sub:  x-0  → x,  x-x  → 0
//	mul:  x*0  → 0,  0*x  → 0,  x*1  → x,  1*x  → x
//	div:  x/1  → x
//	mod:  x%1  → 0
//	and:  x&0  → 0,  0&x  → 0,  x&~0 → x,  ~0&x → x,  x&x  → x
//	or:   x|0  → x,  0|x  → x,  x|x  → x
//	xor:  x^0  → x,  0^x  → x,  x^x  → 0
//	shl/lshr/ashr: x>>0 or x<<0 → x
//
// Additional identity over ICmpInst:
//
//	x == x  → true,  x != x  → false  (and analogous ordered comparisons)
//
// CastInst where source and destination types are equal is replaced by the
// source operand directly.
//
// Uniform PhiInst (all arms carry the same value) is eliminated.
func AlgebraicSimplify(fn *minir.Function) int {
	if fn == nil || fn.Entry == nil {
		return 0
	}

	total := 0
	for {
		keepTemp := tempOnlyUses(fn)
		subst := make(map[*minir.Temp]minir.Value)
		toDelete := make(map[minir.Instr]bool)
		iterChanges := 0

		for _, id := range fn.ReversePostOrder() {
			blk := fn.Blocks[id]
			for i, raw := range blk.Instrs {
				if toDelete[raw] {
					continue
				}

				ins := replaceInInstr(raw, subst)
				if ins != raw {
					blk.Instrs[i] = ins
					iterChanges++
				}

				def := ins.Def()
				if def == nil {
					continue
				}

				if v, ok := simplifyInstruction(ins); ok {
					if applyScalarValue(blk, i, ins, def, v, keepTemp[def], subst, toDelete) {
						iterChanges++
					}
				}
			}
		}

		if iterChanges == 0 {
			break
		}

		for _, blk := range fn.Blocks {
			rebuilt := blk.Instrs[:0]
			for _, ins := range blk.Instrs {
				if !toDelete[ins] {
					rebuilt = append(rebuilt, ins)
				}
			}
			blk.Instrs = rebuilt
			if n := len(rebuilt); n > 0 {
				if t, ok := rebuilt[n-1].(minir.Terminator); ok {
					blk.Term = t
				}
			}
		}

		total += iterChanges
	}

	return total
}

// ── instruction-level simplification dispatcher ───────────────────────────────

func simplifyInstruction(ins minir.Instr) (minir.Value, bool) {
	switch t := ins.(type) {
	case *minir.BinaryInst:
		switch t.Op {
		case "add":
			if isZeroValue(t.Left) {
				return t.Right, true
			}
			if isZeroValue(t.Right) {
				return t.Left, true
			}
		case "sub":
			if isZeroValue(t.Right) {
				return t.Left, true
			}
			if sameValue(t.Left, t.Right) {
				return zeroValueForType(valueType(t.Dst, t.Left)), true
			}
		case "mul":
			if isZeroValue(t.Left) || isZeroValue(t.Right) {
				return zeroValueForType(valueType(t.Dst, t.Left)), true
			}
			if isOneValue(t.Left) {
				return t.Right, true
			}
			if isOneValue(t.Right) {
				return t.Left, true
			}
		case "div":
			if isOneValue(t.Right) {
				return t.Left, true
			}
		case "mod":
			if isOneValue(t.Right) {
				return zeroValueForType(valueType(t.Dst, t.Left)), true
			}
		case "and":
			if isZeroValue(t.Left) || isZeroValue(t.Right) {
				return zeroValueForType(valueType(t.Dst, t.Left)), true
			}
			if isAllOnesValue(t.Left) {
				return t.Right, true
			}
			if isAllOnesValue(t.Right) {
				return t.Left, true
			}
			if sameValue(t.Left, t.Right) {
				return t.Left, true
			}
		case "or":
			if isZeroValue(t.Left) {
				return t.Right, true
			}
			if isZeroValue(t.Right) {
				return t.Left, true
			}
			if sameValue(t.Left, t.Right) {
				return t.Left, true
			}
		case "xor":
			if isZeroValue(t.Left) {
				return t.Right, true
			}
			if isZeroValue(t.Right) {
				return t.Left, true
			}
			if sameValue(t.Left, t.Right) {
				return zeroValueForType(valueType(t.Dst, t.Left)), true
			}
		case "shl", "lshr", "ashr":
			if isZeroValue(t.Right) {
				return t.Left, true
			}
		}
	case *minir.ICmpInst:
		if sameValue(t.Left, t.Right) {
			switch t.Pred {
			case "eq", "sle", "sge", "ule", "uge":
				return constBool(true), true
			case "ne", "slt", "sgt", "ult", "ugt":
				return constBool(false), true
			}
		}
	case *minir.CastInst:
		if t.Src != nil && t.Dst != nil &&
			t.Src.Type() != nil && t.Dst.Type() != nil &&
			t.Src.Type().Equal(t.Dst.Type()) {
			return t.Src, true
		}
	case *minir.PhiInst:
		return simplifyPhi(t)
	}
	return nil, false
}

