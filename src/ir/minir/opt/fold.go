package miniropt

import (
	"math"

	"github.com/anthonyabeo/obx/src/ir/minir"
)

// ConstantFold evaluates instructions whose operands are all compile-time
// constants and replaces them with a single constant result.  The pass
// iterates until no further folding is possible.  Returns the total number of
// instructions rewritten across all iterations.
//
// Covered cases:
//   - BinaryInst  (add/sub/mul/div/mod/and/or/xor/shl/lshr/ashr) over IntegerConst or FloatConst
//   - UnaryInst   (neg/fneg/not) over a constant operand
//   - ICmpInst / FCmpInst over two constants of the same kind
//   - PhiInst     where every arm carries the same value (uniform phi)
//   - CastInst    where the source is a constant and CoerceConst succeeds
func ConstantFold(fn *minir.Function) int {
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

				if v, ok := foldInstruction(ins); ok {
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

// ── instruction-level fold dispatchers ────────────────────────────────────────

func foldInstruction(ins minir.Instr) (minir.Value, bool) {
	switch t := ins.(type) {
	case *minir.BinaryInst:
		return foldBinary(t)
	case *minir.UnaryInst:
		return foldUnary(t)
	case *minir.ICmpInst:
		return foldCmp(t.Pred, t.Left, t.Right)
	case *minir.FCmpInst:
		return foldCmp(t.Pred, t.Left, t.Right)
	case *minir.CastInst:
		if c, ok := t.Src.(minir.Constant); ok {
			if cv := minir.CoerceConst(c, t.Dst.Type()); cv != nil {
				return cv, true
			}
		}
	case *minir.PhiInst:
		return simplifyPhi(t)
	}
	return nil, false
}

func foldBinary(b *minir.BinaryInst) (minir.Value, bool) {
	if lc, ok := b.Left.(*minir.IntegerConst); ok {
		if rc, ok := b.Right.(*minir.IntegerConst); ok {
			return foldIntBinary(b, lc, rc)
		}
	}
	if lf, ok := b.Left.(*minir.FloatConst); ok {
		if rf, ok := b.Right.(*minir.FloatConst); ok {
			return foldFloatBinary(b, lf, rf)
		}
	}
	return nil, false
}

func foldUnary(u *minir.UnaryInst) (minir.Value, bool) {
	ty := valueType(u.Dst, u.Src)
	width := minir.IntBitWidth(ty)
	if width == 0 {
		width = 64
	}
	switch u.Op {
	case "neg":
		if c, ok := u.Src.(*minir.IntegerConst); ok {
			return constIntLike(ty, wrapToWidth(uint64(-signedIntValue(c)), width)), true
		}
	case "not":
		if c, ok := u.Src.(*minir.IntegerConst); ok {
			return constIntLike(ty, wrapToWidth(^unsignedIntValue(c), width)), true
		}
	case "fneg":
		if c, ok := u.Src.(*minir.FloatConst); ok {
			return constFloatLike(ty, -c.Value), true
		}
	}
	return nil, false
}

func foldCmp(pred string, left, right minir.Value) (minir.Value, bool) {
	switch l := left.(type) {
	case *minir.IntegerConst:
		r, ok := right.(*minir.IntegerConst)
		if !ok {
			return nil, false
		}
		return constBool(cmpInts(pred, l, r)), true
	case *minir.FloatConst:
		r, ok := right.(*minir.FloatConst)
		if !ok {
			return nil, false
		}
		return constBool(cmpFloats(pred, l.Value, r.Value)), true
	}
	return nil, false
}

func foldIntBinary(b *minir.BinaryInst, l, r *minir.IntegerConst) (minir.Value, bool) {
	ty := valueType(b.Dst, b.Left)
	if ty == nil {
		ty = valueType(b.Dst, b.Right)
	}
	width := minir.IntBitWidth(ty)
	if width == 0 {
		width = 64
	}

	switch b.Op {
	case "add":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)+unsignedIntValue(r), width)), true
	case "sub":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)-unsignedIntValue(r), width)), true
	case "mul":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)*unsignedIntValue(r), width)), true
	case "and":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)&unsignedIntValue(r), width)), true
	case "or":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)|unsignedIntValue(r), width)), true
	case "xor":
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)^unsignedIntValue(r), width)), true
	case "shl":
		shift := unsignedIntValue(r)
		if shift >= uint64(width) {
			return constIntLike(ty, 0), true
		}
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)<<shift, width)), true
	case "lshr":
		shift := unsignedIntValue(r)
		if shift >= uint64(width) {
			return constIntLike(ty, 0), true
		}
		return constIntLike(ty, wrapToWidth(unsignedIntValue(l)>>shift, width)), true
	case "ashr":
		shift := unsignedIntValue(r)
		if shift >= uint64(width) {
			shift = uint64(width - 1)
		}
		return constIntLike(ty, wrapToWidth(uint64(signedIntValue(l)>>shift), width)), true
	case "div":
		if unsignedIntValue(r) == 0 {
			return nil, false
		}
		if isUnsignedType(ty) {
			return constIntLike(ty, wrapToWidth(unsignedIntValue(l)/unsignedIntValue(r), width)), true
		}
		return constIntLike(ty, wrapToWidth(uint64(signedIntValue(l)/signedIntValue(r)), width)), true
	case "mod":
		if unsignedIntValue(r) == 0 {
			return nil, false
		}
		if isUnsignedType(ty) {
			return constIntLike(ty, wrapToWidth(unsignedIntValue(l)%unsignedIntValue(r), width)), true
		}
		return constIntLike(ty, wrapToWidth(uint64(signedIntValue(l)%signedIntValue(r)), width)), true
	}
	return nil, false
}

func foldFloatBinary(b *minir.BinaryInst, l, r *minir.FloatConst) (minir.Value, bool) {
	ty := valueType(b.Dst, b.Left)
	if ty == nil {
		ty = valueType(b.Dst, b.Right)
	}
	var v float64
	switch b.Op {
	case "add":
		v = l.Value + r.Value
	case "sub":
		v = l.Value - r.Value
	case "mul":
		v = l.Value * r.Value
	case "div":
		if r.Value == 0 {
			return nil, false
		}
		v = l.Value / r.Value
	default:
		return nil, false
	}
	return constFloatLike(ty, v), true
}

// ── comparison helpers ─────────────────────────────────────────────────────────

func cmpInts(pred string, l, r *minir.IntegerConst) bool {
	switch pred {
	case "eq":
		return unsignedIntValue(l) == unsignedIntValue(r)
	case "ne":
		return unsignedIntValue(l) != unsignedIntValue(r)
	case "slt", "ult":
		if pred == "ult" || isUnsignedType(l.Type()) {
			return unsignedIntValue(l) < unsignedIntValue(r)
		}
		return signedIntValue(l) < signedIntValue(r)
	case "sle", "ule":
		if pred == "ule" || isUnsignedType(l.Type()) {
			return unsignedIntValue(l) <= unsignedIntValue(r)
		}
		return signedIntValue(l) <= signedIntValue(r)
	case "sgt", "ugt":
		if pred == "ugt" || isUnsignedType(l.Type()) {
			return unsignedIntValue(l) > unsignedIntValue(r)
		}
		return signedIntValue(l) > signedIntValue(r)
	case "sge", "uge":
		if pred == "uge" || isUnsignedType(l.Type()) {
			return unsignedIntValue(l) >= unsignedIntValue(r)
		}
		return signedIntValue(l) >= signedIntValue(r)
	default:
		return false
	}
}

func cmpFloats(pred string, l, r float64) bool {
	if math.IsNaN(l) || math.IsNaN(r) {
		return pred == "ne"
	}
	switch pred {
	case "eq":
		return l == r
	case "ne":
		return l != r
	case "slt", "olt":
		return l < r
	case "sle", "ole":
		return l <= r
	case "sgt", "ogt":
		return l > r
	case "sge", "oge":
		return l >= r
	default:
		return false
	}
}

// simplifyPhi returns the single value shared by all phi arms when every arm
// carries an identical value, enabling the phi to be eliminated.
func simplifyPhi(phi *minir.PhiInst) (minir.Value, bool) {
	if len(phi.Args) == 0 {
		return nil, false
	}
	first := phi.Args[0].Val
	for _, arm := range phi.Args[1:] {
		if !sameValue(first, arm.Val) {
			return nil, false
		}
	}
	return first, true
}

