package miniropt

import "github.com/anthonyabeo/obx/src/ir/minir"

// StrengthReduce replaces expensive arithmetic operations with cheaper
// equivalents when the operands allow it.  The pass iterates until no further
// reduction is possible.  Returns the total number of instructions rewritten
// across all iterations.
//
// Rules applied to BinaryInst:
//
//	mul  x, 2^n   →  shl  x, n        (any integer type; excludes n==0)
//	2^n  mul  x   →  shl  x, n        (commutative form)
//	div  x, 2^n   →  lshr x, n        (unsigned types only; excludes n==0)
//	mod  x, 2^n   →  and  x, (2^n)-1  (unsigned types only; excludes n==0)
//
// Signed division/mod by a power-of-two is NOT reduced here because arithmetic
// right-shift gives the wrong answer for negative dividends; a dedicated signed
// idiom recognizer should handle that case separately.
func StrengthReduce(fn *minir.Function) int {
	if fn == nil || fn.Entry == nil {
		return 0
	}

	total := 0
	for {
		iterChanges := 0

		for _, id := range fn.ReversePostOrder() {
			blk := fn.Blocks[id]
			for i, raw := range blk.Instrs {
				repl, ok := strengthReduceInstruction(raw)
				if ok && !sameInstr(raw, repl) {
					blk.Instrs[i] = repl
					iterChanges++
				}
			}
		}

		if iterChanges == 0 {
			break
		}
		total += iterChanges
	}

	return total
}

// ── instruction-level strength-reduction dispatcher ───────────────────────────

func strengthReduceInstruction(ins minir.Instr) (minir.Instr, bool) {
	b, ok := ins.(*minir.BinaryInst)
	if !ok {
		return nil, false
	}

	switch b.Op {
	case "mul":
		// 2^n * x  →  shl x, n
		if exp, ok := pow2Const(b.Left); ok && exp > 0 {
			return &minir.BinaryInst{
				Dst:   b.Dst,
				Op:    "shl",
				Left:  b.Right,
				Right: shiftConst(exp, valueType(b.Dst, b.Right)),
			}, true
		}
		// x * 2^n  →  shl x, n
		if exp, ok := pow2Const(b.Right); ok && exp > 0 {
			return &minir.BinaryInst{
				Dst:   b.Dst,
				Op:    "shl",
				Left:  b.Left,
				Right: shiftConst(exp, valueType(b.Dst, b.Left)),
			}, true
		}
	case "div":
		// x / 2^n  →  lshr x, n  (unsigned only)
		if exp, ok := pow2Const(b.Right); ok && exp > 0 && isUnsignedType(valueType(b.Dst, b.Left)) {
			return &minir.BinaryInst{
				Dst:   b.Dst,
				Op:    "lshr",
				Left:  b.Left,
				Right: shiftConst(exp, valueType(b.Dst, b.Left)),
			}, true
		}
	case "mod":
		// x % 2^n  →  and x, (2^n)-1  (unsigned only)
		if exp, ok := pow2Const(b.Right); ok && exp > 0 && isUnsignedType(valueType(b.Dst, b.Left)) {
			mask := constIntLike(valueType(b.Dst, b.Left), (uint64(1)<<exp)-1)
			return &minir.BinaryInst{
				Dst:   b.Dst,
				Op:    "and",
				Left:  b.Left,
				Right: mask,
			}, true
		}
	}

	return nil, false
}

// ── helpers ───────────────────────────────────────────────────────────────────

// pow2Const returns (exp, true) when v is an IntegerConst that equals exactly
// 2^exp for some exp >= 0.  Returns (0, false) for zero, non-powers-of-two,
// and non-integer values.
func pow2Const(v minir.Value) (uint, bool) {
	c, ok := v.(*minir.IntegerConst)
	if !ok {
		return 0, false
	}
	x := unsignedIntValue(c)
	if x == 0 || x&(x-1) != 0 {
		return 0, false
	}
	var exp uint
	for x > 1 {
		x >>= 1
		exp++
	}
	return exp, true
}

// shiftConst builds a constant integer with value exp, typed like ty.
func shiftConst(exp uint, ty minir.Type) minir.Value {
	return constIntLike(ty, uint64(exp))
}
