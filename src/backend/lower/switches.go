package lower

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// LowerSwitchesInProgram rewrites every *mir.SwitchInstr terminator in prog
// into one of two target-neutral forms before instruction selection:
//
//   - Compare-chain: for sparse integer switches the block is split into a
//     chain of blocks each containing a single cmp.eq instruction followed by
//     a CondBrInstr{TrueLabel: caseLabel, FalseLabel: nextCheck}. The last
//     check falls through to the switch default.
//
//   - Jump-table: for dense integer switches (density ≥ ABI.JumpTableMinDensity
//     with ≥ 2 cases) the terminator is replaced with
//     MachineTerm{Op:"switch_table", Srcs:[key], Targets:[entry0…entryN,default]}
//     where entryI is the block label for value low+i (gaps filled with default).
//     The emitter handles this terminator ad-hoc.
//
// After this pass, no *mir.SwitchInstr terminators remain in the program.
func LowerSwitchesInProgram(prog *mir.Program, tgt target.Target) (*mir.Program, error) {
	if prog == nil {
		return mir.NewProgram(), nil
	}
	if tgt == nil {
		return nil, fmt.Errorf("lower switches: nil target")
	}
	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			if err := LowerSwitchesInFunction(fn, tgt); err != nil {
				return nil, fmt.Errorf("lower switches in %s: %w", fn.Name, err)
			}
		}
	}
	return prog, nil
}

// LowerSwitchesInFunction rewrites switch terminators inside fn in-place.
// New blocks created by compare-chain splitting are added to fn.
func LowerSwitchesInFunction(fn *mir.Function, tgt target.Target) error {
	if fn == nil {
		return fmt.Errorf("lower switches: nil function")
	}
	if tgt == nil {
		return fmt.Errorf("lower switches: nil target")
	}
	abi := tgt.ABIInfo()

	// Collect switch blocks first to avoid mutating the slice we iterate.
	var switchBlocks []*mir.Block
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		if _, ok := block.Term.(*mir.SwitchInstr); ok {
			switchBlocks = append(switchBlocks, block)
		}
	}

	nextID := maxFnBlockID(fn) + 1
	for _, block := range switchBlocks {
		sw := block.Term.(*mir.SwitchInstr)
		plan, err := target.BuildSwitchPlan(sw, abi)
		if err != nil {
			return fmt.Errorf("block %s: %w", block.Label, err)
		}

		newBlocks, err := lowerSwitchBlock(block, sw, plan, &nextID)
		if err != nil {
			return fmt.Errorf("block %s: %w", block.Label, err)
		}
		for _, nb := range newBlocks {
			fn.AddBlock(nb)
		}
	}
	return nil
}

// lowerSwitchBlock dispatches to lowerJumpTable or lowerCompareChain based on
// the plan and returns any newly created blocks.
func lowerSwitchBlock(block *mir.Block, sw *mir.SwitchInstr, plan *target.SwitchPlan, nextID *int) ([]*mir.Block, error) {
	if plan.JumpTable != nil {
		return lowerJumpTable(block, sw, plan)
	}
	return lowerCompareChain(block, sw, plan, nextID)
}

// lowerJumpTable replaces the switch terminator with:
//
//	MachineTerm{Op:"switch_table", Srcs:[key], Targets:[entry0…entryN, default]}
//
// Targets[0..N-1] are the jump-table entries (low+i → label; gaps → default).
// Targets[N] is the default label, used for out-of-range keys.
func lowerJumpTable(block *mir.Block, sw *mir.SwitchInstr, plan *target.SwitchPlan) ([]*mir.Block, error) {
	jt := plan.JumpTable
	targets := make([]string, 0, len(jt.Entries)+1)
	targets = append(targets, jt.Entries...)
	targets = append(targets, jt.Default)
	block.Term = mir.NewMachineTerm("switch_table", []mir.Operand{sw.Value}, targets)
	return nil, nil
}

// lowerCompareChain splits the compare-chain over N cases into N blocks:
//
//	original block:   cmp.eq key, case[0]  →  CondBr(case[0].label, check_1)
//	__sw_check_1:     cmp.eq key, case[1]  →  CondBr(case[1].label, check_2)
//	…
//	__sw_check_N-1:   cmp.eq key, case[N-1] → CondBr(case[N-1].label, default)
//
// If there are no cases the terminator is replaced with JumpInstr{default}.
func lowerCompareChain(block *mir.Block, sw *mir.SwitchInstr, plan *target.SwitchPlan, nextID *int) ([]*mir.Block, error) {
	cases := plan.CompareChain

	if len(cases) == 0 {
		block.Term = &mir.JumpInstr{Target: plan.Default}
		return nil, nil
	}

	// Derive the type of the key for immediate operands.
	keyTy := sw.Value.Type()
	if keyTy == nil {
		keyTy = mir.NewScalarType("i64", 8)
	}
	i1Ty := mir.NewScalarType("i1", 1)

	// Pre-create one intermediate check block for each case after the first.
	checkBlocks := make([]*mir.Block, 0, len(cases)-1)
	for i := 1; i < len(cases); i++ {
		label := fmt.Sprintf("%s.__sw_check_%d", block.Label, i)
		nb := mir.NewBlock(*nextID, label)
		*nextID++
		checkBlocks = append(checkBlocks, nb)
	}

	for i, c := range cases {
		// Block that will hold this comparison.
		var cur *mir.Block
		if i == 0 {
			cur = block
		} else {
			cur = checkBlocks[i-1]
		}

		// Where to go when this case does not match.
		var falseLabel string
		if i == len(cases)-1 {
			falseLabel = plan.Default
		} else {
			falseLabel = checkBlocks[i].Label
		}

		// emit: cmp.eq __sw_cmpI, key, caseVal
		cmpDst := mir.NewRegister(
			fmt.Sprintf("__sw_cmp_%s_%d", block.Label, i),
			mir.VirtualReg,
			i1Ty,
		)
		caseImm := mir.NewImmediate(c.Value, keyTy)
		cmp := &mir.CompareInstr{Dst: cmpDst, Pred: "eq", Left: sw.Value, Right: caseImm}

		if i == 0 {
			cur.Instrs = append(cur.Instrs, cmp)
		} else {
			cur.Instrs = []mir.Instr{cmp}
		}
		cur.Term = &mir.CondBrInstr{
			Cond:       cmpDst,
			TrueLabel:  c.Label,
			FalseLabel: falseLabel,
		}
	}

	return checkBlocks, nil
}

// maxFnBlockID returns the highest block ID currently in fn, or -1 if fn is
// empty.
func maxFnBlockID(fn *mir.Function) int {
	max := -1
	for _, b := range fn.Blocks {
		if b != nil && b.ID > max {
			max = b.ID
		}
	}
	return max
}
