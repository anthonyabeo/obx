package opt

import "github.com/anthonyabeo/obx/src/ir/mir"

type ConstantFold struct{}

func (c ConstantFold) Name() string { return "ConstantFold" }

func (c ConstantFold) Run(fn *mir.Function, ctx *PassContext) *ChangeSet {
	//cs := &ChangeSet{}

	for _, id := range fn.DFSOrder() {
		blk := fn.Blocks[id]
		for i, instr := range blk.Instrs {
			if foldable, ok := instr.(mir.Foldable); ok && foldable.CanFold() {
				foldedValue := foldable.Fold()
				asn := &mir.MoveInst{Target: instr.Def(), Value: foldedValue}
				ctx.cs.Notef("folded %s -> %s", instr.String(), asn.String())
				blk.Instrs[i] = asn
			}
		}

	}

	return ctx.cs
}
