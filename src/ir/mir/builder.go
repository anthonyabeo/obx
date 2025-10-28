package mir

import (
	"fmt"
	"strconv"
)

type Builder struct {
	Func  *Function // Current function being built
	Block *Block    // Current basic block

	TempGen    int // To generate fresh temps
	labelCount int // To generate new labels
}

func NewBuilder() *Builder {
	return &Builder{}
}

func (b *Builder) NewTemp(ty Type) *Temp {
	b.TempGen++
	name := fmt.Sprintf("t%d", b.TempGen)
	return &Temp{ID: name, BName: name, Typ: ty}
}

func (b *Builder) NewLabel(prefix string) string {
	b.labelCount++
	return prefix + "_" + strconv.Itoa(b.labelCount)
}

func (b *Builder) Emit(instr Instr) {
	b.Block.Instrs = append(b.Block.Instrs, instr)
}

func (b *Builder) SetTerm(term Instr) {
	b.Block.Term = term
}

func (b *Builder) BlockTermSet() bool {
	return b.Block.Term != nil
}

func (b *Builder) NewBlock(label string) *Block {
	blk := NewBlock(label)
	b.Func.Blocks[blk.ID] = blk

	return blk
}

func (b *Builder) SetBlock(block *Block) {
	b.Block = block
}

func (b *Builder) CreateBinary(op InstrOp, left, right Value, ty Type) Value {
	t := b.NewTemp(ty)
	left = b.ensureValue(left)
	right = b.ensureValue(right)

	if op.IsCmpCondCode() {
		b.Emit(&CmpInst{Target: t, Op: op, Left: left, Right: right})
	} else {
		b.Emit(&BinaryInst{Target: t, Op: op, Left: left, Right: right})
	}

	return t
}

func (b *Builder) CreateUnary(op InstrOp, operand Value, ty Type) Value {
	t := b.NewTemp(ty)
	b.Emit(&UnaryInst{
		Target:  t,
		Op:      op,
		Operand: operand,
	})

	return t
}

func (b *Builder) CreateReturn(value Value) {
	ret := &ReturnInst{Result: b.ensureValue(value)}
	b.SetTerm(ret)
	b.Emit(ret)
}

func (b *Builder) CreateAssign(target, value Value) {
	b.Emit(&MovInst{
		Target: target,
		Value:  value,
	})
}

func (b *Builder) CreateStore(target, value Value) {
	b.Emit(&StoreInst{
		Addr: target,
		Val:  b.ensureValue(value),
	})
}

func (b *Builder) CreateLoad(dst, src Value) {
	b.Emit(&LoadInst{
		Target: dst,
		Addr:   src,
	})
}
