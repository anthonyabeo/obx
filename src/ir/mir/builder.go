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

func (b *Builder) NewTemp() Temp {
	b.TempGen++
	return Temp{ID: fmt.Sprintf("t%d", b.TempGen)}
}

func (b *Builder) NewLabel(prefix string) string {
	b.labelCount++
	return prefix + "." + strconv.Itoa(b.labelCount)
}

func (b *Builder) Emit(instr Instr) {
	b.Block.Instrs = append(b.Block.Instrs, instr)
}

func (b *Builder) SetTerm(term Instr) {
	b.Block.Term = term
}

func (b *Builder) NewBlock(label string) *Block {
	blk := NewBlock(label)
	b.Func.Blocks[blk.ID] = blk

	return blk
}

func (b *Builder) SetBlock(block *Block) {
	b.Block = block
}

func (b *Builder) CreateBinary(op InstrOp, left, right Value) Value {
	t := b.NewTemp()

	if op.IsCmpCondCode() {
		b.Emit(&CmpInst{Target: t, Op: op, Left: left, Right: right})
	} else {
		b.Emit(&BinaryInst{
			Target: t,
			Op:     op,
			Left:   left,
			Right:  right,
		})
	}

	return t
}

func (b *Builder) CreateUnary(op InstrOp, operand Value) Value {
	t := b.NewTemp()
	b.Emit(&UnaryInst{
		Target:  t,
		Op:      op,
		Operand: operand,
	})

	return t
}

func (b *Builder) CreateCallInst(callee string, args []Value) Value {
	t := b.NewTemp()
	b.Emit(&CallInst{
		Target: t,
		Callee: callee,
		Args:   args,
	})

	return t
}
