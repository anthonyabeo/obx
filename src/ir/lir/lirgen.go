package lir

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/mir"
)

type Generator struct {
}

func NewGenerator() *Generator {
	return &Generator{}
}

//func (g *Generator) genModule(m *mir.Module) *Module {
//	module := &Module{Name: m.Name}
//
//	for _, decl := range m.Decl {
//		switch d := decl.(type) {
//		case *mir.Function:
//			module.Func = append(module.Func, g.genFunction(d))
//		}
//	}
//
//	if m.Init != nil {
//		module.Func = append(module.Func, g.genFunction(m.Init))
//	}
//
//	return module
//}

func (g *Generator) genFunction(fxn *mir.Function) *Function {
	fn := &Function{Name: fxn.Name, Ret: fxn.Result}

	for _, block := range fxn.Blocks {
		fn.Instructions = append(fn.Instructions, g.genBlock(block)...)
	}

	return fn
}

func (g *Generator) genBlock(blk *mir.Block) []Inst {
	ixns := make([]Inst, 0)

	ixns = append(ixns, &Label{Kind: "INST", Name: blk.Label})
	for _, inst := range blk.Inst {
		ixns = append(ixns, g.genInstr(inst))
	}

	return ixns
}

func (g *Generator) genInstr(x mir.Inst) Inst {
	switch x := x.(type) {
	case *mir.AssignInst:
		dst := g.genOperand(x.Target)
		src := g.genOperand(x.Value)
		return &MoveInst{
			OpCode: Move,
			Src:    src,
			Dst:    dst,
		}
	case *mir.JumpInst:
		return &JmpInst{
			OpCode: Jmp,
			Dst:    &Label{Name: x.Target},
		}
	case *mir.CondBrInst:
		return &CondBrInst{
			OpCode:  Br,
			Cond:    g.genOperand(x.Cond),
			IfFalse: &Label{Name: x.FalseLabel},
			IfTrue:  &Label{Name: x.TrueLabel},
		}
	case *mir.ReturnInst:
		var result Operand
		if x.Result != nil {
			result = g.genOperand(x.Result)
		}

		return &RetInst{
			OpCode: Ret,
			Value:  result,
		}
	default:
		panic(fmt.Sprintf("unknown MIR Instruction: '%v'", x))
	}
}

func (g *Generator) genOperand(x mir.Operand) Operand {
	switch x := x.(type) {
	case mir.IntegerConst:
		return IntegerConst{
			Value:  x.Value,
			Signed: x.Signed,
			Bits:   x.Bits,
			Typ:    x.Typ,
		}
	case mir.FloatConst:
		return RealConst{
			Value: x.Value,
			Bits:  x.Bits,
			Typ:   x.Typ,
		}
	case mir.Temp:
		return &Register{
			Name: x.Name,
			Type: x.Typ,
		}
	default:
		panic(fmt.Sprintf("unknown MIR operand: '%v'", x))
	}
}
