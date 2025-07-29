package mir

import "strconv"

func (g *Generator) newLabel(prefix string) string {
	g.labelCount++
	return prefix + "_" + strconv.Itoa(g.labelCount)
}

func (g *Generator) newTemp(typ Type) Temp {
	g.tempCount++
	return Temp{Name: "_t" + strconv.Itoa(g.tempCount), Typ: typ}
}

func (g *Generator) emit(instr Inst) {
	g.currentBlk.Inst = append(g.currentBlk.Inst, instr)
}
