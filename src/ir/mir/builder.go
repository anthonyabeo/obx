package mir

import "strconv"

func (g *Generator) newLabel(prefix string) string {
	g.labelCount++
	return prefix + "_" + strconv.Itoa(g.labelCount)
}

func (g *Generator) newTemp(typ Type) Temp {
	g.tempCount++
	return Temp{ID: "_t" + strconv.Itoa(g.tempCount), Typ: typ}
}

func (g *Generator) emit(instr Instr) {
	g.currentBlk.Instrs = append(g.currentBlk.Instrs, instr)
}
