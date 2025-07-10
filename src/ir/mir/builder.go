package mir

import "strconv"

func (g *Generator) newLabel(prefix string) string {
	g.labelCount++
	return prefix + "_" + strconv.Itoa(g.labelCount)
}

func (g *Generator) newTemp() string {
	g.tempCount++
	return "_t" + strconv.Itoa(g.tempCount)
}

func (g *Generator) emit(instr Instr) {
	g.currentBlk.Instrs = append(g.currentBlk.Instrs, instr)
}
