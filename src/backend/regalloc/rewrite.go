package regalloc

import (
	"fmt"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

type edgeKey struct {
	predID    int
	joinLabel string
}

func rewriteFunction(fn *mir.Function, alloc *regAllocResult, fa *functionAnalysis, frame *mir.FrameLayout, abi target.ABI) error {
	if fn == nil || alloc == nil || fa == nil || frame == nil {
		return nil
	}

	edgeLoads := make(map[edgeKey][]mir.Instr)

	for _, ba := range fa.blocks {
		if ba == nil || ba.block == nil {
			continue
		}

		newInstrs := make([]mir.Instr, 0, len(ba.block.Instrs)+4)
		for idx, item := range ba.items {
			if item == nil || item.instr == nil {
				continue
			}
			if isTerminator(item.instr) {
				term, pre, err := rewriteTerminator(item.instr.(mir.Terminator), item.liveIn, alloc, frame, abi, ba)
				if err != nil {
					return err
				}
				newInstrs = append(newInstrs, pre...)
				if term != nil {
					ba.block.Term = term
				}
				continue
			}

			if phi, ok := item.instr.(*mir.PhiInstr); ok {
				rewritten, loadsByPred, err := rewritePhi(phi, ba, fa, alloc, frame, abi)
				if err != nil {
					return err
				}
				newInstrs = append(newInstrs, rewritten)
				for key, loads := range loadsByPred {
					edgeLoads[key] = append(edgeLoads[key], loads...)
				}
				_ = idx
				continue
			}

			rewritten, pre, post, err := rewriteInstr(item.instr, item.liveIn, alloc, frame, abi, ba)
			if err != nil {
				return err
			}
			newInstrs = append(newInstrs, pre...)
			if rewritten != nil {
				newInstrs = append(newInstrs, rewritten)
			}
			newInstrs = append(newInstrs, post...)
		}

		ba.block.Instrs = newInstrs
	}

	return materializeEdgeBlocks(fn, fa, edgeLoads)
}

func rewritePhi(phi *mir.PhiInstr, ba *blockAnalysis, fa *functionAnalysis, alloc *regAllocResult, frame *mir.FrameLayout, abi target.ABI) (*mir.PhiInstr, map[edgeKey][]mir.Instr, error) {
	if phi == nil {
		return nil, nil, nil
	}
	loadsByPred := make(map[edgeKey][]mir.Instr)
	rewritten := &mir.PhiInstr{Dst: phi.Dst, Arms: make([]mir.PhiArm, 0, len(phi.Arms))}

	if phi.Dst != nil && phi.Dst.Kind == mir.VirtualReg {
		preg, ok := alloc.mapVRegToPReg[phi.Dst.Name]
		if !ok {
			if _, spilled := alloc.spillSlots[phi.Dst.Name]; spilled {
				return nil, nil, fmt.Errorf("register allocation: spilled phi destination %q", phi.Dst.Name)
			}
			return nil, nil, fmt.Errorf("register allocation: missing color for phi destination %q", phi.Dst.Name)
		}
		rewritten.Dst = &mir.Register{Name: preg, Kind: mir.PhysicalReg, Ty: phi.Dst.Type()}
	}

	for _, arm := range phi.Arms {
		if arm.Value == nil {
			rewritten.Arms = append(rewritten.Arms, arm)
			continue
		}
		pred := predecessorByLabel(ba.block, arm.BlockLabel)
		if pred == nil {
			return nil, nil, fmt.Errorf("register allocation: phi arm predecessor %q not found", arm.BlockLabel)
		}
		predBA := fa.byID[pred.ID]
		if predBA == nil {
			return nil, nil, fmt.Errorf("register allocation: predecessor analysis for %q missing", pred.Label)
		}
		val, pre, _, err := rewriteOperand(arm.Value, alloc, frame, abi, predBA.blockOut)
		if err != nil {
			return nil, nil, err
		}
		if len(pre) > 0 {
			key := edgeKey{predID: pred.ID, joinLabel: ba.block.Label}
			loadsByPred[key] = append(loadsByPred[key], pre...)
		}
		rewritten.Arms = append(rewritten.Arms, mir.PhiArm{BlockLabel: arm.BlockLabel, Value: val})
	}

	return rewritten, loadsByPred, nil
}

func rewriteInstr(instr mir.Instr, liveIn map[string]bool, alloc *regAllocResult, frame *mir.FrameLayout, abi target.ABI, ba *blockAnalysis) (mir.Instr, []mir.Instr, []mir.Instr, error) {
	occupied := physicalRegsInUse(liveIn, alloc)

	usedTemps := make(map[string]bool)
	choose := func(hint *mir.Type) (*mir.Register, error) {
		for _, reg := range alloc.scratchRegs {
			if !occupied[reg] && !usedTemps[reg] {
				usedTemps[reg] = true
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		for _, reg := range freeColorsFor(liveIn, alloc) {
			if !occupied[reg] && !usedTemps[reg] {
				usedTemps[reg] = true
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		return nil, fmt.Errorf("register allocation: no scratch register available for %T", instr)
	}

	mapOperand := func(op mir.Operand) (mir.Operand, []mir.Instr, []mir.Instr, error) {
		return rewriteOperandWithChooser(op, alloc, frame, abi, choose, ba)
	}

	switch ins := instr.(type) {
	case *mir.MoveInstr:
		src, pre, post, err := mapOperand(ins.Src)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Src = src
		return ins, pre, post, nil
	case *mir.LoadInstr:
		addr, pre, post, err := mapOperand(ins.Addr)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Addr = addr
		return ins, pre, post, nil
	case *mir.StoreInstr:
		addr, pre1, post1, err := mapOperand(ins.Addr)
		if err != nil {
			return nil, nil, nil, err
		}
		val, pre2, post2, err := mapOperand(ins.Value)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Addr = addr
		ins.Value = val
		return ins, append(pre1, pre2...), append(post1, post2...), nil
	case *mir.UnaryInstr:
		x, pre, post, err := mapOperand(ins.X)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.X = x
		return ins, pre, post, nil
	case *mir.BinaryInstr:
		left, pre1, post1, err := mapOperand(ins.Left)
		if err != nil {
			return nil, nil, nil, err
		}
		right, pre2, post2, err := mapOperand(ins.Right)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Left = left
		ins.Right = right
		return ins, append(pre1, pre2...), append(post1, post2...), nil
	case *mir.CompareInstr:
		left, pre1, post1, err := mapOperand(ins.Left)
		if err != nil {
			return nil, nil, nil, err
		}
		right, pre2, post2, err := mapOperand(ins.Right)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Left = left
		ins.Right = right
		return ins, append(pre1, pre2...), append(post1, post2...), nil
	case *mir.CallInstr:
		callee, pre1, post1, err := mapOperand(ins.Callee)
		if err != nil {
			return nil, nil, nil, err
		}
		ins.Callee = callee
		pre := append([]mir.Instr(nil), pre1...)
		post := append([]mir.Instr(nil), post1...)
		args := make([]mir.Operand, 0, len(ins.Args))
		for _, arg := range ins.Args {
			mapped, pre2, post2, err := mapOperand(arg)
			if err != nil {
				return nil, nil, nil, err
			}
			pre = append(pre, pre2...)
			post = append(post, post2...)
			args = append(args, mapped)
		}
		ins.Args = args
		return ins, pre, post, nil
	case *mir.MachineInstr:
		switch strings.ToLower(ins.Op) {
		case "spill":
			if len(ins.Srcs) == 0 && len(ins.Dsts) == 0 {
				return nil, nil, nil, nil
			}
			srcOp := firstOperand(ins.Srcs)
			if srcOp == nil && len(ins.Dsts) > 0 {
				srcOp = ins.Dsts[0]
			}
			if srcOp == nil {
				return nil, nil, nil, fmt.Errorf("register allocation: spill requires a source operand")
			}
			src, pre, post, err := mapOperand(srcOp)
			if err != nil {
				return nil, nil, nil, err
			}
			reg, ok := src.(*mir.Register)
			if !ok {
				return nil, nil, nil, fmt.Errorf("register allocation: spill source must resolve to a register, got %T", src)
			}
			slotName := spillOperandName(srcOp)
			slot, ok := alloc.spillSlots[slotName]
			if !ok {
				return nil, nil, nil, fmt.Errorf("register allocation: missing spill slot for %q", slotName)
			}
			return &mir.StoreInstr{Addr: spillSlotAddr(slot, abi), Value: reg}, pre, post, nil
		case "reload":
			dstReg := firstRegister(ins.Dsts)
			if dstReg == nil && len(ins.Srcs) > 0 {
				if reg, ok := ins.Srcs[0].(*mir.Register); ok {
					dstReg = reg
				}
			}
			if dstReg == nil {
				return nil, nil, nil, fmt.Errorf("register allocation: reload requires a destination operand")
			}
			mapped, pre, post, err := mapOperand(dstReg)
			if err != nil {
				return nil, nil, nil, err
			}
				if reg, ok := mapped.(*mir.Register); ok {
				slotName := dstReg.Name
				slot, ok := alloc.spillSlots[slotName]
				if !ok {
					return nil, nil, nil, fmt.Errorf("register allocation: missing spill slot for %q", slotName)
				}
				return &mir.LoadInstr{Dst: reg, Addr: spillSlotAddr(slot, abi)}, pre, post, nil
			}
			if len(pre) > 0 {
				return nil, pre, post, nil
			}
			return nil, nil, nil, fmt.Errorf("register allocation: reload destination must resolve to a register")
		}
		pre := make([]mir.Instr, 0)
		post := make([]mir.Instr, 0)
		for i, dst := range ins.Dsts {
			mapped, pre1, post1, err := mapOperand(dst)
			if err != nil {
				return nil, nil, nil, err
			}
			pre = append(pre, pre1...)
			post = append(post, post1...)
			if r, ok := mapped.(*mir.Register); ok {
				ins.Dsts[i] = r
			}
		}
		for i, src := range ins.Srcs {
			mapped, pre1, post1, err := mapOperand(src)
			if err != nil {
				return nil, nil, nil, err
			}
			pre = append(pre, pre1...)
			post = append(post, post1...)
			ins.Srcs[i] = mapped
		}
		return ins, pre, post, nil
	default:
		return instr, nil, nil, nil
	}
}

func firstOperand(ops []mir.Operand) mir.Operand {
	if len(ops) == 0 {
		return nil
	}
	return ops[0]
}

func firstRegister(rs []*mir.Register) *mir.Register {
	if len(rs) == 0 {
		return nil
	}
	return rs[0]
}

func spillOperandName(op mir.Operand) string {
	switch x := op.(type) {
	case *mir.Register:
		return x.Name
	case *mir.Label:
		return x.Name
	case *mir.Symbol:
		return x.Name
	case *mir.Immediate:
		return fmt.Sprint(x.Value)
	default:
		return fmt.Sprint(op)
	}
}

func rewriteTerminator(term mir.Terminator, liveIn map[string]bool, alloc *regAllocResult, frame *mir.FrameLayout, abi target.ABI, ba *blockAnalysis) (mir.Terminator, []mir.Instr, error) {
	if term == nil {
		return nil, nil, nil
	}
	occupied := physicalRegsInUse(liveIn, alloc)
	usedTemps := make(map[string]bool)
	choose := func(hint *mir.Type) (*mir.Register, error) {
		for _, reg := range alloc.scratchRegs {
			if !occupied[reg] && !usedTemps[reg] {
				usedTemps[reg] = true
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		for _, reg := range freeColorsFor(liveIn, alloc) {
			if !occupied[reg] && !usedTemps[reg] {
				usedTemps[reg] = true
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		return nil, fmt.Errorf("register allocation: no scratch register available for terminator %T", term)
	}

	mapOperand := func(op mir.Operand) (mir.Operand, []mir.Instr, error) {
		mapped, pre, _, err := rewriteOperandWithChooser(op, alloc, frame, abi, choose, ba)
		return mapped, pre, err
	}

	switch t := term.(type) {
	case *mir.JumpInstr:
		return t, nil, nil
	case *mir.CondBrInstr:
		cond, pre, err := mapOperand(t.Cond)
		if err != nil {
			return nil, nil, err
		}
		t.Cond = cond
		return t, pre, nil
	case *mir.ReturnInstr:
		if t.Value == nil {
			return t, nil, nil
		}
		val, pre, err := mapOperand(t.Value)
		if err != nil {
			return nil, nil, err
		}
		t.Value = val
		return t, pre, nil
	case *mir.HaltInstr:
		if t.Code == nil {
			return t, nil, nil
		}
		val, pre, err := mapOperand(t.Code)
		if err != nil {
			return nil, nil, err
		}
		t.Code = val
		return t, pre, nil
	case *mir.SwitchInstr:
		val, pre, err := mapOperand(t.Value)
		if err != nil {
			return nil, nil, err
		}
		t.Value = val
		for i, arm := range t.Arms {
			if arm.Value == nil {
				continue
			}
			mapped, pre2, err := mapOperand(arm.Value)
			if err != nil {
				return nil, nil, err
			}
			pre = append(pre, pre2...)
			t.Arms[i].Value = mapped
		}
		return t, pre, nil
	case *mir.MachineTerm:
		pre := make([]mir.Instr, 0)
		for i, src := range t.Srcs {
			mapped, pre2, err := mapOperand(src)
			if err != nil {
				return nil, nil, err
			}
			pre = append(pre, pre2...)
			t.Srcs[i] = mapped
		}
		return t, pre, nil
	default:
		return t, nil, nil
	}
}

func materializeEdgeBlocks(fn *mir.Function, fa *functionAnalysis, edgeLoads map[edgeKey][]mir.Instr) error {
	if fn == nil || fa == nil || len(edgeLoads) == 0 {
		return nil
	}

	keys := make([]edgeKey, 0, len(edgeLoads))
	for key, loads := range edgeLoads {
		if len(loads) == 0 {
			continue
		}
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].predID != keys[j].predID {
			return keys[i].predID < keys[j].predID
		}
		return keys[i].joinLabel < keys[j].joinLabel
	})

	nextID := maxBlockID(fn) + 1
	for _, key := range keys {
		loads := edgeLoads[key]
		if len(loads) == 0 {
			continue
		}

		pred := fa.byID[key.predID]
		join := fa.byName[key.joinLabel]
		if pred == nil || join == nil {
			return fmt.Errorf("register allocation: edge %d -> %s references unknown blocks", key.predID, key.joinLabel)
		}

		edge := mir.NewBlock(nextID, uniqueEdgeLabel(fn, pred.block.Label, join.block.Label))
		nextID++
		edge.Instrs = append(edge.Instrs, loads...)
		edge.Term = &mir.JumpInstr{Target: join.block.Label}

		if err := redirectEdge(pred.block, join.block, edge); err != nil {
			return err
		}

		fn.AddBlock(edge)
	}

	return nil
}

func redirectEdge(pred, join, edge *mir.Block) error {
	if pred == nil || join == nil || edge == nil {
		return fmt.Errorf("register allocation: cannot redirect nil edge block")
	}
	if pred.Term == nil {
		return fmt.Errorf("register allocation: predecessor %s has no terminator to redirect", pred.Label)
	}
	if !replaceTerminatorTarget(pred.Term, join.Label, edge.Label) {
		return fmt.Errorf("register allocation: predecessor %s does not target %s", pred.Label, join.Label)
	}

	if pred.Succs == nil {
		pred.Succs = make(map[int]*mir.Block)
	}
	delete(pred.Succs, join.ID)
	pred.Succs[edge.ID] = edge

	if join.Preds == nil {
		join.Preds = make(map[int]*mir.Block)
	}
	delete(join.Preds, pred.ID)
	join.Preds[edge.ID] = edge

	edge.Preds = make(map[int]*mir.Block, 1)
	edge.Succs = make(map[int]*mir.Block, 1)
	edge.Preds[pred.ID] = pred
	edge.Succs[join.ID] = join

	return nil
}

func replaceTerminatorTarget(term mir.Terminator, oldLabel, newLabel string) bool {
	if term == nil || oldLabel == "" || newLabel == "" {
		return false
	}
	switched := false
	switch t := term.(type) {
	case *mir.JumpInstr:
		if t.Target == oldLabel {
			t.Target = newLabel
			switched = true
		}
	case *mir.CondBrInstr:
		if t.TrueLabel == oldLabel {
			t.TrueLabel = newLabel
			switched = true
		}
		if t.FalseLabel == oldLabel {
			t.FalseLabel = newLabel
			switched = true
		}
	case *mir.SwitchInstr:
		if t.Default == oldLabel {
			t.Default = newLabel
			switched = true
		}
		for i := range t.Arms {
			if t.Arms[i].Label == oldLabel {
				t.Arms[i].Label = newLabel
				switched = true
			}
		}
	case *mir.MachineTerm:
		for i := range t.Targets {
			if t.Targets[i] == oldLabel {
				t.Targets[i] = newLabel
				switched = true
			}
		}
	}
	return switched
}

func uniqueEdgeLabel(fn *mir.Function, predLabel, joinLabel string) string {
	base := fmt.Sprintf("%s.__ra_edge__%s", predLabel, joinLabel)
	if base == "" {
		base = "__ra_edge"
	}
	label := base
	for i := 0; fn != nil && fn.BlockByLabel(label) != nil; i++ {
		label = fmt.Sprintf("%s.%d", base, i+1)
	}
	return label
}

func maxBlockID(fn *mir.Function) int {
	if fn == nil {
		return -1
	}
	maxID := -1
	for _, blk := range fn.Blocks {
		if blk == nil {
			continue
		}
		if blk.ID > maxID {
			maxID = blk.ID
		}
	}
	return maxID
}

func rewriteOperandWithChooser(op mir.Operand, alloc *regAllocResult, frame *mir.FrameLayout, abi target.ABI, choose func(*mir.Type) (*mir.Register, error), ba *blockAnalysis) (mir.Operand, []mir.Instr, []mir.Instr, error) {
	switch v := op.(type) {
	case *mir.Register:
		if v == nil {
			return nil, nil, nil, nil
		}
		if v.Kind == mir.PhysicalReg {
			return v, nil, nil, nil
		}
		if preg, ok := alloc.mapVRegToPReg[v.Name]; ok {
			return &mir.Register{Name: preg, Kind: mir.PhysicalReg, Ty: v.Type()}, nil, nil, nil
		}
		slot, ok := alloc.spillSlots[v.Name]
		if !ok {
			return nil, nil, nil, fmt.Errorf("register allocation: missing spill slot for %q", v.Name)
		}
		tmp, err := choose(v.Type())
		if err != nil {
			return nil, nil, nil, err
		}
		load := spillLoad(tmp, frameOffset(slot, abi.WordSize), abi)
		return tmp, []mir.Instr{load}, nil, nil
	case *mir.Memory:
		base, pre1, post1, err := rewriteOperandWithChooser(v.Base, alloc, frame, abi, choose, ba)
		if err != nil {
			return nil, nil, nil, err
		}
		offset, pre2, post2, err := rewriteOperandWithChooser(v.Offset, alloc, frame, abi, choose, ba)
		if err != nil {
			return nil, nil, nil, err
		}
		mem := &mir.Memory{Base: base, Offset: offset, Ty: v.Type()}
		return mem, append(pre1, pre2...), append(post1, post2...), nil
	default:
		return op, nil, nil, nil
	}
}

func rewriteOperand(op mir.Operand, alloc *regAllocResult, frame *mir.FrameLayout, abi target.ABI, live map[string]bool) (mir.Operand, []mir.Instr, []mir.Instr, error) {
	choose := func(hint *mir.Type) (*mir.Register, error) {
		occupied := physicalRegsInUse(live, alloc)
		free := freeColorsFor(live, alloc)
		for _, reg := range alloc.scratchRegs {
			if !occupied[reg] {
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		for _, reg := range free {
			if !occupied[reg] {
				return &mir.Register{Name: reg, Kind: mir.PhysicalReg, Ty: hint}, nil
			}
		}
		return nil, fmt.Errorf("register allocation: no scratch register available")
	}
	return rewriteOperandWithChooser(op, alloc, frame, abi, choose, nil)
}

func spillLoad(dst *mir.Register, offset int, abi target.ABI) *mir.LoadInstr {
	base := abi.FramePointer
	if base == "" {
		base = abi.StackPointer
	}
	return &mir.LoadInstr{Dst: dst, Addr: &mir.Memory{Base: &mir.Register{Name: base, Kind: mir.PhysicalReg}, Offset: &mir.Immediate{Value: offset}}}
}

func spillSlotAddr(slot int, abi target.ABI) *mir.Memory {
	base := abi.FramePointer
	if base == "" {
		base = abi.StackPointer
	}
	return &mir.Memory{Base: &mir.Register{Name: base, Kind: mir.PhysicalReg}, Offset: &mir.Immediate{Value: frameOffset(slot, abi.WordSize)}}
}

func frameOffset(slot int, wordSize int) int {
	if wordSize <= 0 {
		wordSize = 8
	}
	return -(slot + 1) * wordSize
}

func freeColorsFor(live map[string]bool, alloc *regAllocResult) []string {
	occupied := physicalRegsInUse(live, alloc)
	free := make([]string, 0)
	for vreg, preg := range alloc.mapVRegToPReg {
		if vreg == preg {
			continue
		}
		if containsString(alloc.scratchRegs, preg) {
			continue
		}
		if !occupied[preg] {
			free = append(free, preg)
		}
	}
	return uniqueStrings(free)
}

func physicalRegsInUse(live map[string]bool, alloc *regAllocResult) map[string]bool {
	occupied := make(map[string]bool)
	for name := range live {
		if preg, ok := alloc.mapVRegToPReg[name]; ok {
			occupied[preg] = true
		}
	}
	return occupied
}

func predecessorByLabel(block *mir.Block, label string) *mir.Block {
	if block == nil {
		return nil
	}
	for _, pred := range block.Preds {
		if pred != nil && pred.Label == label {
			return pred
		}
	}
	return nil
}

func virtualDefs(instr mir.Instr) []string {
	if instr == nil {
		return nil
	}
	defs := instr.Defs()
	out := make([]string, 0, len(defs))
	for _, d := range defs {
		if d == nil || d.Kind != mir.VirtualReg {
			continue
		}
		out = append(out, d.Name)
	}
	return out
}

func virtualUses(instr mir.Instr) []string {
	if instr == nil {
		return nil
	}
	out := make([]string, 0)
	for _, op := range instr.Uses() {
		for _, reg := range operandRegs(op) {
			out = append(out, reg)
		}
	}
	return uniqueStrings(out)
}

func operandRegs(op mir.Operand) []string {
	switch v := op.(type) {
	case *mir.Register:
		if v != nil && v.Kind == mir.VirtualReg {
			return []string{v.Name}
		}
	case *mir.Memory:
		regs := operandRegs(v.Base)
		regs = append(regs, operandRegs(v.Offset)...)
		return uniqueStrings(regs)
	}
	return nil
}

func abiColorPools(abi target.ABI) (colors []string, scratch []string) {
	colors = append(colors, abi.CallerSaved...)
	colors = append(colors, abi.CalleeSaved...)
	colors = append(colors, abi.IntArgRegs...)
	colors = append(colors, abi.IntRetRegs...)
	colors = uniqueStrings(colors)
	colors = filterStrings(colors, func(reg string) bool {
		return reg != abi.StackPointer && reg != abi.FramePointer && reg != abi.LinkRegister
	})

	preferredScratch := make([]string, 0)
	for _, reg := range abi.CallerSaved {
		if reg == abi.StackPointer || reg == abi.FramePointer || reg == abi.LinkRegister {
			continue
		}
		if containsString(abi.IntArgRegs, reg) || containsString(abi.IntRetRegs, reg) {
			continue
		}
		preferredScratch = append(preferredScratch, reg)
	}
	preferredScratch = uniqueStrings(preferredScratch)
	if len(preferredScratch) < 2 {
		for _, reg := range colors {
			if containsString(preferredScratch, reg) {
				continue
			}
			preferredScratch = append(preferredScratch, reg)
			if len(preferredScratch) >= 2 {
				break
			}
		}
	}
	if len(preferredScratch) > 2 {
		preferredScratch = preferredScratch[:2]
	}
	scratch = preferredScratch
	colors = filterStrings(colors, func(reg string) bool { return !containsString(scratch, reg) })
	return colors, scratch
}

func calleeSavedOnly(colors []string, abi target.ABI) []string {
	out := make([]string, 0)
	for _, reg := range colors {
		if containsString(abi.CalleeSaved, reg) {
			out = append(out, reg)
		}
	}
	return out
}

func pickSimplifyNode(order []string, remaining map[string]bool, degree map[string]int, allowedCount func(string) int) string {
	best := ""
	for _, name := range order {
		if !remaining[name] {
			continue
		}
		if degree[name] >= allowedCount(name) {
			continue
		}
		if best == "" || degree[name] < degree[best] || (degree[name] == degree[best] && name < best) {
			best = name
		}
	}
	return best
}

func pickSpillNode(order []string, remaining map[string]bool, nodes map[string]*graphNode, degree map[string]int) string {
	best := ""
	for _, name := range order {
		if !remaining[name] {
			continue
		}
		if best == "" {
			best = name
			continue
		}
		if nodes[name].weight > nodes[best].weight ||
			(nodes[name].weight == nodes[best].weight && degree[name] > degree[best]) ||
			(nodes[name].weight == nodes[best].weight && degree[name] == degree[best] && name < best) {
			best = name
		}
	}
	return best
}
