package legalize

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// Run normalizes a selected MIR program into ABI-correct MIR.
func Run(prog *mir.Program, tgt target.Target) (*mir.Program, error) {
	if prog == nil {
		return nil, fmt.Errorf("backend legalization: nil MIR program")
	}
	if tgt == nil {
		return nil, fmt.Errorf("backend legalization: nil target")
	}

	abi := tgt.ABIInfo()
	retReg, ok := abi.RetReg(0)
	if !ok || retReg == "" {
		return prog, nil
	}

	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			if err := normalizeFunction(fn, tgt, retReg); err != nil {
				return nil, fmt.Errorf("function %s: %w", fn.Name, err)
			}
		}
	}

	return prog, nil
}

func normalizeFunction(fn *mir.Function, tgt target.Target, retReg string) error {
	if fn == nil {
		return nil
	}
	for _, blk := range fn.Blocks {
		if blk == nil || blk.Term == nil {
			continue
		}
		if err := normalizeLegalizeBlock(blk); err != nil {
			return fmt.Errorf("block %s: %w", blk.Label, err)
		}
		if err := normalizeReturnBlock(blk, tgt, retReg); err != nil {
			return fmt.Errorf("block %s: %w", blk.Label, err)
		}
	}
	return nil
}

func normalizeLegalizeBlock(blk *mir.Block) error {
	if blk == nil {
		return nil
	}
	for _, instr := range blk.Instrs {
		mi, ok := instr.(*mir.MachineInstr)
		if !ok {
			continue
		}
		switch strings.ToLower(mi.Op) {
		case "spill":
			if len(mi.Srcs) == 0 && len(mi.Dsts) == 1 {
				mi.Srcs = []mir.Operand{mi.Dsts[0]}
				mi.Dsts = nil
			}
		case "reload":
			if len(mi.Dsts) == 0 && len(mi.Srcs) == 1 {
				if reg, ok := mi.Srcs[0].(*mir.Register); ok {
					mi.Dsts = []*mir.Register{reg}
					mi.Srcs = nil
				}
			}
		}
	}
	return nil
}

func normalizeReturnBlock(blk *mir.Block, tgt target.Target, retReg string) error {
	if blk == nil || blk.Term == nil || retReg == "" {
		return nil
	}

	switch term := blk.Term.(type) {
	case *mir.ReturnInstr:
		if rewriteTrailingReturnMove(blk, tgt, retReg) {
			term.Value = nil
			return nil
		}
		if term.Value != nil {
			blk.AddInstr(returnMoveInstr(tgt, term.Value, retReg))
			term.Value = nil
		}
	case *mir.MachineTerm:
		if !strings.EqualFold(term.Op, "ret") {
			return nil
		}
		if rewriteTrailingReturnMove(blk, tgt, retReg) {
			term.Srcs = nil
			return nil
		}
		if len(term.Srcs) > 0 {
			blk.AddInstr(returnMoveInstr(tgt, term.Srcs[0], retReg))
			term.Srcs = nil
		}
	}

	return nil
}

func rewriteTrailingReturnMove(blk *mir.Block, tgt target.Target, retReg string) bool {
	if blk == nil || len(blk.Instrs) == 0 || retReg == "" {
		return false
	}

	last := blk.Instrs[len(blk.Instrs)-1]
	switch x := last.(type) {
	case *mir.MoveInstr:
		if x.Dst == nil || x.Dst.Name != retReg {
			return false
		}
		blk.Instrs[len(blk.Instrs)-1] = returnMoveInstr(tgt, x.Src, retReg)
		return true
	case *mir.MachineInstr:
		if len(x.Dsts) != 1 || x.Dsts[0] == nil || x.Dsts[0].Name != retReg {
			return false
		}
		switch strings.ToLower(x.Op) {
		case "mov":
			var src mir.Operand
			if len(x.Srcs) > 0 {
				src = x.Srcs[0]
			}
			blk.Instrs[len(blk.Instrs)-1] = returnMoveInstr(tgt, src, retReg)
			return true
		case "addi", "add":
			return true
		default:
			return false
		}
	default:
		return false
	}
}

func returnMoveInstr(tgt target.Target, src mir.Operand, retReg string) mir.Instr {
	var ty *mir.Type
	if src != nil {
		ty = src.Type()
	}
	dst := mir.NewRegister(retReg, mir.PhysicalReg, ty)
	zero := mir.NewImmediate(0, ty)
	name := ""
	if tgt != nil {
		name = strings.ToLower(tgt.Name())
	}
	switch {
	case strings.Contains(name, "rv64") || strings.Contains(name, "riscv"):
		return mir.NewMachineInstr("addi", []*mir.Register{dst}, []mir.Operand{src, zero})
	case strings.Contains(name, "arm64") || strings.Contains(name, "aarch64"):
		return mir.NewMachineInstr("add", []*mir.Register{dst}, []mir.Operand{src, zero})
	default:
		return mir.NewMachineInstr("mov", []*mir.Register{dst}, []mir.Operand{src})
	}
}

