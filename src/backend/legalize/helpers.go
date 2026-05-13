package legalize

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// ResolveOperand looks up a named legalization operand in env and returns it as
// a MIR operand when possible.
func ResolveOperand(name string, env map[string]any) (mir.Operand, bool) {
	if strings.TrimSpace(name) == "" {
		return nil, false
	}
	if strings.HasPrefix(name, "$") {
		if resolved, ok := env[trimDollar(name)]; ok {
			if op, ok := asOperand(resolved); ok {
				return op, true
			}
		}
		return nil, false
	}
	if resolved, ok := env[name]; ok {
		if op, ok := asOperand(resolved); ok {
			return op, true
		}
	}
	if resolved, ok := env[trimDollar(name)]; ok {
		if op, ok := asOperand(resolved); ok {
			return op, true
		}
	}
	return nil, false
}

// BuildRewrite turns a legalization rewrite into a machine instruction.
func BuildRewrite(name string, args []mir.Operand) ([]mir.Instr, bool, error) {
	if strings.TrimSpace(name) == "" {
		return nil, false, nil
	}
	var dsts []*mir.Register
	if len(args) > 0 {
		if reg, ok := args[0].(*mir.Register); ok {
			dsts = append(dsts, reg)
			args = args[1:]
		}
	}
	return []mir.Instr{mir.NewMachineInstr(name, dsts, args)}, true, nil
}

// BuildSpill turns a spill request into a canonical pseudo instruction.
func BuildSpill(op mir.Operand) ([]mir.Instr, bool, error) {
	if op == nil {
		return nil, false, nil
	}
	return []mir.Instr{mir.NewMachineInstr("spill", nil, []mir.Operand{op})}, true, nil
}

// BuildReload turns a reload request into a canonical pseudo instruction.
func BuildReload(op mir.Operand) ([]mir.Instr, bool, error) {
	reg, ok := op.(*mir.Register)
	if !ok {
		return nil, false, fmt.Errorf("legalize reload operand must resolve to a register, got %T", op)
	}
	return []mir.Instr{mir.NewMachineInstr("reload", []*mir.Register{reg}, nil)}, true, nil
}

// BuildMove copies a resolved source operand into a resolved destination register.
func BuildMove(src, dst mir.Operand) ([]mir.Instr, bool, error) {
	reg, ok := dst.(*mir.Register)
	if !ok {
		return nil, false, fmt.Errorf("legalize move destination must resolve to a register, got %T", dst)
	}
	if src == nil {
		return nil, false, nil
	}
	return []mir.Instr{mir.NewMachineInstr("mov", []*mir.Register{reg}, []mir.Operand{src})}, true, nil
}

// BuildHi20 returns the upper 20 bits of a large RV64 immediate.
func BuildHi20(op mir.Operand) (mir.Operand, bool) {
	if sym, ok := operandToSymbol(op); ok {
		return relocSymbol(sym, "@HI20"), true
	}
	v, ok := operandToInt64(op)
	if !ok {
		return nil, false
	}
	hi := (v + 0x800) >> 12
	return &mir.Immediate{Value: hi}, true
}

// BuildLo12 returns the low 12 bits of a large RV64 immediate.
func BuildLo12(op mir.Operand) (mir.Operand, bool) {
	if sym, ok := operandToSymbol(op); ok {
		return relocSymbol(sym, "@LO12"), true
	}
	v, ok := operandToInt64(op)
	if !ok {
		return nil, false
	}
	hi := (v + 0x800) >> 12
	lo := v - (hi << 12)
	return &mir.Immediate{Value: lo}, true
}

// BuildImm16Part returns a 16-bit chunk of a large immediate, selecting the
// requested lane from the low end of the value.
func BuildImm16Part(op mir.Operand, part int) (mir.Operand, bool) {
	v, ok := operandToUint64(op)
	if !ok || part < 0 || part > 3 {
		return nil, false
	}
	shift := uint(part * 16)
	chunk := (v >> shift) & 0xffff
	return &mir.Immediate{Value: int64(chunk)}, true
}

// BuildPageHi returns an ARM64 PAGE relocation-like symbol for a global.
func BuildPageHi(op mir.Operand) (mir.Operand, bool) {
	if sym, ok := operandToSymbol(op); ok {
		return relocSymbol(sym, "@PAGE"), true
	}
	return nil, false
}

// BuildPageOff returns an ARM64 PAGEOFF relocation-like symbol for a global.
func BuildPageOff(op mir.Operand) (mir.Operand, bool) {
	if sym, ok := operandToSymbol(op); ok {
		return relocSymbol(sym, "@PAGEOFF"), true
	}
	return nil, false
}

// EmitsReturnMove reports whether instrs materialize a value into an ABI return register.
func EmitsReturnMove(instrs []mir.Instr, abi string) bool {
	if len(instrs) == 0 {
		return false
	}
	retRegs := abiReturnRegs(abi)
	if len(retRegs) == 0 {
		return false
	}
	for _, instr := range instrs {
		switch x := instr.(type) {
		case *mir.MoveInstr:
			if x.Dst != nil && hasRegisterName(retRegs, x.Dst.Name) {
				return true
			}
		case *mir.MachineInstr:
			if len(x.Dsts) != 1 || x.Dsts[0] == nil || !hasRegisterName(retRegs, x.Dsts[0].Name) {
				continue
			}
			if strings.EqualFold(x.Op, "mov") || strings.EqualFold(x.Op, "add") || strings.EqualFold(x.Op, "addi") {
				return true
			}
		}
	}
	return false
}

// NormalizeReturnTerminator strips the return value from a return terminator.
func NormalizeReturnTerminator(term mir.Terminator) {
	switch t := term.(type) {
	case *mir.ReturnInstr:
		t.Value = nil
	case *mir.MachineTerm:
		if strings.EqualFold(t.Op, "ret") {
			t.Srcs = nil
		}
	}
}

func trimDollar(s string) string { return strings.TrimPrefix(s, "$") }

func asOperand(v any) (mir.Operand, bool) {
	op, ok := v.(mir.Operand)
	return op, ok
}

func operandToInt64(op mir.Operand) (int64, bool) {
	imm, ok := op.(*mir.Immediate)
	if !ok || imm == nil {
		return 0, false
	}
	switch v := imm.Value.(type) {
	case int:
		return int64(v), true
	case int8:
		return int64(v), true
	case int16:
		return int64(v), true
	case int32:
		return int64(v), true
	case int64:
		return v, true
	case uint:
		return int64(v), true
	case uint8:
		return int64(v), true
	case uint16:
		return int64(v), true
	case uint32:
		return int64(v), true
	case uint64:
		if v > ^uint64(0)>>1 {
			return 0, false
		}
		return int64(v), true
	case string:
		return 0, false
	default:
		return 0, false
	}
}

func operandToUint64(op mir.Operand) (uint64, bool) {
	imm, ok := op.(*mir.Immediate)
	if !ok || imm == nil {
		return 0, false
	}
	switch v := imm.Value.(type) {
	case int:
		return uint64(int64(v)), true
	case int8:
		return uint64(int64(v)), true
	case int16:
		return uint64(int64(v)), true
	case int32:
		return uint64(int64(v)), true
	case int64:
		return uint64(v), true
	case uint:
		return uint64(v), true
	case uint8:
		return uint64(v), true
	case uint16:
		return uint64(v), true
	case uint32:
		return uint64(v), true
	case uint64:
		return v, true
	default:
		return 0, false
	}
}

func operandToSymbol(op mir.Operand) (*mir.Symbol, bool) {
	sym, ok := op.(*mir.Symbol)
	if !ok || sym == nil || strings.TrimSpace(sym.Name) == "" {
		return nil, false
	}
	return sym, true
}

func relocSymbol(sym *mir.Symbol, suffix string) *mir.Symbol {
	if sym == nil {
		return nil
	}
	return &mir.Symbol{Name: sym.Name + suffix, Ty: sym.Ty}
}

func hasRegisterName(names []string, want string) bool {
	for _, name := range names {
		if strings.EqualFold(name, want) {
			return true
		}
	}
	return false
}

func abiReturnRegs(abi string) []string {
	if strings.TrimSpace(abi) == "" {
		return []string{"a0", "x0"}
	}
	switch strings.ToLower(strings.TrimSpace(abi)) {
	case "lp64d", "rv64imafd", "riscv64", "riscv", "rv64":
		return []string{"a0"}
	case "aapcs64", "aapcs64-macos", "arm64", "aarch64":
		return []string{"x0"}
	default:
		return nil
	}
}

