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
	return []mir.Instr{mir.NewMachineInstr(name, nil, args)}, true, nil
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

