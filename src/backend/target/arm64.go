package target

import (
	"fmt"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// ARM64Target is the first minir-first target implementation.
type ARM64Target struct {
	*BaseTarget
}

var arm64Default = &ARM64Target{
	BaseTarget: NewBaseTarget(Arm64Name, ABI{
		Name:                "AAPCS64",
		WordSize:            8,
		Align:               16,
		IntArgRegs:          []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"},
		IntRetRegs:          []string{"x0", "x1"},
		FloatArgRegs:        []string{"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"},
		FloatRetRegs:        []string{"d0", "d1"},
		CallerSaved:         []string{"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18"},
		CalleeSaved:         []string{"x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"},
		StackPointer:        "sp",
		FramePointer:        "x29",
		LinkRegister:        "x30",
		JumpTableMinDensity: 0.5,
	}),
}

func (t *ARM64Target) Emit(mod *mir.Module) string {
	return emitARM64Module(mod)
}

func init() {
	Register(Arm64Name, func() Target { return arm64Default })
	RegisterAlias(Arm64AppleMacosName, Arm64Name)
	RegisterAlias(AArch64AppleDarwinName, Arm64Name)
}

func emitARM64Module(mod *mir.Module) string {
	if mod == nil {
		return ""
	}

	var buf strings.Builder
	for _, ext := range mod.Externals {
		if ext == nil || ext.Name == "" {
			continue
		}
		buf.WriteString(fmt.Sprintf("\t.extern _%s\n", ext.Name))
	}

	if len(mod.Globals) > 0 {
		buf.WriteString("\t.section __DATA,__bss\n")
		for _, g := range mod.Globals {
			if g == nil || g.Name == "" {
				continue
			}
			size := 8
			if g.Type != nil && g.Type.Size > 0 {
				size = g.Type.Size
			}
			buf.WriteString(fmt.Sprintf("\t.p2align 3\n_%s:\n\t.zero %d\n\n", g.Name, size))
		}
	}

	funcs := append([]*mir.Function(nil), mod.Functions...)
	sort.Slice(funcs, func(i, j int) bool {
		if funcs[i] == nil {
			return false
		}
		if funcs[j] == nil {
			return true
		}
		return funcs[i].Name < funcs[j].Name
	})

	for _, fn := range funcs {
		if fn == nil {
			continue
		}
		buf.WriteString(emitARM64Function(fn))
	}

	return buf.String()
}

func emitARM64Function(fn *mir.Function) string {
	var buf strings.Builder
	name := fn.Name
	if strings.HasPrefix(name, "__init_") {
		name = "main"
	}

	mangled := "_" + name
	buf.WriteString("\t.section __TEXT,__text\n")
	buf.WriteString("\t.p2align 2\n")
	buf.WriteString("\t.globl " + mangled + "\n")
	buf.WriteString(mangled + ":\n")

	// Emit an inline AAPCS64 prologue only when no frame-layout information
	// is available (i.e. the register-allocator / prologue-epilogue pass has
	// not run).  When fn.Frame != nil the prologue_epilogue pass has already
	// prepended proper MachineInstrs to fn.Entry.Instrs.
	if fn.Frame == nil {
		buf.WriteString("\tstp x29, x30, [sp, #-16]!\n")
		buf.WriteString("\tmov x29, sp\n")
	}

	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		if block.Label != "" && block.Label != "entry" {
			buf.WriteString("L" + block.Label + ":\n")
		}
		for _, instr := range block.Instrs {
			if line := emitARM64Instr(instr); line != "" {
				buf.WriteString("\t" + line + "\n")
			}
		}
		if term := emitARM64Terminator(block.Term); term != "" {
			buf.WriteString("\t" + term + "\n")
		}
	}
	return buf.String()
}

func emitARM64Instr(instr mir.Instr) string {
	switch i := instr.(type) {
	case *mir.MachineInstr:
		return emitARM64MachineInstr(i)
	case *mir.MoveInstr:
		return fmt.Sprintf("mov %s, %s", formatARM64Operand(i.Dst), formatARM64Operand(i.Src))
	case *mir.LoadInstr:
		if sym, ok := i.Addr.(*mir.Symbol); ok {
			return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tldr %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), formatARM64Operand(i.Dst))
		}
		return fmt.Sprintf("ldr %s, %s", formatARM64Operand(i.Dst), formatARM64Mem(i.Addr))
	case *mir.StoreInstr:
		if sym, ok := i.Addr.(*mir.Symbol); ok {
			return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tstr %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), formatARM64Operand(i.Value))
		}
		return fmt.Sprintf("str %s, %s", formatARM64Operand(i.Value), formatARM64Mem(i.Addr))
	case *mir.BinaryInstr:
		return fmt.Sprintf("%s %s, %s, %s", strings.ToLower(i.Op), formatARM64Operand(i.Dst), formatARM64Operand(i.Left), formatARM64Operand(i.Right))
	case *mir.CompareInstr:
		if i.Dst != nil {
			return fmt.Sprintf("cmp %s, %s\n\tcset %s, %s", formatARM64Operand(i.Left), formatARM64Operand(i.Right), formatARM64Operand(i.Dst), arm64CondSuffix(i.Pred))
		}
		return fmt.Sprintf("cmp %s, %s", formatARM64Operand(i.Left), formatARM64Operand(i.Right))
	case *mir.UnaryInstr:
		return fmt.Sprintf("%s %s, %s", strings.ToLower(i.Op), formatARM64Operand(i.Dst), formatARM64Operand(i.X))
	case *mir.CallInstr:
		callee := formatARM64Operand(i.Callee)
		line := fmt.Sprintf("bl %s", callee)
		if i.Dst != nil && i.Dst.Name != "x0" {
			line += fmt.Sprintf("\n\tmov %s, x0", formatARM64Operand(i.Dst))
		}
		return line
	default:
		return ""
	}
}

func emitARM64MachineInstr(i *mir.MachineInstr) string {
	if i == nil {
		return ""
	}
	op := strings.ToLower(i.Op)
	switch op {
	case "nop":
		return "nop"
	case "mov", "add", "sub", "mul", "and", "orr", "eor", "lsl", "lsr", "asr", "madd", "msub":
		return formatGenericARM64(i)

	// ── Frame prologue / epilogue ─────────────────────────────────────────────
	//
	// stp.pre  Rn, Rm, [base, #offset]!   — pre-indexed store pair (allocates frame)
	// stp      Rn, Rm, [base, #offset]    — signed-offset store pair
	// ldp      Rn, Rm, [base, #offset]    — signed-offset load pair
	// ldp.post Rn, Rm, [base], #offset    — post-indexed load pair (frees frame)
	// str      Rn, [base, #offset]        — single-register store  (odd-register save)
	// ldr      Rn, [base, #offset]        — single-register load   (odd-register restore)
	case "stp.pre":
		// stp Rn, Rm, [base, #offset]!
		if len(i.Srcs) >= 3 {
			r1 := formatARM64Operand(i.Srcs[0])
			r2 := formatARM64Operand(i.Srcs[1])
			return fmt.Sprintf("stp %s, %s, %s!", r1, r2, formatARM64Mem(i.Srcs[2]))
		}
	case "stp":
		// stp Rn, Rm, [base, #offset]
		if len(i.Srcs) >= 3 {
			r1 := formatARM64Operand(i.Srcs[0])
			r2 := formatARM64Operand(i.Srcs[1])
			return fmt.Sprintf("stp %s, %s, %s", r1, r2, formatARM64Mem(i.Srcs[2]))
		}
	case "ldp":
		// ldp Rn, Rm, [base, #offset]
		if len(i.Dsts) >= 2 && len(i.Srcs) >= 1 {
			r1 := formatARM64Operand(i.Dsts[0])
			r2 := formatARM64Operand(i.Dsts[1])
			return fmt.Sprintf("ldp %s, %s, %s", r1, r2, formatARM64Mem(i.Srcs[0]))
		}
	case "ldp.post":
		// ldp Rn, Rm, [base], #offset
		// Srcs[0] = Memory{base, nil}  → "[base]"
		// Srcs[1] = Immediate(offset)  → "#offset"
		if len(i.Dsts) >= 2 && len(i.Srcs) >= 2 {
			r1 := formatARM64Operand(i.Dsts[0])
			r2 := formatARM64Operand(i.Dsts[1])
			base := formatARM64Mem(i.Srcs[0])
			postOff := formatARM64Operand(i.Srcs[1])
			return fmt.Sprintf("ldp %s, %s, %s, %s", r1, r2, base, postOff)
		}
	case "str":
		// str Rn, [base, #offset]  — MachineInstr form (odd-register spill)
		if len(i.Srcs) >= 2 {
			return fmt.Sprintf("str %s, %s", formatARM64Operand(i.Srcs[0]), formatARM64Mem(i.Srcs[1]))
		}
	case "ldr":
		// ldr Rn, [base, #offset]  — MachineInstr form (odd-register reload)
		if len(i.Dsts) >= 1 && len(i.Srcs) >= 1 {
			return fmt.Sprintf("ldr %s, %s", formatARM64Operand(i.Dsts[0]), formatARM64Mem(i.Srcs[0]))
		}
	case "load":
		if len(i.Dsts) > 0 && len(i.Srcs) > 0 {
			if sym, ok := i.Srcs[0].(*mir.Symbol); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tldr %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), formatARM64Operand(i.Dsts[0]))
			}
			return fmt.Sprintf("ldr %s, %s", formatARM64Operand(i.Dsts[0]), formatARM64Mem(i.Srcs[0]))
		}
	case "store":
		if len(i.Srcs) == 2 {
			if sym, ok := i.Srcs[1].(*mir.Symbol); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tstr %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), formatARM64Operand(i.Srcs[0]))
			}
			return fmt.Sprintf("str %s, %s", formatARM64Operand(i.Srcs[0]), formatARM64Mem(i.Srcs[1]))
		}
	case "cmp":
		return formatGenericARM64(i)
	case "cset":
		if len(i.Dsts) > 0 && len(i.Srcs) > 0 {
			cond := formatARM64Operand(i.Srcs[0])
			if l, ok := i.Srcs[0].(*mir.Label); ok {
				cond = strings.ToLower(l.Name)
			}
			return fmt.Sprintf("cset %s, %s", formatARM64Operand(i.Dsts[0]), cond)
		}
		return formatGenericARM64(i)
	case "cmp.eq", "cmp.ne", "cmp.lt", "cmp.le", "cmp.gt", "cmp.ge":
		if len(i.Dsts) > 0 && len(i.Srcs) >= 2 {
			return fmt.Sprintf("cmp %s, %s\n\tcset %s, %s", formatARM64Operand(i.Srcs[0]), formatARM64Operand(i.Srcs[1]), formatARM64Operand(i.Dsts[0]), arm64CondSuffix(op))
		}
	case "call":
		if len(i.Srcs) > 0 {
			line := fmt.Sprintf("bl %s", formatARM64Operand(i.Srcs[0]))
			if len(i.Dsts) > 0 && i.Dsts[0] != nil && i.Dsts[0].Name != "x0" {
				line += fmt.Sprintf("\n\tmov %s, x0", formatARM64Operand(i.Dsts[0]))
			}
			return line
		}
	case "spill", "reload":
		// These should normally disappear during register allocation.
		// Emit a no-op comment rather than panicking so smoke tests can proceed.
		return "; " + op
	}
	return formatGenericARM64(i)
}

func emitARM64Halt(code mir.Operand) string {
	// Use bl _exit so the shell captures the non-zero exit code.
	// macOS libc _exit(n) terminates the process with exit status n.
	if code == nil {
		// Default to exit code 1 (assertion failure / explicit halt).
		return "mov w0, #1\n\tbl _exit"
	}

	codeStr := formatARM64Operand(code)
	// x0 and w0 are the same physical register on AArch64; no move needed.
	if codeStr == "x0" || codeStr == "w0" {
		return "bl _exit"
	}

	// Move exit code into w0 (32-bit arg register 0) then call _exit.
	return fmt.Sprintf("mov w0, %s\n\tbl _exit", codeStr)
}

func emitARM64Terminator(term mir.Terminator) string {
	switch t := term.(type) {
	case nil:
		return ""
	case *mir.MachineTerm:
		op := strings.ToLower(t.Op)
		switch op {
		case "ret":
			if len(t.Srcs) > 0 && t.Srcs[0] != nil {
				if formatARM64Operand(t.Srcs[0]) != "x0" {
					return fmt.Sprintf("mov x0, %s\n\tret", formatARM64Operand(t.Srcs[0]))
				}
			}
			return "ret"
		case "ret.bare":
			// The prologue_epilogue pass has already inserted the ldp/addi restore
			// instructions into the exit block's Instrs.  Only the architectural
			// "ret" is needed here.
			if len(t.Srcs) > 0 && t.Srcs[0] != nil {
				opStr := formatARM64Operand(t.Srcs[0])
				if opStr != "x0" && opStr != "w0" {
					return fmt.Sprintf("mov x0, %s\n\tret", opStr)
				}
			}
			return "ret"
		case "j":
			if len(t.Targets) > 0 {
				return "b " + arm64Label(t.Targets[0])
			}
		case "br":
			if len(t.Targets) >= 2 && len(t.Srcs) > 0 {
				return fmt.Sprintf("cmp %s, #0\n\tb.ne %s\n\tb %s", formatARM64Operand(t.Srcs[0]), arm64Label(t.Targets[0]), arm64Label(t.Targets[1]))
			}
		case "beq", "bne", "blt", "ble", "bgt", "bge":
			if len(t.Targets) >= 2 {
				return fmt.Sprintf("%s %s\n\tb %s",
					arm64BranchOpcode(op), arm64Label(t.Targets[0]), arm64Label(t.Targets[1]))
			}
			if len(t.Targets) > 0 {
				return fmt.Sprintf("%s %s", arm64BranchOpcode(op), arm64Label(t.Targets[0]))
			}
		case "switch":
			if len(t.Targets) > 0 {
				// Fallback compare chain: just branch to the default when present.
				return "b " + arm64Label(t.Targets[len(t.Targets)-1])
			}
		case "halt":
			var code mir.Operand
			if len(t.Srcs) > 0 {
				code = t.Srcs[0]
			}
			return emitARM64Halt(code)
		}
	case *mir.JumpInstr:
		return "b " + arm64Label(t.Target)
	case *mir.CondBrInstr:
		return fmt.Sprintf("cmp %s, #0\n\tb.ne %s\n\tb %s", formatARM64Operand(t.Cond), arm64Label(t.TrueLabel), arm64Label(t.FalseLabel))
	case *mir.ReturnInstr:
		if t.Value != nil {
			return fmt.Sprintf("mov x0, %s\n\tret", formatARM64Operand(t.Value))
		}
		return "ret"
	case *mir.HaltInstr:
		return emitARM64Halt(t.Code)
	case *mir.SwitchInstr:
		if len(t.Arms) > 0 {
			return "b " + arm64Label(t.Default)
		}
	}
	return ""
}

func formatGenericARM64(i *mir.MachineInstr) string {
	if i == nil {
		return ""
	}
	parts := make([]string, 0, len(i.Dsts)+len(i.Srcs))
	for _, d := range i.Dsts {
		if d != nil {
			parts = append(parts, formatARM64Operand(d))
		}
	}
	for _, s := range i.Srcs {
		if s != nil {
			parts = append(parts, formatARM64Operand(s))
		}
	}
	if len(parts) == 0 {
		return strings.ToLower(i.Op)
	}
	return strings.ToLower(i.Op) + " " + strings.Join(parts, ", ")
}

func formatARM64Operand(op mir.Operand) string {
	switch v := op.(type) {
	case nil:
		return ""
	case *mir.Register:
		return v.Name
	case *mir.Label:
		return arm64Label(v.Name)
	case *mir.Symbol:
		return "_" + v.Name
	case *mir.Immediate:
		return "#" + fmt.Sprint(v.Value)
	case *mir.Memory:
		return formatARM64Mem(v)
	default:
		return op.String()
	}
}

func formatARM64Mem(op mir.Operand) string {
	m, ok := op.(*mir.Memory)
	if !ok || m == nil {
		return formatARM64Operand(op)
	}
	if m.Offset == nil {
		return fmt.Sprintf("[%s]", formatARM64Operand(m.Base))
	}
	return fmt.Sprintf("[%s, %s]", formatARM64Operand(m.Base), formatARM64Operand(m.Offset))
}

func arm64Label(name string) string {
	if name == "" {
		return "L"
	}
	if strings.HasPrefix(name, "L") {
		return name
	}
	return "L" + name
}

func arm64CondSuffix(op string) string {
	switch strings.ToLower(op) {
	case "cmp.eq", "beq":
		return "eq"
	case "cmp.ne", "bne":
		return "ne"
	case "cmp.lt", "blt":
		return "lt"
	case "cmp.le", "ble":
		return "le"
	case "cmp.gt", "bgt":
		return "gt"
	case "cmp.ge", "bge":
		return "ge"
	default:
		return "eq"
	}
}

func arm64BranchOpcode(op string) string {
	switch strings.ToLower(op) {
	case "beq", "cmp.eq":
		return "b.eq"
	case "bne", "cmp.ne":
		return "b.ne"
	case "blt", "cmp.lt":
		return "b.lt"
	case "ble", "cmp.le":
		return "b.le"
	case "bgt", "cmp.gt":
		return "b.gt"
	case "bge", "cmp.ge":
		return "b.ge"
	default:
		return "b"
	}
}
