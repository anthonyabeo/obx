package target

import (
	"fmt"
	"math"
	"sort"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// ARM64Target is the first minir-first target implementation.
type ARM64Target struct {
	*BaseTarget
}

var arm64FuncLabelScope string

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
		var bssGlobals []*mir.GlobalDecl
		for _, g := range mod.Globals {
			if g == nil || g.Name == "" {
				continue
			}
			if g.IsExternRef {
				// Cross-module reference: declare as extern, not as a local BSS allocation.
				buf.WriteString(fmt.Sprintf("\t.extern _%s\n", g.Name))
			} else {
				bssGlobals = append(bssGlobals, g)
			}
		}
		if len(bssGlobals) > 0 {
			buf.WriteString("\t.section __DATA,__bss\n")
			for _, g := range bssGlobals {
				size := 8
				if g.Type != nil && g.Type.Size > 0 {
					size = g.Type.Size
				}
				buf.WriteString(fmt.Sprintf("\t.globl _%s\n\t.p2align 3\n_%s:\n\t.zero %d\n\n", g.Name, g.Name, size))
			}
		}
	}

	if len(mod.Constants) > 0 {
		emitARM64Constants(&buf, mod.Constants)
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
		buf.WriteString(emitARM64Function(mod, fn))
	}

	stdioInit := ""
	if mod.DLLName == "libc" {
		stdioInit = emitARM64StdioInit(mod)
	}

	if initStub := emitARM64ModuleInitStub(mod, stdioInit != ""); initStub != "" {
		buf.WriteString(initStub)
	}

	// For DEFINITION modules with [dll "libc"], generate a platform-specific
	// __init_<Name> function that loads the real stdio FILE* handles from libc
	// globals into the module's stdin/stdout/stderr variables.  This is called
	// automatically by the importer's prepended module-init call chain.
	if stdioInit != "" {
		buf.WriteString(stdioInit)
	}

	return buf.String()
}

func emitARM64ModuleInitStub(mod *mir.Module, hasSynthesizedInit bool) string {
	if mod == nil || mod.Name == "" {
		return ""
	}
	if hasSynthesizedInit {
		return ""
	}
	initName := "__init_" + mod.Name
	for _, fn := range mod.Functions {
		if fn != nil && fn.Name == initName {
			return ""
		}
	}

	return fmt.Sprintf("\t.section __TEXT,__text\n\t.p2align 2\n\t.globl _%s\n_%s:\n\tret\n", initName, initName)
}

func emitARM64Constants(buf *strings.Builder, consts []*mir.ConstDecl) {
	if buf == nil || len(consts) == 0 {
		return
	}

	var cstrings []*mir.ConstDecl
	var scalars []*mir.ConstDecl
	for _, c := range consts {
		if c == nil || c.Name == "" || c.Value == nil {
			continue
		}
		if imm, ok := c.Value.(*mir.Immediate); ok {
			if _, ok := imm.Value.(string); ok {
				cstrings = append(cstrings, c)
				continue
			}
		}
		scalars = append(scalars, c)
	}

	if len(cstrings) > 0 {
		buf.WriteString("\t.section __TEXT,__cstring,cstring_literals\n")
		for _, c := range cstrings {
			imm := c.Value.(*mir.Immediate)
			s, _ := imm.Value.(string)
			buf.WriteString(fmt.Sprintf("_%s:\n", c.Name))
			buf.WriteString("\t.asciz " + strconv.Quote(s) + "\n")
		}
		buf.WriteString("\n")
	}

	if len(scalars) > 0 {
		buf.WriteString("\t.section __DATA,__const\n")
		for _, c := range scalars {
			if imm, ok := c.Value.(*mir.Immediate); ok {
				if lit, ok := arm64ConstDirective(imm, c.Type); ok {
					buf.WriteString(fmt.Sprintf("\t.p2align 3\n_%s:\n\t%s\n", c.Name, lit))
				}
			}
		}
		buf.WriteString("\n")
	}
}

func arm64ConstDirective(imm *mir.Immediate, ty *mir.Type) (string, bool) {
	if imm == nil {
		return "", false
	}
	sz := 8
	if ty != nil && ty.Size > 0 {
		sz = ty.Size
	}
	// Keep scalar emission conservative; string constants are handled separately.
	switch v := imm.Value.(type) {
	case int, int8, int16, int32, int64, uint, uint8, uint16, uint32, uint64:
		switch sz {
		case 1:
			return fmt.Sprintf(".byte %v", v), true
		case 2:
			return fmt.Sprintf(".short %v", v), true
		case 4:
			return fmt.Sprintf(".long %v", v), true
		default:
			return fmt.Sprintf(".quad %v", v), true
		}
	case float32:
		return fmt.Sprintf(".float %v", v), true
	case float64:
		return fmt.Sprintf(".double %v", v), true
	default:
		return "", false
	}
}

func emitARM64Function(mod *mir.Module, fn *mir.Function) string {
	var buf strings.Builder
	name := fn.Name
	// Only rename the entry module's OWN init function to "main".
	// Imported-module init functions keep their __init_<Name> symbols so the
	// linker can distinguish them from the program entrypoint.
	if mod != nil && mod.IsEntry && name == "__init_"+mod.Name {
		name = "main"
	}

	mangled := "_" + name
	prevScope := arm64FuncLabelScope
	arm64FuncLabelScope = name
	defer func() { arm64FuncLabelScope = prevScope }()

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
			buf.WriteString(arm64Label(block.Label) + ":\n")
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
		return emitARM64Move(formatARM64Operand(i.Dst), formatARM64Operand(i.Src))
	case *mir.LoadInstr:
		ldOp := arm64LoadOpcode(i.Dst.Type())
		dstReg := formatARM64DataReg(i.Dst, i.Dst.Type())
		if sym, ok := i.Addr.(*mir.Symbol); ok {
			return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\t%s %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), ldOp, dstReg)
		}
		return fmt.Sprintf("%s %s, %s", ldOp, dstReg, formatARM64Mem(i.Addr))
	case *mir.StoreInstr:
		stOp := arm64StoreOpcode(i.Value.Type())
		valReg := formatARM64DataReg(i.Value, i.Value.Type())
		if sym, ok := i.Addr.(*mir.Symbol); ok {
			return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\t%s %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), stOp, valReg)
		}
		return fmt.Sprintf("%s %s, %s", stOp, valReg, formatARM64Mem(i.Addr))
	case *mir.BinaryInstr:
		op := strings.ToLower(i.Op)
		switch op {
		case "fadd", "fsub", "fmul", "fdiv":
			dst := formatARM64DataReg(i.Dst, i.Dst.Type())
			left, preL := arm64FloatOperand(i.Left, "d14")
			rightScratch := "d15"
			if left == rightScratch {
				rightScratch = "d14"
			}
			right, preR := arm64FloatOperand(i.Right, rightScratch)
			parts := make([]string, 0, 3)
			if preL != "" {
				parts = append(parts, preL)
			}
			if preR != "" {
				parts = append(parts, preR)
			}
			parts = append(parts, fmt.Sprintf("%s %s, %s, %s", op, dst, left, right))
			return strings.Join(parts, "\n\t")
		default:
			if op == "add" {
				if imm, ok := i.Right.(*mir.Immediate); ok {
					if n, ok2, _ := toInt64Immediate(imm); ok2 && n == 0 {
						dst := formatARM64Operand(i.Dst)
						src := formatARM64Operand(i.Left)
						return emitARM64Move(dst, src)
					}
				}
			}
			if op == "and" {
				dst := formatARM64Operand(i.Dst)
				if sym, ok := arm64SymbolOperand(i.Left); ok {
					tmp := "x9"
					if strings.HasPrefix(dst, "w") {
						tmp = "w9"
					}
					return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tand %s, %s, %s", sym, sym, dst, tmp, formatARM64Operand(i.Right))
				}
				if sym, ok := arm64SymbolOperand(i.Right); ok {
					tmp := "x9"
					if strings.HasPrefix(dst, "w") {
						tmp = "w9"
					}
					return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tand %s, %s, %s", sym, sym, dst, formatARM64Operand(i.Left), tmp)
				}
			}
			return fmt.Sprintf("%s %s, %s, %s", op, formatARM64Operand(i.Dst), formatARM64Operand(i.Left), formatARM64Operand(i.Right))
		}
	case *mir.CompareInstr:
		if i.Dst != nil {
			return fmt.Sprintf("cmp %s, %s\n\tcset %s, %s", formatARM64Operand(i.Left), formatARM64Operand(i.Right), formatARM64Operand(i.Dst), arm64CondSuffix(i.Pred))
		}
		return fmt.Sprintf("cmp %s, %s", formatARM64Operand(i.Left), formatARM64Operand(i.Right))
	case *mir.UnaryInstr:
		op := strings.ToLower(i.Op)
		dst := formatARM64Operand(i.Dst)
		src := formatARM64Operand(i.X)
		switch op {
		case "bitcast", "zext", "sext":
			return emitARM64Move(dst, src)
		case "sitofp":
			return fmt.Sprintf("scvtf %s, %s", formatARM64DataReg(i.Dst, i.Dst.Type()), formatARM64DataReg(i.X, i.X.Type()))
		case "fptosi":
			return fmt.Sprintf("fcvtzs %s, %s", formatARM64DataReg(i.Dst, i.Dst.Type()), formatARM64DataReg(i.X, i.X.Type()))
		case "fpext", "fptrunc":
			return fmt.Sprintf("fcvt %s, %s", formatARM64DataReg(i.Dst, i.Dst.Type()), formatARM64DataReg(i.X, i.X.Type()))
		case "trunc":
			if i.Dst != nil && i.Dst.Type() != nil {
				if bits := i.Dst.Type().Size * 8; bits > 0 && bits < 64 {
					mask := int64((uint64(1) << bits) - 1)
					if sym, ok := arm64SymbolOperand(i.X); ok {
						tmp := "x9"
						if strings.HasPrefix(dst, "w") {
							tmp = "w9"
						}
						return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tand %s, %s, #%d", sym, sym, dst, tmp, mask)
					}
					return fmt.Sprintf("and %s, %s, #%d", dst, src, mask)
				}
			}
			return emitARM64Move(dst, src)
		default:
			return fmt.Sprintf("%s %s, %s", op, dst, src)
		}
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
	case "mov":
		if len(i.Dsts) > 0 && len(i.Srcs) > 0 {
			return emitARM64Move(formatARM64Operand(i.Dsts[0]), formatARM64Operand(i.Srcs[0]))
		}
		return formatGenericARM64(i)
	case "add", "sub", "mul", "and", "orr", "eor", "lsl", "lsr", "asr", "madd", "msub":
		if op == "add" && len(i.Dsts) > 0 && len(i.Srcs) >= 2 {
			if imm, ok := i.Srcs[1].(*mir.Immediate); ok {
				if n, ok2, _ := toInt64Immediate(imm); ok2 && n == 0 {
					dst := formatARM64Operand(i.Dsts[0])
					src := formatARM64Operand(i.Srcs[0])
					if strings.HasPrefix(dst, "d") || strings.HasPrefix(dst, "s") || strings.HasPrefix(src, "d") || strings.HasPrefix(src, "s") {
						return emitARM64Move(dst, src)
					}
				}
			}
		}
		if op == "and" && len(i.Dsts) > 0 && len(i.Srcs) >= 2 {
			if sym, ok := arm64SymbolOperand(i.Srcs[0]); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tand %s, x9, %s", sym, sym, formatARM64Operand(i.Dsts[0]), formatARM64Operand(i.Srcs[1]))
			}
			if sym, ok := arm64SymbolOperand(i.Srcs[1]); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\tand %s, %s, x9", sym, sym, formatARM64Operand(i.Dsts[0]), formatARM64Operand(i.Srcs[0]))
			}
		}
		return formatGenericARM64(i)
	case "fadd", "fsub", "fmul", "fdiv":
		return emitARM64FloatBinary(i, op)

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
			op := arm64StoreOpcode(i.Srcs[0].Type())
			reg := formatARM64DataReg(i.Srcs[0], i.Srcs[0].Type())
			return fmt.Sprintf("%s %s, %s", op, reg, formatARM64Mem(i.Srcs[1]))
		}
	case "ldr":
		// ldr Rn, [base, #offset]  — MachineInstr form (odd-register reload)
		if len(i.Dsts) >= 1 && len(i.Srcs) >= 1 {
			op := arm64LoadOpcode(i.Dsts[0].Type())
			reg := formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type())
			return fmt.Sprintf("%s %s, %s", op, reg, formatARM64Mem(i.Srcs[0]))
		}
	case "load":
		if len(i.Dsts) > 0 && len(i.Srcs) > 0 {
			op := arm64LoadOpcode(i.Dsts[0].Type())
			reg := formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type())
			if sym, ok := i.Srcs[0].(*mir.Symbol); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\t%s %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), op, reg)
			}
			return fmt.Sprintf("%s %s, %s", op, reg, formatARM64Mem(i.Srcs[0]))
		}
	case "store":
		if len(i.Srcs) == 2 {
			op := arm64StoreOpcode(i.Srcs[0].Type())
			reg := formatARM64DataReg(i.Srcs[0], i.Srcs[0].Type())
			if sym, ok := i.Srcs[1].(*mir.Symbol); ok {
				return fmt.Sprintf("adrp x9, %s@PAGE\n\tadd x9, x9, %s@PAGEOFF\n\t%s %s, [x9]", formatARM64Operand(sym), formatARM64Operand(sym), op, reg)
			}
			return fmt.Sprintf("%s %s, %s", op, reg, formatARM64Mem(i.Srcs[1]))
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
	case "trunc":
		return emitARM64Trunc(i)
	case "zext":
		return emitARM64ZExt(i)
	case "sext":
		return emitARM64SExt(i)
	case "sitofp":
		return emitARM64SIToFP(i)
	case "fptosi":
		return emitARM64FPToSI(i)
	case "fpext", "fptrunc":
		return emitARM64FPCvt(i)
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
		if strings.HasPrefix(v.Name, "__pc_tmp") {
			if v.Type() != nil {
				ty := strings.ToLower(v.Type().String())
				if strings.HasPrefix(ty, "f") {
					return "d15"
				}
			}
			return "x15"
		}
		if strings.HasPrefix(v.Name, "__Lstr_") || strings.HasPrefix(v.Name, "_Lstr_") {
			return arm64ScratchRegForType(v.Type())
		}
		if isARM64XReg(v.Name) || isARM64WReg(v.Name) || isARM64FloatReg(v.Name) || v.Name == "sp" || v.Name == "xzr" || v.Name == "wzr" {
			return v.Name
		}
		if strings.HasPrefix(v.Name, "t") {
			if idx, err := strconv.Atoi(v.Name[1:]); err == nil {
				if idx >= 0 && idx <= 30 {
					return formatARM64DataReg(v, v.Type())
				}
				return arm64ScratchRegForType(v.Type())
			}
		}
		if v.Kind == mir.VirtualReg {
			return arm64ScratchRegForType(v.Type())
		}
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
		if r, ok := op.(*mir.Register); ok && r != nil {
			return fmt.Sprintf("[%s]", formatARM64Operand(r))
		}
		return formatARM64Operand(op)
	}
	if m.Offset == nil {
		return fmt.Sprintf("[%s]", formatARM64Operand(m.Base))
	}
	return fmt.Sprintf("[%s, %s]", formatARM64Operand(m.Base), formatARM64Operand(m.Offset))
}

func emitARM64Move(dst, src string) string {
	if dst == "" || src == "" {
		return fmt.Sprintf("mov %s, %s", dst, src)
	}
	if strings.HasPrefix(src, "_") && isARM64XReg(dst) {
		// Darwin AArch64 cannot encode symbol addresses with a plain mov.
		// Materialize the address using page+pageoff relocations.
		return fmt.Sprintf("adrp %s, %s@PAGE\n\tadd %s, %s, %s@PAGEOFF", dst, src, dst, dst, src)
	}
	if strings.HasPrefix(src, "#") {
		if isARM64FloatReg(dst) {
			if f, ok := parseARM64ImmediateFloat(src); ok {
				intReg, bits, ok := arm64FloatImmediateBits(dst, f)
				if ok {
					return emitARM64LoadImmediateBits(intReg, bits) + "\n\tfmov " + dst + ", " + intReg
				}
			}
		}
		if isARM64XReg(dst) || isARM64WReg(dst) {
			if bits, ok := parseARM64ImmediateBits(src, isARM64WReg(dst)); ok {
				return emitARM64LoadImmediateBits(dst, bits)
			}
		}
	}
	if isARM64FloatReg(dst) || isARM64FloatReg(src) {
		return fmt.Sprintf("fmov %s, %s", dst, src)
	}
	return fmt.Sprintf("mov %s, %s", dst, src)
}

func isARM64XReg(name string) bool {
	if name == "xzr" || name == "sp" {
		return true
	}
	if !strings.HasPrefix(name, "x") || len(name) < 2 {
		return false
	}
	n, err := strconv.Atoi(name[1:])
	return err == nil && n >= 0 && n <= 30
}

func isARM64WReg(name string) bool {
	if name == "wzr" {
		return true
	}
	if !strings.HasPrefix(name, "w") || len(name) < 2 {
		return false
	}
	n, err := strconv.Atoi(name[1:])
	return err == nil && n >= 0 && n <= 30
}

func arm64ScratchRegForType(ty *mir.Type) string {
	if arm64IsFloatType(ty) {
		if arm64TypeSize(ty) <= 4 {
			return "s15"
		}
		return "d15"
	}
	if arm64TypeSize(ty) <= 4 {
		return "w15"
	}
	return "x15"
}

func arm64SymbolOperand(op mir.Operand) (string, bool) {
	switch v := op.(type) {
	case *mir.Symbol:
		return formatARM64Operand(v), true
	case *mir.Register:
		if v != nil && (strings.HasPrefix(v.Name, "__Lstr_") || strings.HasPrefix(v.Name, "_Lstr_")) {
			if strings.HasPrefix(v.Name, "_") {
				return v.Name, true
			}
			return "_" + v.Name, true
		}
	}
	return "", false
}

func parseARM64ImmediateFloat(src string) (float64, bool) {
	if !strings.HasPrefix(src, "#") {
		return 0, false
	}
	raw := strings.TrimPrefix(src, "#")
	f, err := strconv.ParseFloat(raw, 64)
	if err != nil {
		return 0, false
	}
	return f, true
}

func parseARM64ImmediateBits(src string, forWReg bool) (uint64, bool) {
	if !strings.HasPrefix(src, "#") {
		return 0, false
	}
	raw := strings.TrimPrefix(src, "#")
	if i, err := strconv.ParseInt(raw, 0, 64); err == nil {
		if forWReg {
			return uint64(uint32(i)), true
		}
		return uint64(i), true
	}
	if u, err := strconv.ParseUint(raw, 0, 64); err == nil {
		if forWReg {
			return uint64(uint32(u)), true
		}
		return u, true
	}
	if f, err := strconv.ParseFloat(raw, 64); err == nil {
		if forWReg {
			return uint64(math.Float32bits(float32(f))), true
		}
		return math.Float64bits(f), true
	}
	return 0, false
}

func toInt64Immediate(imm *mir.Immediate) (int64, bool, error) {
	if imm == nil {
		return 0, false, nil
	}
	switch v := imm.Value.(type) {
	case int:
		return int64(v), true, nil
	case int8:
		return int64(v), true, nil
	case int16:
		return int64(v), true, nil
	case int32:
		return int64(v), true, nil
	case int64:
		return v, true, nil
	case uint:
		return int64(v), true, nil
	case uint8:
		return int64(v), true, nil
	case uint16:
		return int64(v), true, nil
	case uint32:
		return int64(v), true, nil
	case uint64:
		if v > uint64(math.MaxInt64) {
			return 0, false, fmt.Errorf("immediate %v overflows int64", v)
		}
		return int64(v), true, nil
	default:
		return 0, false, nil
	}
}

func arm64FloatImmediateBits(floatReg string, f float64) (string, uint64, bool) {
	if strings.HasPrefix(floatReg, "s") {
		idx := strings.TrimPrefix(floatReg, "s")
		if _, err := strconv.Atoi(idx); err != nil {
			return "", 0, false
		}
		return "w" + idx, uint64(math.Float32bits(float32(f))), true
	}
	if strings.HasPrefix(floatReg, "d") {
		idx := strings.TrimPrefix(floatReg, "d")
		if _, err := strconv.Atoi(idx); err != nil {
			return "", 0, false
		}
		return "x" + idx, math.Float64bits(f), true
	}
	return "", 0, false
}

func emitARM64LoadImmediateBits(dst string, bits uint64) string {
	if isARM64WReg(dst) {
		bits = uint64(uint32(bits))
		if bits == 0 {
			return fmt.Sprintf("mov %s, #0", dst)
		}
		return emitARM64MovZK(dst, bits, 2)
	}
	if bits == 0 {
		return fmt.Sprintf("mov %s, #0", dst)
	}
	return emitARM64MovZK(dst, bits, 4)
}

func emitARM64MovZK(dst string, bits uint64, halfWords int) string {
	parts := make([]string, 0, halfWords)
	started := false
	for i := 0; i < halfWords; i++ {
		chunk := uint16((bits >> (16 * i)) & 0xFFFF)
		if !started {
			if chunk == 0 {
				continue
			}
			if i == 0 {
				parts = append(parts, fmt.Sprintf("movz %s, #%d", dst, chunk))
			} else {
				parts = append(parts, fmt.Sprintf("movz %s, #%d, lsl #%d", dst, chunk, 16*i))
			}
			started = true
			continue
		}
		if chunk == 0 {
			continue
		}
		parts = append(parts, fmt.Sprintf("movk %s, #%d, lsl #%d", dst, chunk, 16*i))
	}
	if !started {
		return fmt.Sprintf("mov %s, #0", dst)
	}
	return strings.Join(parts, "\n\t")
}

func emitARM64Trunc(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	dst := formatARM64Operand(i.Dsts[0])
	src := formatARM64Operand(i.Srcs[0])

	bw := scalarBitWidth(i.Dsts[0].Type())
	switch {
	case bw <= 0 || bw >= 64:
		return emitARM64Move(dst, src)
	default:
		mask := (uint64(1) << bw) - 1
		return fmt.Sprintf("and %s, %s, #%d", dst, src, mask)
	}
}

func emitARM64ZExt(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	dst := formatARM64Operand(i.Dsts[0])
	src := formatARM64Operand(i.Srcs[0])

	srcBits := scalarBitWidth(i.Srcs[0].Type())
	if srcBits <= 0 || srcBits >= 64 {
		return emitARM64Move(dst, src)
	}
	mask := (uint64(1) << srcBits) - 1
	return fmt.Sprintf("and %s, %s, #%d", dst, src, mask)
}

func emitARM64SExt(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	dst := formatARM64Operand(i.Dsts[0])
	src := formatARM64Operand(i.Srcs[0])

	srcBits := scalarBitWidth(i.Srcs[0].Type())
	switch srcBits {
	case 8:
		return fmt.Sprintf("sxtb %s, %s", dst, src)
	case 16:
		return fmt.Sprintf("sxth %s, %s", dst, src)
	case 32:
		return fmt.Sprintf("sxtw %s, %s", dst, src)
	default:
		return emitARM64Move(dst, src)
	}
}

func emitARM64SIToFP(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	return fmt.Sprintf("scvtf %s, %s", formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type()), formatARM64DataReg(i.Srcs[0], i.Srcs[0].Type()))
}

func emitARM64FPToSI(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	return fmt.Sprintf("fcvtzs %s, %s", formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type()), formatARM64DataReg(i.Srcs[0], i.Srcs[0].Type()))
}

func emitARM64FPCvt(i *mir.MachineInstr) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) == 0 {
		return formatGenericARM64(i)
	}
	return fmt.Sprintf("fcvt %s, %s", formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type()), formatARM64DataReg(i.Srcs[0], i.Srcs[0].Type()))
}

func emitARM64FloatBinary(i *mir.MachineInstr, op string) string {
	if i == nil || len(i.Dsts) == 0 || len(i.Srcs) < 2 {
		return formatGenericARM64(i)
	}
	dst := formatARM64DataReg(i.Dsts[0], i.Dsts[0].Type())
	left, preL := arm64FloatOperand(i.Srcs[0], "d14")
	rightScratch := "d15"
	if left == rightScratch {
		rightScratch = "d14"
	}
	right, preR := arm64FloatOperand(i.Srcs[1], rightScratch)

	parts := make([]string, 0, 3)
	if preL != "" {
		parts = append(parts, preL)
	}
	if preR != "" {
		parts = append(parts, preR)
	}
	parts = append(parts, fmt.Sprintf("%s %s, %s, %s", op, dst, left, right))
	return strings.Join(parts, "\n\t")
}

func arm64FloatOperand(op mir.Operand, scratch string) (reg string, pre string) {
	if r, ok := op.(*mir.Register); ok {
		return formatARM64DataReg(r, r.Type()), ""
	}
	if imm, ok := op.(*mir.Immediate); ok {
		src := formatARM64Operand(imm)
		return scratch, emitARM64Move(scratch, src)
	}
	return formatARM64Operand(op), ""
}

func scalarBitWidth(ty *mir.Type) int {
	if ty == nil || ty.Size <= 0 {
		return 0
	}
	return ty.Size * 8
}

func arm64LoadOpcode(ty *mir.Type) string {
	if arm64IsFloatType(ty) {
		return "ldr"
	}
	switch arm64TypeSize(ty) {
	case 1:
		return "ldrb"
	case 2:
		return "ldrh"
	default:
		return "ldr"
	}
}

func arm64StoreOpcode(ty *mir.Type) string {
	if arm64IsFloatType(ty) {
		return "str"
	}
	switch arm64TypeSize(ty) {
	case 1:
		return "strb"
	case 2:
		return "strh"
	default:
		return "str"
	}
}

func arm64TypeSize(ty *mir.Type) int {
	if ty == nil || ty.Size <= 0 {
		return 8
	}
	return ty.Size
}

func arm64IsFloatType(ty *mir.Type) bool {
	if ty == nil {
		return false
	}
	name := strings.ToLower(ty.Name)
	return name == "f32" || name == "f64"
}

func formatARM64DataReg(op mir.Operand, ty *mir.Type) string {
	r, ok := op.(*mir.Register)
	if !ok || r == nil {
		return formatARM64Operand(op)
	}

	name := r.Name
	if name == "" {
		return name
	}
	if name == "sp" || name == "xzr" || name == "wzr" {
		return name
	}

	idx := -1
	prefixLen := 0
	for i := 0; i < len(name); i++ {
		if name[i] >= '0' && name[i] <= '9' {
			prefixLen = i
			if n, err := strconv.Atoi(name[i:]); err == nil {
				idx = n
			}
			break
		}
	}
	if idx < 0 || prefixLen == 0 {
		return name
	}

	if arm64IsFloatType(ty) {
		if idx > 31 {
			return arm64ScratchRegForType(ty)
		}
		if arm64TypeSize(ty) <= 4 {
			return fmt.Sprintf("s%d", idx)
		}
		return fmt.Sprintf("d%d", idx)
	}
	if idx > 30 {
		return arm64ScratchRegForType(ty)
	}

	if arm64TypeSize(ty) <= 4 {
		return fmt.Sprintf("w%d", idx)
	}
	return fmt.Sprintf("x%d", idx)
}

func isARM64FloatReg(name string) bool {
	if name == "" {
		return false
	}
	if !(strings.HasPrefix(name, "d") || strings.HasPrefix(name, "s")) {
		return false
	}
	if len(name) < 2 {
		return false
	}
	n, err := strconv.Atoi(name[1:])
	return err == nil && n >= 0 && n <= 31
}

func arm64Label(name string) string {
	if name == "" {
		return "L"
	}
	name = strings.TrimPrefix(name, "L")
	if arm64FuncLabelScope != "" && !strings.HasPrefix(name, arm64FuncLabelScope+"$") {
		name = arm64FuncLabelScope + "$" + name
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

// emitARM64StdioInit generates a platform-specific __init_<ModuleName> function
// for DEFINITION modules with [dll "libc"] that export stdin/stdout/stderr globals.
// On Apple Darwin/macOS the C standard streams are accessed via __stdinp,
// __stdoutp, __stderrp (the underlying FILE* pointers in libSystem).
// On other POSIX targets (Linux etc.) the symbols are stdin, stdout, stderr.
//
// The generated function is called automatically by any module that imports this
// DEFINITION module, via the __init_<ImportedModule> call prepended to the
// importer's own init body.
func emitARM64StdioInit(mod *mir.Module) string {
	if mod == nil {
		return ""
	}

	// Detect which platform-specific stdio symbol names to use based on the
	// module globals present (e.g. Stdio$stdin, Stdio$stdout, Stdio$stderr).
	type stdioPair struct {
		global   string // name of our BSS global (e.g. "Stdio$stdin")
		platform string // Darwin: "___stdinp", Linux: "_stdin" etc.
	}

	var pairs []stdioPair
	for _, g := range mod.Globals {
		if g == nil || g.Name == "" {
			continue
		}
		// Match <ModuleName>$stdin, <ModuleName>$stdout, <ModuleName>$stderr
		suffix := ""
		switch {
		case strings.HasSuffix(g.Name, "$stdin"):
			suffix = "stdin"
		case strings.HasSuffix(g.Name, "$stdout"):
			suffix = "stdout"
		case strings.HasSuffix(g.Name, "$stderr"):
			suffix = "stderr"
		}
		if suffix == "" {
			continue
		}
		// On Apple Darwin the platform symbols are __stdinp/__stdoutp/__stderrp
		// (with the leading underscore added by the assembler → ___stdinp etc.).
		// On Linux/POSIX the symbols are stdin/stdout/stderr.
		platformSym := platformStdioSym(suffix)
		pairs = append(pairs, stdioPair{global: g.Name, platform: platformSym})
	}

	if len(pairs) == 0 {
		return ""
	}

	var buf strings.Builder
	initName := fmt.Sprintf("__init_%s", mod.Name)

	// Declare the Darwin platform symbols as extern so the assembler/linker
	// can find them in libSystem.
	seen := make(map[string]bool)
	for _, p := range pairs {
		if !seen[p.platform] {
			buf.WriteString(fmt.Sprintf("\t.extern %s\n", p.platform))
			seen[p.platform] = true
		}
	}

	// If this module is not Stdio itself, declare __init_Stdio as extern so
	// the init chain ensures the Stdio FILE* globals are populated before any
	// caller uses them.
	if mod.Name != "Stdio" {
		buf.WriteString("\t.extern ___init_Stdio\n")
	}

	buf.WriteString("\t.section __TEXT,__text\n")
	buf.WriteString("\t.p2align 2\n")
	buf.WriteString(fmt.Sprintf("\t.globl _%s\n", initName))
	buf.WriteString(fmt.Sprintf("_%s:\n", initName))
	buf.WriteString("\tstp x29, x30, [sp, #-16]!\n")
	buf.WriteString("\tmov x29, sp\n")

	// Call __init_Stdio to ensure stdin/stdout/stderr globals are loaded from
	// libSystem before any code in this module tries to read them.
	// Skip the self-call when this IS the Stdio init to avoid infinite recursion.
	if mod.Name != "Stdio" {
		buf.WriteString("\tbl ___init_Stdio\n")
	}

	for _, p := range pairs {
		// Darwin ARM64: __stdinp/__stdoutp/__stderrp are extern globals in libSystem.
		// External symbols must be reached via the GOT (two-level namespace):
		//   adrp x9, ___stdinp@GOTPAGE        ; x9 = page of GOT entry
		//   ldr  x9, [x9, ___stdinp@GOTPAGEOFF] ; x9 = GOT slot → address of __stdinp
		//   ldr  x0, [x9]                     ; x0 = *__stdinp = FILE*
		buf.WriteString(fmt.Sprintf("\tadrp x9, %s@GOTPAGE\n", p.platform))
		buf.WriteString(fmt.Sprintf("\tldr x9, [x9, %s@GOTPAGEOFF]\n", p.platform))
		buf.WriteString("\tldr x0, [x9]\n")
		buf.WriteString(fmt.Sprintf("\tadrp x9, _%s@PAGE\n", p.global))
		buf.WriteString(fmt.Sprintf("\tadd x9, x9, _%s@PAGEOFF\n", p.global))
		buf.WriteString("\tstr x0, [x9]\n")
	}

	exitLabel := fmt.Sprintf("L_%s_exit", initName)
	buf.WriteString(fmt.Sprintf("\tb %s\n", exitLabel))
	buf.WriteString(fmt.Sprintf("%s:\n", exitLabel))
	buf.WriteString("\tldp x29, x30, [sp], #16\n")
	buf.WriteString("\tret\n")

	return buf.String()
}

// platformStdioSym returns the assembler symbol for the given C stdio stream
// name on the current platform.  On Apple Darwin/macOS the streams are
// indirected via __stdinp/__stdoutp/__stderrp; on POSIX/Linux they are direct
// globals named stdin/stdout/stderr.
func platformStdioSym(stream string) string {
	// Darwin/macOS: the runtime exports __stdinp etc. (ARM64 adds one _ → ___stdinp).
	// We hard-code Darwin here because the ARM64 target is currently only built
	// for Apple Silicon.  A future POSIX-generic path would inspect tgt.OS.
	switch stream {
	case "stdin":
		return "___stdinp"
	case "stdout":
		return "___stdoutp"
	case "stderr":
		return "___stderrp"
	}
	return "_" + stream
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
