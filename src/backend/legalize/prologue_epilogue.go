package legalize

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// EmitPrologueEpilogue inserts prologue and epilogue instructions based on frame layout.
// This should be called after register allocation when fn.Frame is populated.
// It operates on the MIR representation, inserting actual machine instructions.
func EmitPrologueEpilogue(prog *mir.Program, tgt target.Target) error {
	if prog == nil {
		return fmt.Errorf("EmitPrologueEpilogue: nil program")
	}
	if tgt == nil {
		return fmt.Errorf("EmitPrologueEpilogue: nil target")
	}

	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			if err := emitFunctionPrologueEpilogue(mod, fn, tgt); err != nil {
				return fmt.Errorf("function %s: %w", fn.Name, err)
			}
		}
	}

	return nil
}

// emitFunctionPrologueEpilogue emits prologue/epilogue for a single function.
func emitFunctionPrologueEpilogue(mod *mir.Module, fn *mir.Function, tgt target.Target) error {
	if fn == nil || fn.Frame == nil {
		// No frame layout; skip prologue/epilogue
		return nil
	}

	if fn.Entry == nil || fn.Exit == nil {
		return fmt.Errorf("function lacks entry or exit block")
	}

	abi := tgt.ABIInfo()
	if abi.WordSize == 0 {
		// No valid ABI information; skip prologue/epilogue
		return nil
	}

	// ── PROLOGUE ──────────────────────────────────────────────────────────────

	prologue := emitPrologueInstrs(fn, abi)
	if len(prologue) > 0 {
		// Insert prologue at the start of entry block
		fn.Entry.Instrs = append(prologue, fn.Entry.Instrs...)
	}

	// ── EPILOGUE ──────────────────────────────────────────────────────────────

	epilogue := emitEpilogueInstrs(fn, abi)
	if len(epilogue) > 0 {
		out := append([]mir.Instr(nil), fn.Exit.Instrs...)

		// Replace ReturnInstr / MachineTerm "ret" with a bare "ret.bare" so the
		// ARM64 emitter does not re-emit an inline ldp… ret. The epilogue
		// instructions above have already taken care of the frame teardown.
		switch term := fn.Exit.Term.(type) {
		case *mir.ReturnInstr:
			if term.Value != nil {
				out = append(out, mir.NewMachineInstr("mov",
					[]*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, nil)},
					[]mir.Operand{term.Value},
				))
			} else if mod != nil && mod.IsEntry && fn.Name == "__init_"+mod.Name {
				// Only the entry module's own __init_<Name> function maps to C
				// _main.  The OS reads x0 as the process exit code, so we must
				// return 0 for a clean exit.  Append a "mov x0, #0" before the
				// ret so that any leftover value in x0 does not cause a spurious
				// failure.
				out = append(out, mir.NewMachineInstr("mov",
					[]*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, nil)},
					[]mir.Operand{mir.NewImmediate(0, nil)},
				))
			}
			fn.Exit.Term = mir.NewMachineTerm("ret.bare", nil, nil)
		case *mir.MachineTerm:
			if strings.EqualFold(term.Op, "ret") {
				if len(term.Srcs) > 0 && term.Srcs[0] != nil {
					out = append(out, mir.NewMachineInstr("mov",
						[]*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, nil)},
						[]mir.Operand{term.Srcs[0]},
					))
				}
				// For void __init_ functions (which become C _main), ensure x0=0
				// so the OS sees a clean exit code rather than a leftover value.
				if len(term.Srcs) == 0 && mod != nil && mod.IsEntry && fn.Name == "__init_"+mod.Name {
					out = append(out, mir.NewMachineInstr("mov",
						[]*mir.Register{mir.NewRegister("x0", mir.PhysicalReg, nil)},
						[]mir.Operand{mir.NewImmediate(0, nil)},
					))
				}
				fn.Exit.Term = mir.NewMachineTerm("ret.bare", nil, nil)
			}
		}

		// Epilogue executes after any return-value move into x0 and before ret.bare.
		fn.Exit.Instrs = append(out, epilogue...)
	}

	return nil
}

// emitPrologueInstrs generates prologue instructions (frame allocation, register saves).
// This dispatches to target-specific implementations.
func emitPrologueInstrs(fn *mir.Function, abi target.ABI) []mir.Instr {
	var instrs []mir.Instr

	if fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return instrs // No frame to allocate
	}

	// Dispatch to target-specific prologue emission
	switch abi.Name {
	case "AAPCS64": // ARM64
		return emitARM64Prologue(fn, abi)
	case "LP64D": // RISC-V
		return emitRISCVPrologue(fn, abi)
	}

	return instrs
}

// emitEpilogueInstrs generates epilogue instructions (register restores, frame deallocation).
// Does not include the return instruction itself, which is handled by the exit block.
// This dispatches to target-specific implementations.
func emitEpilogueInstrs(fn *mir.Function, abi target.ABI) []mir.Instr {
	var instrs []mir.Instr

	if fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return instrs // No frame to deallocate
	}

	// Dispatch to target-specific epilogue emission
	switch abi.StackPointer {
	case "sp": // Both ARM64 and RISC-V use "sp", so check FramePointer
		switch abi.FramePointer {
		case "x29": // ARM64
			return emitARM64Epilogue(fn, abi)
		case "s0": // RISC-V
			return emitRISCVEpilogue(fn, abi)
		}
	}

	return instrs
}

// ── ARM64 AAPCS64 Prologue/Epilogue ────────────────────────────────────────
//
// ARM64 uses the ARM Architecture Procedure Call Standard (AAPCS64).
// The prologue allocates the frame and saves the callee-saved registers, especially
// the frame pointer (x29) and link register (x30).
//
// A typical prologue uses the "stp x29, x30, [sp, #-frameSize]!" instruction which
// simultaneously decrements sp, saves the pair, and is atomic for exception handling.
//
// The epilogue uses "ldp x29, x30, [sp], #frameSize!" for symmetric restoration.

func emitARM64Prologue(fn *mir.Function, abi target.ABI) []mir.Instr {
	if fn == nil || fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return nil
	}

	var instrs []mir.Instr
	var extra []string
	for _, r := range fn.Frame.SavedRegs {
		if r != "x29" && r != "x30" {
			extra = append(extra, r)
		}
	}
	frameSize := arm64EffectiveFrameSize(fn.Frame.TotalSize, len(extra))

	x29 := mir.NewRegister("x29", mir.PhysicalReg, nil)
	x30 := mir.NewRegister("x30", mir.PhysicalReg, nil)
	sp := mir.NewRegister("sp", mir.PhysicalReg, nil)
	negFrameSize := mir.NewImmediate(-frameSize, nil)

	// stp.pre x29, x30, [sp, #-frameSize]!
	// Pre-indexed: sp -= frameSize, then store x29 at [sp+0], x30 at [sp+8].
	// This is atomic for async-unwind purposes (AAPCS64 §6.2.3).
	instrs = append(instrs, mir.NewMachineInstr("stp.pre",
		[]*mir.Register{},
		[]mir.Operand{x29, x30, mir.NewMemory(sp, negFrameSize, nil)},
	))

	// Establish frame pointer at the caller-SP equivalent (sp + frameSize).
	// The allocator uses negative offsets from FP; anchoring FP to caller SP
	// keeps all frame objects within the allocated stack area.
	if frameSize != 0 {
		instrs = append(instrs, mir.NewMachineInstr("add",
			[]*mir.Register{x29},
			[]mir.Operand{sp, mir.NewImmediate(frameSize, nil)},
		))
	} else {
		instrs = append(instrs, mir.NewMachineInstr("mov",
			[]*mir.Register{x29},
			[]mir.Operand{sp},
		))
	}

	// Save additional callee-saved registers (x19–x28) in pairs.
	// x29 is at [sp+0], x30 at [sp+8]; extra pairs start at [sp+16].
	//
	//   offset  16: stp regN,   regN+1, [sp, #16]
	//   offset  32: stp regN+2, regN+3, [sp, #32]
	//   …
	//   odd tail:   str regLast,        [sp, #offset]
	offset := 16
	for i := 0; i+1 < len(extra); i += 2 {
		r1 := mir.NewRegister(extra[i], mir.PhysicalReg, nil)
		r2 := mir.NewRegister(extra[i+1], mir.PhysicalReg, nil)
		off := mir.NewImmediate(offset, nil)
		instrs = append(instrs, mir.NewMachineInstr("stp",
			[]*mir.Register{},
			[]mir.Operand{r1, r2, mir.NewMemory(sp, off, nil)},
		))
		offset += 16
	}
	if len(extra)%2 != 0 {
		last := mir.NewRegister(extra[len(extra)-1], mir.PhysicalReg, nil)
		off := mir.NewImmediate(offset, nil)
		instrs = append(instrs, mir.NewMachineInstr("str",
			[]*mir.Register{},
			[]mir.Operand{last, mir.NewMemory(sp, off, nil)},
		))
	}

	return instrs
}

func emitARM64Epilogue(fn *mir.Function, abi target.ABI) []mir.Instr {
	if fn == nil || fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return nil
	}

	var instrs []mir.Instr
	frameSize := fn.Frame.TotalSize

	x29 := mir.NewRegister("x29", mir.PhysicalReg, nil)
	x30 := mir.NewRegister("x30", mir.PhysicalReg, nil)
	sp := mir.NewRegister("sp", mir.PhysicalReg, nil)

	// Collect extra callee-saved registers in the same order as saved, so we
	// can compute their stack offsets, then restore in reverse order.
	var extra []string
	for _, r := range fn.Frame.SavedRegs {
		if r != "x29" && r != "x30" {
			extra = append(extra, r)
		}
	}
	frameSize = arm64EffectiveFrameSize(frameSize, len(extra))

	// Compute the offset just past the last saved register (mirrors prologue).
	numPairs := len(extra) / 2
	hasOdd := len(extra)%2 != 0
	baseOffset := 16 // extra registers start here

	// Restore odd-trailing register first (highest offset), then pairs in reverse.
	if hasOdd {
		oddOffset := baseOffset + numPairs*16
		last := mir.NewRegister(extra[len(extra)-1], mir.PhysicalReg, nil)
		off := mir.NewImmediate(oddOffset, nil)
		instrs = append(instrs, mir.NewMachineInstr("ldr",
			[]*mir.Register{last},
			[]mir.Operand{mir.NewMemory(sp, off, nil)},
		))
	}
	for i := numPairs - 1; i >= 0; i-- {
		r1 := mir.NewRegister(extra[i*2], mir.PhysicalReg, nil)
		r2 := mir.NewRegister(extra[i*2+1], mir.PhysicalReg, nil)
		off := mir.NewImmediate(baseOffset+i*16, nil)
		instrs = append(instrs, mir.NewMachineInstr("ldp",
			[]*mir.Register{r1, r2},
			[]mir.Operand{mir.NewMemory(sp, off, nil)},
		))
	}

	// ldp.post x29, x30, [sp], #frameSize
	// Post-indexed: load x29 from [sp], x30 from [sp+8], then sp += frameSize.
	// Srcs[0] = base address ([sp]), Srcs[1] = post-index increment (#frameSize).
	instrs = append(instrs, mir.NewMachineInstr("ldp.post",
		[]*mir.Register{x29, x30},
		[]mir.Operand{mir.NewMemory(sp, nil, nil), mir.NewImmediate(frameSize, nil)},
	))

	return instrs
}

func arm64EffectiveFrameSize(base int, extraSaved int) int {
	if base < 16 {
		base = 16
	}
	need := 16 + extraSaved*8
	if base < need {
		base = need
	}
	if rem := base % 16; rem != 0 {
		base += 16 - rem
	}
	return base
}

// ── RISC-V Prologue/Epilogue ──────────────────────────────────────────────
//
// RISC-V uses the standard RISC-V calling convention.
// The prologue allocates the frame and saves callee-saved registers,
// particularly ra (link register) and s0 (frame pointer).
//
// A typical prologue:
// 1. Allocate frame: addi sp, sp, -frameSize
// 2. Save ra: sd ra, frameSize-8(sp)
// 3. Save s0: sd s0, frameSize-16(sp)
// 4. Set up fp: addi s0, sp, frameSize

func emitRISCVPrologue(fn *mir.Function, abi target.ABI) []mir.Instr {
	if fn == nil || fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return nil
	}

	var instrs []mir.Instr
	frameSize := fn.Frame.TotalSize

	sp := mir.NewRegister("sp", mir.PhysicalReg, nil)
	ra := mir.NewRegister("ra", mir.PhysicalReg, nil)
	s0 := mir.NewRegister("s0", mir.PhysicalReg, nil)

	// Step 1: allocate frame.
	//   addi sp, sp, -frameSize
	instrs = append(instrs, mir.NewMachineInstr("addi",
		[]*mir.Register{sp},
		[]mir.Operand{sp, mir.NewImmediate(-frameSize, nil)},
	))

	// Steps 2–3: save ra and s0 at the top of the new frame.
	// The RISC-V psABI places them just below the old sp:
	//   ra at (frameSize-8)(sp)   — i.e. the highest occupied slot
	//   s0 at (frameSize-16)(sp)  — one word below ra
	instrs = append(instrs, mir.NewMachineInstr("sd",
		[]*mir.Register{},
		[]mir.Operand{ra, mir.NewMemory(sp, mir.NewImmediate(frameSize-8, nil), nil)},
	))
	instrs = append(instrs, mir.NewMachineInstr("sd",
		[]*mir.Register{},
		[]mir.Operand{s0, mir.NewMemory(sp, mir.NewImmediate(frameSize-16, nil), nil)},
	))

	// Step 4: save any additional callee-saved registers (s1–s11) that the
	// register allocator marked as live-across-call.
	// They are laid out in SavedRegs order at frameSize-24, frameSize-32, …
	slotOffset := frameSize - 24
	for _, regName := range fn.Frame.SavedRegs {
		if regName == "ra" || regName == "s0" {
			continue // already saved above
		}
		reg := mir.NewRegister(regName, mir.PhysicalReg, nil)
		instrs = append(instrs, mir.NewMachineInstr("sd",
			[]*mir.Register{},
			[]mir.Operand{reg, mir.NewMemory(sp, mir.NewImmediate(slotOffset, nil), nil)},
		))
		slotOffset -= 8
	}

	// Step 5: establish frame pointer.
	//   addi s0, sp, frameSize   (s0 = old sp)
	instrs = append(instrs, mir.NewMachineInstr("addi",
		[]*mir.Register{s0},
		[]mir.Operand{sp, mir.NewImmediate(frameSize, nil)},
	))

	return instrs
}

func emitRISCVEpilogue(fn *mir.Function, abi target.ABI) []mir.Instr {
	if fn == nil || fn.Frame == nil || fn.Frame.TotalSize == 0 {
		return nil
	}

	var instrs []mir.Instr
	frameSize := fn.Frame.TotalSize

	sp := mir.NewRegister("sp", mir.PhysicalReg, nil)
	ra := mir.NewRegister("ra", mir.PhysicalReg, nil)
	s0 := mir.NewRegister("s0", mir.PhysicalReg, nil)

	// Collect extra callee-saved registers in save order so we can compute their
	// offsets, then restore them in reverse order (LIFO mirrors the prologue).
	var extra []string
	for _, r := range fn.Frame.SavedRegs {
		if r != "ra" && r != "s0" {
			extra = append(extra, r)
		}
	}

	// Restore extra registers in reverse save order.
	// Save order assigned offsets: extra[0] → frameSize-24,
	//                               extra[1] → frameSize-32, …
	for i := len(extra) - 1; i >= 0; i-- {
		offset := frameSize - 24 - i*8
		reg := mir.NewRegister(extra[i], mir.PhysicalReg, nil)
		instrs = append(instrs, mir.NewMachineInstr("ld",
			[]*mir.Register{reg},
			[]mir.Operand{mir.NewMemory(sp, mir.NewImmediate(offset, nil), nil)},
		))
	}

	// Restore s0 (frame pointer) then ra (link register).
	//   ld s0, (frameSize-16)(sp)
	//   ld ra, (frameSize-8)(sp)
	instrs = append(instrs, mir.NewMachineInstr("ld",
		[]*mir.Register{s0},
		[]mir.Operand{mir.NewMemory(sp, mir.NewImmediate(frameSize-16, nil), nil)},
	))
	instrs = append(instrs, mir.NewMachineInstr("ld",
		[]*mir.Register{ra},
		[]mir.Operand{mir.NewMemory(sp, mir.NewImmediate(frameSize-8, nil), nil)},
	))

	// Deallocate frame.
	//   addi sp, sp, frameSize
	instrs = append(instrs, mir.NewMachineInstr("addi",
		[]*mir.Register{sp},
		[]mir.Operand{sp, mir.NewImmediate(frameSize, nil)},
	))

	return instrs
}
