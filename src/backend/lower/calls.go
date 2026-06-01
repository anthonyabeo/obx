package lower

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// LowerCallsInProgram expands every *mir.CallInstr in prog into an
// ABI-explicit sequence:
//
//  1. One MoveInstr per register-allocated argument (virtual → physical reg).
//  2. One StoreInstr per stack-spilled argument (SP-relative address);
//     the frame-layout pass is responsible for adjusting the stack pointer.
//  3. A bare *mir.CallInstr{Callee: ..., Args: nil, Dst: nil}.
//  4. An optional MoveInstr that copies the physical return register into
//     the original virtual destination.
//
// After this pass the selector needs only a single-element call pattern,
// call($callee), for both direct (symbol) and indirect (register) callees.
func LowerCallsInProgram(prog *mir.Program, tgt target.Target) (*mir.Program, error) {
	if prog == nil {
		return mir.NewProgram(), nil
	}
	if tgt == nil {
		return nil, fmt.Errorf("lower calls: nil target")
	}
	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		for _, fn := range mod.Functions {
			if fn == nil {
				continue
			}
			if err := lowerCallsInFunctionWithModule(fn, mod, tgt); err != nil {
				return nil, fmt.Errorf("lower calls in %s: %w", fn.Name, err)
			}
		}
	}
	return prog, nil
}

// LowerCallsInFunction expands call instructions inside fn in place.
func LowerCallsInFunction(fn *mir.Function, tgt target.Target) error {
	return lowerCallsInFunctionWithModule(fn, nil, tgt)
}

func lowerCallsInFunctionWithModule(fn *mir.Function, mod *mir.Module, tgt target.Target) error {
	if fn == nil {
		return fmt.Errorf("lower calls: nil function")
	}
	if tgt == nil {
		return fmt.Errorf("lower calls: nil target")
	}
	abi := tgt.ABIInfo()
	for _, block := range fn.Blocks {
		if block == nil {
			continue
		}
		expanded, err := lowerCallsInBlock(block, mod, tgt, abi)
		if err != nil {
			return fmt.Errorf("block %s: %w", block.Label, err)
		}
		block.Instrs = expanded
	}
	return nil
}

// lowerCallsInBlock returns a new instruction slice for block with every
// *mir.CallInstr expanded into the ABI-explicit sequence described above.
func lowerCallsInBlock(block *mir.Block, mod *mir.Module, tgt target.Target, abi target.ABI) ([]mir.Instr, error) {
	wordTy := mir.NewScalarType("i64", abi.WordSize)
	ptrTy := mir.NewScalarType("ptr", abi.WordSize)

	out := make([]mir.Instr, 0, len(block.Instrs))
	for _, instr := range block.Instrs {
		call, ok := instr.(*mir.CallInstr)
		if !ok {
			out = append(out, instr)
			continue
		}

		// Build the ABI plan for this call from the original arguments.
		plan, err := target.BuildCallPlan(call, abi)
		if err != nil {
			return nil, fmt.Errorf("call plan: %w", err)
		}

		// 1. Emit argument moves / stores in ABI order.
		varFixed, varOnStack := variadicCallPolicy(mod, tgt, call)
		stackStores := make([]mir.Instr, 0)
		regCopies := make([]target.Copy, 0)
		for _, loc := range plan.Args {
			argTy := loc.Value.Type()
			if argTy == nil {
				argTy = wordTy
			}

			// Darwin ARM64 variadics place unnamed args in the outgoing stack area.
			if varOnStack && loc.Index >= varFixed {
				sp := mir.NewRegister(abi.StackPointer, mir.PhysicalReg, ptrTy)
				stackOff := (loc.Index - varFixed) * abi.WordSize
				addr := mir.NewMemory(sp, mir.NewImmediate(stackOff, wordTy), ptrTy)
				stackStores = append(stackStores, &mir.StoreInstr{Addr: addr, Value: loc.Value})
				continue
			}

			if loc.InRegister {
				dst := mir.NewRegister(loc.Register, mir.PhysicalReg, argTy)
				regCopies = append(regCopies, target.Copy{Dst: dst, Src: loc.Value})
			} else {
				// Stack-spilled arg: store to the outgoing-args area.
				// The frame-layout pass inserts the SP adjustment.
				sp := mir.NewRegister(abi.StackPointer, mir.PhysicalReg, ptrTy)
				addr := mir.NewMemory(sp, mir.NewImmediate(loc.StackOffset, wordTy), ptrTy)
				stackStores = append(stackStores, &mir.StoreInstr{Addr: addr, Value: loc.Value})
			}
		}

		// Emit stack stores first so any physical-register source values are
		// captured before register-argument shuffles may overwrite them.
		out = append(out, stackStores...)

		// Register args form a parallel copy set and must be linearized to avoid
		// destructive cycles (e.g. x0<-x1, x1<-x2, x2<-x0).
		for _, cpy := range target.LinearizeParallelCopy(target.ParallelCopy{Copies: regCopies}) {
			out = append(out, &mir.MoveInstr{Dst: cpy.Dst, Src: cpy.Src})
		}

		// 2. Bare call — args are already in physical regs; Dst is cleared
		//    so classifyInstr produces matchNode{op:"call", args:[callee]}.
		originalDst := call.Dst
		out = append(out, &mir.CallInstr{Callee: call.Callee, Args: nil, Dst: nil})

		// 3. Copy physical return register into the original virtual destination.
		if originalDst != nil && plan.Result != nil && plan.Result.InRegister {
			retTy := originalDst.Type()
			if retTy == nil {
				retTy = wordTy
			}
			src := mir.NewRegister(plan.Result.Register, mir.PhysicalReg, retTy)
			out = append(out, &mir.MoveInstr{Dst: originalDst, Src: src})
		}
	}

	return out, nil
}

func variadicCallPolicy(mod *mir.Module, tgt target.Target, call *mir.CallInstr) (fixed int, stackVariadic bool) {
	if tgt == nil || call == nil {
		return 0, false
	}
	if tgt.Name() != target.Arm64Name {
		return 0, false
	}
	sym, ok := call.Callee.(*mir.Symbol)
	if !ok || sym == nil || sym.Name == "" {
		return 0, false
	}
	if mod != nil {
		ext := mod.ExternByName(sym.Name)
		if ext != nil && ext.Variadic {
			if ext.FixedArgCount < 0 {
				return 0, false
			}
			return ext.FixedArgCount, true
		}
	}

	// Some precompiled stdlib bundles may omit variadic extern metadata in the
	// lowered MIR. Fall back to known libc vararg prototypes used by stdlib IO.
	if fixed, ok := knownDarwinVariadicFixedArgs[sym.Name]; ok {
		return fixed, true
	}

	return 0, false
}

var knownDarwinVariadicFixedArgs = map[string]int{
	"printf":   1,
	"fprintf":  2,
	"sprintf":  2,
	"snprintf": 3,
	"scanf":    1,
	"fscanf":   2,
	"sscanf":   2,
}

