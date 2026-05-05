package minir

import (
	"fmt"
	"io"
	"strings"
)

// TempString returns a compact representation of a Temp including its name
// and type: e.g. %x:i32 or %t1:i64
func TempString(t *Temp) string {
	if t == nil {
		return "<nil>"
	}
	ty := "<nil>"
	if t.Ty != nil {
		ty = t.Ty.String()
	}
	if t.NameStr != "" {
		return fmt.Sprintf("%%%s:%s", t.NameStr, ty)
	}
	return fmt.Sprintf("%%t%d:%s", t.ID, ty)
}

// ValueString formats a Value (Temp or Constant) for printing.
func ValueString(v Value) string {
	if v == nil {
		return "<nil>"
	}
	switch x := v.(type) {
	case *Temp:
		return TempString(x)
	case *Constant:
		// Constants no longer carry a ":type" suffix in their textual
		// representation; Constant.String() already returns the short form.
		return x.String()
	default:
		return v.String()
	}
}

// ShortValueString returns a compact operand representation without an attached
// ":<type>" suffix. This is useful when the surrounding formatter prints
// the operand type separately (LLVM-style: "i32 42", not "42:i32").
func ShortValueString(v Value) string {
	if v == nil {
		return "<nil>"
	}
	switch x := v.(type) {
	case *Temp:
		return x.String()
	case *Constant:
		if x.NameStr != "" {
			return x.NameStr
		}
		return fmt.Sprintf("%v", x.Val)
	default:
		return v.String()
	}
}

// FormatInstr renders a single instruction to a string. It prefers
// instruction-specific formatting but falls back to Instr.String().
func FormatInstr(ins Instr) string {
	switch v := ins.(type) {
	case *PhiInst:
		et := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			et = v.Dst.Type().String()
		}
		var parts []string
		for _, a := range v.Args {
			// LLVM style: [ <val>, <label> ]
			parts = append(parts, fmt.Sprintf("[ %s, %s ]", ShortValueString(a.Val), a.BlockLabel))
		}
		return fmt.Sprintf("%s = phi %s %s", ShortValueString(v.Dst), et, strings.Join(parts, ", "))

	case *BinaryInst:
		ty := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			ty = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = %s %s %s, %s", ShortValueString(v.Dst), v.Op, ty, ShortValueString(v.Left), ShortValueString(v.Right))

	case *ICmpInst:
		opTy := "<nil>"
		if v.Left != nil && v.Left.Type() != nil {
			opTy = v.Left.Type().String()
		}
		return fmt.Sprintf("%s = icmp.%s %s %s, %s", ShortValueString(v.Dst), v.Pred, opTy, ShortValueString(v.Left), ShortValueString(v.Right))

	case *FCmpInst:
		// FCmpInst embeds ICmpInst
		base := &v.ICmpInst
		opTy := "<nil>"
		if base.Left != nil && base.Left.Type() != nil {
			opTy = base.Left.Type().String()
		}
		return fmt.Sprintf("%s = fcmp.%s %s %s, %s", ShortValueString(base.Dst), base.Pred, opTy, ShortValueString(base.Left), ShortValueString(base.Right))

	case *LoadInst:
		et := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			et = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = load %s, %s", ShortValueString(v.Dst), et, ShortValueString(v.Addr))

	case *StoreInst:
		valTy := "<nil>"
		if v.Val != nil && v.Val.Type() != nil {
			valTy = v.Val.Type().String()
		}
		return fmt.Sprintf("store %s %s, %s", valTy, ShortValueString(v.Val), ShortValueString(v.Addr))

	case *AllocaInst:
		at := "<nil>"
		if v.AllocType != nil {
			at = v.AllocType.String()
		}
		return fmt.Sprintf("%s = alloca %s", ShortValueString(v.Dst), at)

	case *GEPInst:
		// offsets as comma-separated ints
		offs := ""
		for i, o := range v.Offsets {
			if i > 0 {
				offs += ", "
			}
			offs += fmt.Sprintf("%d", o)
		}
		et := "<nil>"
		if v.ElemType != nil {
			et = v.ElemType.String()
		}
		return fmt.Sprintf("%s = gep %s, %s, %s", ShortValueString(v.Dst), et, ShortValueString(v.Base), offs)

	case *UnaryInst:
		ty := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			ty = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = %s %s %s", ShortValueString(v.Dst), v.Op, ty, ShortValueString(v.Src))

	case *CastInst:
		srcTy := "<nil>"
		dstTy := "<nil>"
		if v.Src != nil && v.Src.Type() != nil {
			srcTy = v.Src.Type().String()
		}
		if v.Dst != nil && v.Dst.Type() != nil {
			dstTy = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = %s %s %s to %s", ShortValueString(v.Dst), v.Op, srcTy, ShortValueString(v.Src), dstTy)

	case *HaltInst:
		if v.Code == nil {
			return "halt"
		}
		return fmt.Sprintf("halt %s", ShortValueString(v.Code))

	case *CallInst:
		var args []string
		for _, a := range v.Args {
			// use short form for args (no ":type" suffix). If you prefer
			// typed arguments (LLVM style) we can print the type before each
			// arg instead.
			args = append(args, ShortValueString(a))
		}
		if v.Dst == nil {
			return fmt.Sprintf("call %s(%s)", v.Callee, strings.Join(args, ", "))
		}
		// attempt to include result type if available
		res := "void"
		if v.Dst != nil && v.Dst.Type() != nil {
			res = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = call %s %s(%s)", ShortValueString(v.Dst), res, v.Callee, strings.Join(args, ", "))

	case *ReturnInst:
		if v.Result == nil {
			return "ret"
		}
		return fmt.Sprintf("ret %s", ShortValueString(v.Result))

	case *JumpInst:
		return fmt.Sprintf("jmp %s", v.Target)

	case *CondBrInst:
		return fmt.Sprintf("br %s, %s, %s", ShortValueString(v.Cond), v.TrueLabel, v.FalseLabel)

	case *SwitchInst:
		var arms []string
		for _, a := range v.Arms {
			arms = append(arms, fmt.Sprintf("%d:%s", a.Val, a.Label))
		}
		return fmt.Sprintf("switch %s, default=%s [%s]", ShortValueString(v.Key), v.Default, strings.Join(arms, ", "))
	}
	// fallback
	return ins.String()
}

// FormatFunction returns a textual representation of fn suitable for
// debugging/printing. Blocks are printed in reverse-postorder (a natural CFG
// traversal order). It is a thin wrapper over Emitter.EmitFunction writing
// into a strings.Builder.
func FormatFunction(fn *Function) string {
	var sb strings.Builder
	_, _ = NewEmitter(&sb).EmitFunction(fn) // strings.Builder never errors
	return sb.String()
}

// PrintFunction writes the formatted function to w.
func PrintFunction(w io.Writer, fn *Function) (int, error) {
	return NewEmitter(w).EmitFunction(fn)
}

// FormatModule returns a textual representation of a Module in a style
// analogous to LLVM textual IR:
//
//	module <name>
//
//	@g  = internal  global   i32 0
//	@C  = private   constant i32 42
//
//	declare @printf(ptr.i32, ...) -> i32
//
//	func foo(...) -> T
//	  entry:
//	    ...
//
// It is a thin wrapper over Emitter.EmitModule writing into a strings.Builder.
func FormatModule(m *Module) string {
	var sb strings.Builder
	_, _ = NewEmitter(&sb).EmitModule(m) // strings.Builder never errors
	return sb.String()
}

// PrintModule writes the formatted module to w.
func PrintModule(w io.Writer, m *Module) (int, error) {
	return NewEmitter(w).EmitModule(m)
}

// FormatProgram returns a textual representation of all modules in prog,
// separated by a blank line, in order.
// It is a thin wrapper over Emitter.EmitProgram writing into a strings.Builder.
func FormatProgram(prog *Program) string {
	var sb strings.Builder
	_, _ = NewEmitter(&sb).EmitProgram(prog) // strings.Builder never errors
	return sb.String()
}

// PrintProgram writes the formatted program to w.
func PrintProgram(w io.Writer, prog *Program) (int, error) {
	return NewEmitter(w).EmitProgram(prog)
}
