package mir

import (
	"fmt"
	"io"
	"strings"
)

// OperandString returns a compact string representation of an Operand.
// Handles Register, Symbol, Immediate, Memory, and Label types.
func OperandString(op Operand) string {
	if op == nil {
		return "<nil>"
	}
	switch x := op.(type) {
	case *Register:
		if x == nil {
			return "<nil>"
		}
		if x.Ty != nil {
			return fmt.Sprintf("%s:%s", x.Name, x.Ty.String())
		}
		return x.Name
	case *Symbol:
		if x == nil {
			return "<nil>"
		}
		if x.Ty != nil {
			return fmt.Sprintf("@%s:%s", x.Name, x.Ty.String())
		}
		return "@" + x.Name
	case *Immediate:
		if x == nil {
			return "<nil>"
		}
		if x.Ty != nil {
			return fmt.Sprintf("%v:%s", x.Value, x.Ty.String())
		}
		return fmt.Sprint(x.Value)
	case *Memory:
		if x == nil {
			return "<nil>"
		}
		if x.Offset == nil {
			return fmt.Sprintf("[%s]", OperandString(x.Base))
		}
		return fmt.Sprintf("[%s + %s]", OperandString(x.Base), OperandString(x.Offset))
	case *Label:
		if x == nil {
			return "<nil>"
		}
		return x.Name
	default:
		return op.String()
	}
}

// ShortOperandString returns an operand string without type suffix.
// Useful when the surrounding formatter prints type elsewhere (LLVM-style).
func ShortOperandString(op Operand) string {
	if op == nil {
		return "<nil>"
	}
	switch x := op.(type) {
	case *Register:
		if x == nil {
			return "<nil>"
		}
		return x.Name
	case *Symbol:
		if x == nil {
			return "<nil>"
		}
		return "@" + x.Name
	case *Immediate:
		if x == nil {
			return "<nil>"
		}
		return fmt.Sprint(x.Value)
	case *Memory:
		if x == nil {
			return "<nil>"
		}
		if x.Offset == nil {
			return fmt.Sprintf("[%s]", ShortOperandString(x.Base))
		}
		return fmt.Sprintf("[%s + %s]", ShortOperandString(x.Base), ShortOperandString(x.Offset))
	case *Label:
		if x == nil {
			return "<nil>"
		}
		return x.Name
	default:
		return op.String()
	}
}

// FormatInstr returns a formatted string representation of an instruction,
// using instruction-specific formatting where available.
func FormatInstr(ins Instr) string {
	switch v := ins.(type) {
	case *MoveInstr:
		return fmt.Sprintf("%s = mov %s", ShortOperandString(v.Dst), OperandString(v.Src))

	case *LoadInstr:
		ty := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			ty = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = load %s, %s", ShortOperandString(v.Dst), ty, OperandString(v.Addr))

	case *StoreInstr:
		valTy := "<nil>"
		if v.Value != nil && v.Value.Type() != nil {
			valTy = v.Value.Type().String()
		}
		return fmt.Sprintf("store %s %s, %s", valTy, OperandString(v.Value), OperandString(v.Addr))

	case *UnaryInstr:
		ty := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			ty = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = %s %s %s", ShortOperandString(v.Dst), v.Op, ty, OperandString(v.X))

	case *BinaryInstr:
		ty := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			ty = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = %s %s %s, %s", ShortOperandString(v.Dst), v.Op, ty, ShortOperandString(v.Left), ShortOperandString(v.Right))

	case *CompareInstr:
		ty := "<nil>"
		if v.Left != nil && v.Left.Type() != nil {
			ty = v.Left.Type().String()
		}
		return fmt.Sprintf("%s = cmp.%s %s %s, %s", ShortOperandString(v.Dst), v.Pred, ty, ShortOperandString(v.Left), ShortOperandString(v.Right))

	case *PhiInstr:
		et := "<nil>"
		if v.Dst != nil && v.Dst.Type() != nil {
			et = v.Dst.Type().String()
		}
		var parts []string
		for _, a := range v.Arms {
			// LLVM style: [ <val>, <label> ]
			parts = append(parts, fmt.Sprintf("[ %s, %s ]", ShortOperandString(a.Value), a.BlockLabel))
		}
		return fmt.Sprintf("%s = phi %s %s", ShortOperandString(v.Dst), et, strings.Join(parts, ", "))

	case *CallInstr:
		var args []string
		for _, a := range v.Args {
			args = append(args, ShortOperandString(a))
		}
		if v.Dst == nil {
			return fmt.Sprintf("call %s(%s)", OperandString(v.Callee), strings.Join(args, ", "))
		}
		// attempt to include result type if available
		res := "void"
		if v.Dst != nil && v.Dst.Type() != nil {
			res = v.Dst.Type().String()
		}
		return fmt.Sprintf("%s = call %s %s(%s)", ShortOperandString(v.Dst), res, OperandString(v.Callee), strings.Join(args, ", "))

	case *JumpInstr:
		return fmt.Sprintf("jmp %s", v.Target)

	case *CondBrInstr:
		return fmt.Sprintf("br %s, %s, %s", ShortOperandString(v.Cond), v.TrueLabel, v.FalseLabel)

	case *ReturnInstr:
		if v.Value == nil {
			return "ret"
		}
		return fmt.Sprintf("ret %s", OperandString(v.Value))

	case *HaltInstr:
		if v.Code == nil {
			return "halt"
		}
		return fmt.Sprintf("halt %s", OperandString(v.Code))

	case *SwitchInstr:
		var arms []string
		for _, a := range v.Arms {
			arms = append(arms, fmt.Sprintf("%s:%s", OperandString(a.Value), a.Label))
		}
		return fmt.Sprintf("switch %s, default=%s [%s]", ShortOperandString(v.Value), v.Default, strings.Join(arms, ", "))

	case *MachineInstr:
		var parts []string
		for _, d := range v.Dsts {
			parts = append(parts, ShortOperandString(d))
		}
		for _, s := range v.Srcs {
			parts = append(parts, ShortOperandString(s))
		}
		if len(parts) == 0 {
			return v.Op
		}
		return fmt.Sprintf("%s %s", v.Op, strings.Join(parts, ", "))

	case *MachineTerm:
		var parts []string
		for _, s := range v.Srcs {
			parts = append(parts, ShortOperandString(s))
		}
		parts = append(parts, v.Targets...)
		if len(parts) == 0 {
			return v.Op
		}
		return fmt.Sprintf("%s %s", v.Op, strings.Join(parts, ", "))
	}

	// fallback
	return ins.String()
}

// FormatFunction returns a textual representation of fn suitable for
// debugging/printing. It is a thin wrapper over Emitter.EmitFunction writing
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

