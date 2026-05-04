package minir

import (
	"bytes"
	"fmt"
	"io"
	"sort"
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
		// print constant with its type if available
		ty := "<nil>"
		if x.Ty != nil {
			ty = x.Ty.String()
		}
		if x.NameStr != "" {
			return fmt.Sprintf("%s:%s", x.NameStr, ty)
		}
		return fmt.Sprintf("%v:%s", x.Val, ty)
	default:
		return v.String()
	}
}

// FormatInstr renders a single instruction to a string. It prefers
// instruction-specific formatting but falls back to Instr.String().
func FormatInstr(ins Instr) string {
	switch v := ins.(type) {
	case *PhiInst:
		var parts []string
		for _, a := range v.Args {
			parts = append(parts, fmt.Sprintf("%s:%s", a.BlockLabel, ValueString(a.Val)))
		}
		return fmt.Sprintf("%s = phi [%s]", TempString(v.Dst), strings.Join(parts, ", "))

	case *BinaryInst:
		return fmt.Sprintf("%s = %s %s, %s", TempString(v.Dst), v.Op, ValueString(v.Left), ValueString(v.Right))

	case *ICmpInst:
		return fmt.Sprintf("%s = icmp.%s %s, %s", TempString(v.Dst), v.Pred, ValueString(v.Left), ValueString(v.Right))

	case *FCmpInst:
		// FCmpInst embeds ICmpInst
		base := &v.ICmpInst
		return fmt.Sprintf("%s = fcmp.%s %s, %s", TempString(base.Dst), base.Pred, ValueString(base.Left), ValueString(base.Right))

	case *LoadInst:
		return fmt.Sprintf("%s = load %s", TempString(v.Dst), ValueString(v.Addr))

	case *StoreInst:
		return fmt.Sprintf("store %s, %s", ValueString(v.Val), ValueString(v.Addr))

	case *AllocaInst:
		at := "<nil>"
		if v.AllocType != nil {
			at = v.AllocType.String()
		}
		return fmt.Sprintf("%s = alloca %s", TempString(v.Dst), at)

	case *GEPInst:
		offs := fmt.Sprint(v.Offsets)
		et := "<nil>"
		if v.ElemType != nil {
			et = v.ElemType.String()
		}
		return fmt.Sprintf("%s = gep %s, %s, offsets=%s", TempString(v.Dst), ValueString(v.Base), et, offs)

	case *CallInst:
		var args []string
		for _, a := range v.Args {
			args = append(args, ValueString(a))
		}
		if v.Dst == nil {
			return fmt.Sprintf("call %s(%s)", v.Callee, strings.Join(args, ", "))
		}
		return fmt.Sprintf("%s = call %s(%s)", TempString(v.Dst), v.Callee, strings.Join(args, ", "))

	case *ReturnInst:
		if v.Result == nil {
			return "ret"
		}
		return fmt.Sprintf("ret %s", ValueString(v.Result))

	case *JumpInst:
		return fmt.Sprintf("jmp %s", v.Target)

	case *CondBrInst:
		return fmt.Sprintf("br %s, %s, %s", TempString(v.Cond), v.TrueLabel, v.FalseLabel)

	case *SwitchInst:
		var arms []string
		for _, a := range v.Arms {
			arms = append(arms, fmt.Sprintf("%d:%s", a.Val, a.Label))
		}
		return fmt.Sprintf("switch %s, default=%s [%s]", TempString(v.Key), v.Default, strings.Join(arms, ", "))
	}
	// fallback
	return ins.String()
}

// FormatFunction returns a textual representation of fn suitable for
// debugging/printing. Blocks are printed in ascending ID order.
func FormatFunction(fn *Function) string {
	var buf bytes.Buffer
	// signature
	var params []string
	for _, p := range fn.Params {
		params = append(params, TempString(p))
	}
	res := "void"
	if fn.Result != nil {
		res = fn.Result.String()
	}
	buf.WriteString(fmt.Sprintf("func %s(%s) -> %s\n", fn.FnName, strings.Join(params, ", "), res))

	// sort blocks by ID for deterministic output
	ids := make([]int, 0, len(fn.Blocks))
	for id := range fn.Blocks {
		ids = append(ids, id)
	}
	sort.Ints(ids)

	for _, id := range ids {
		b := fn.Blocks[id]
		buf.WriteString(fmt.Sprintf("%s:\n", b.Label))
		for _, ins := range b.Instrs {
			buf.WriteString("  ")
			buf.WriteString(FormatInstr(ins))
			buf.WriteByte('\n')
		}
		// ensure terminator printed if not already printed as last instr
		if b.Term != nil {
			lastIdx := -1
			if len(b.Instrs) > 0 {
				lastIdx = len(b.Instrs) - 1
			}
			if lastIdx < 0 || b.Instrs[lastIdx] != b.Term {
				buf.WriteString("  ")
				buf.WriteString(FormatInstr(b.Term))
				buf.WriteByte('\n')
			}
		}
	}
	return buf.String()
}

// PrintFunction writes the formatted function to w.
func PrintFunction(w io.Writer, fn *Function) (int, error) {
	s := FormatFunction(fn)
	return w.Write([]byte(s))
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
func FormatModule(m *Module) string {
	var buf bytes.Buffer
	name := m.Name
	if name == "" {
		name = "<unnamed>"
	}
	buf.WriteString(fmt.Sprintf("module %s\n", name))

	if len(m.Globals)+len(m.Constants)+len(m.Externals) > 0 {
		buf.WriteByte('\n')
	}

	for _, gv := range m.Globals {
		ty := "<nil>"
		if gv.Ty != nil {
			ty = gv.Ty.String()
		}
		init := "zeroinit"
		if gv.Init != nil {
			init = ValueString(gv.Init)
		}
		buf.WriteString(fmt.Sprintf("@%s = %-8s global    %s %s\n", gv.Name, gv.Linkage, ty, init))
	}

	for _, gc := range m.Constants {
		ty := "<nil>"
		if gc.Ty != nil {
			ty = gc.Ty.String()
		}
		init := "<uninit>"
		if gc.Init != nil {
			init = ValueString(gc.Init)
		}
		buf.WriteString(fmt.Sprintf("@%s = %-8s constant  %s %s\n", gc.Name, gc.Linkage, ty, init))
	}

	if len(m.Externals) > 0 {
		buf.WriteByte('\n')
	}
	for _, ef := range m.Externals {
		buf.WriteString(ef.String())
		buf.WriteByte('\n')
	}

	if len(m.Functions) > 0 {
		buf.WriteByte('\n')
	}
	for i, fn := range m.Functions {
		buf.WriteString(FormatFunction(fn))
		if i < len(m.Functions)-1 {
			buf.WriteByte('\n')
		}
	}
	return buf.String()
}

// PrintModule writes the formatted module to w.
func PrintModule(w io.Writer, m *Module) (int, error) {
	return w.Write([]byte(FormatModule(m)))
}

// FormatProgram returns a textual representation of all modules in prog,
// separated by a blank line, in order.
func FormatProgram(prog *Program) string {
	var buf bytes.Buffer
	for i, mod := range prog.Modules {
		buf.WriteString(FormatModule(mod))
		if i < len(prog.Modules)-1 {
			buf.WriteString("\n; ────────────────────────────────────────────\n\n")
		}
	}
	return buf.String()
}

// PrintProgram writes the formatted program to w.
func PrintProgram(w io.Writer, prog *Program) (int, error) {
	return w.Write([]byte(FormatProgram(prog)))
}

