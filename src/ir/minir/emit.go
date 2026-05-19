package minir

import (
	"fmt"
	"io"
	"sort"
	"strings"
)

// Emitter writes minir textual IR directly to an io.Writer.
// The caller is responsible for any buffering (e.g. wrapping w in a
// bufio.Writer before passing it in).  Each Emit* method returns the
// total number of bytes written and the first error encountered.
type Emitter struct {
	w io.Writer
}

// NewEmitter returns an Emitter that writes to w.
func NewEmitter(w io.Writer) *Emitter { return &Emitter{w: w} }

// ── low-level helpers ─────────────────────────────────────────────────────────

// write is a thin wrapper around io.WriteString that converts
// (int, error) → (int, error) and accumulates the byte count.
func (e *Emitter) write(s string) (int, error) {
	return io.WriteString(e.w, s)
}

func (e *Emitter) writef(format string, args ...interface{}) (int, error) {
	return fmt.Fprintf(e.w, format, args...)
}

// addN adds n2 to n1, returning the larger total and the first non-nil error.
func addN(n1 int, err1 error, n2 int, err2 error) (int, error) {
	if err1 != nil {
		return n1, err1
	}
	return n1 + n2, err2
}

// ── EmitInstr ─────────────────────────────────────────────────────────────────

// EmitInstr writes a single instruction (formatted by FormatInstr) to w.
func (e *Emitter) EmitInstr(ins Instr) (int, error) {
	return e.write(FormatInstr(ins))
}

// ── EmitBlock ────────────────────────────────────────────────────────────────

// EmitBlock writes a basic block (label + indented instructions) to w.
func (e *Emitter) EmitBlock(b *Block) (int, error) {
	total := 0

	n, err := e.writef("%s:\n", b.Label)
	total += n
	if err != nil {
		return total, err
	}

	for _, ins := range b.Instrs {
		n, err = e.write("  ")
		total += n
		if err != nil {
			return total, err
		}
		n, err = e.EmitInstr(ins)
		total += n
		if err != nil {
			return total, err
		}
		n, err = e.write("\n")
		total += n
		if err != nil {
			return total, err
		}
	}

	// emit terminator if it is not already the last instruction in Instrs
	if b.Term != nil {
		lastIdx := len(b.Instrs) - 1
		if lastIdx < 0 || b.Instrs[lastIdx] != b.Term {
			n, err = e.write("  ")
			total += n
			if err != nil {
				return total, err
			}
			n, err = e.EmitInstr(b.Term)
			total += n
			if err != nil {
				return total, err
			}
			n, err = e.write("\n")
			total += n
			if err != nil {
				return total, err
			}
		}
	}

	return total, nil
}

// ── EmitFunction ─────────────────────────────────────────────────────────────

// EmitFunction writes the full textual representation of fn to w.
// Blocks are emitted in ascending ID order for deterministic output.
func (e *Emitter) EmitFunction(fn *Function) (int, error) {
	total := 0

	// signature
	params := make([]string, 0, len(fn.Params))
	for _, p := range fn.Params {
		params = append(params, TempString(p))
	}
	res := "void"
	if fn.Result != nil {
		res = fn.Result.String()
	}
	n, err := e.writef("func %s(%s) -> %s\n", fn.FnName, strings.Join(params, ", "), res)
	total += n
	if err != nil {
		return total, err
	}

	// Emit blocks in reverse-postorder (a natural order for CFGs). If the
	// function has no entry/graph structure fall back to ascending ID order.
	ids := fn.ReversePostOrder()
	if len(ids) == 0 {
		ids = make([]int, 0, len(fn.Blocks))
		for id := range fn.Blocks {
			ids = append(ids, id)
		}
		sort.Ints(ids)
	}

	for i, id := range ids {
		n, err = e.EmitBlock(fn.Blocks[id])
		total += n
		if err != nil {
			return total, err
		}
		// separate blocks with a blank line for readability (but not after
		// the last block).
		if i < len(ids)-1 {
			n, err = e.write("\n")
			total += n
			if err != nil {
				return total, err
			}
		}
	}

	return total, nil
}

// ── EmitModule ───────────────────────────────────────────────────────────────

// EmitModule writes the full textual representation of m to w in a style
// analogous to LLVM textual IR.
func (e *Emitter) EmitModule(m *Module) (int, error) {
	total := 0
	var n int
	var err error

	name := m.Name
	if name == "" {
		name = "<unnamed>"
	}
	n, err = e.writef("module %s\n", name)
	total += n
	if err != nil {
		return total, err
	}

	if len(m.Globals)+len(m.Constants)+len(m.Externals) > 0 {
		n, err = e.write("\n")
		total, err = addN(total, err, n, err)
		if err != nil {
			return total, err
		}
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
		n, err = e.writef("@%s = %-8s global    %s %s\n", gv.Name, gv.Linkage, ty, init)
		total += n
		if err != nil {
			return total, err
		}
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
		n, err = e.writef("@%s = %-8s constant  %s %s\n", gc.Name, gc.Linkage, ty, init)
		total += n
		if err != nil {
			return total, err
		}
	}

	if len(m.Externals) > 0 {
		n, err = e.write("\n")
		total += n
		if err != nil {
			return total, err
		}
	}
	for _, ef := range m.Externals {
		n, err = e.writef("%s\n", ef.String())
		total += n
		if err != nil {
			return total, err
		}
	}

	if len(m.Functions) > 0 {
		n, err = e.write("\n")
		total += n
		if err != nil {
			return total, err
		}
	}
	for i, fn := range m.Functions {
		n, err = e.EmitFunction(fn)
		total += n
		if err != nil {
			return total, err
		}
		if i < len(m.Functions)-1 {
			n, err = e.write("\n")
			total += n
			if err != nil {
				return total, err
			}
		}
	}

	return total, nil
}

// ── EmitProgram ──────────────────────────────────────────────────────────────

// EmitProgram writes all modules in prog to w, separated by a divider line.
func (e *Emitter) EmitProgram(prog *Program) (int, error) {
	total := 0
	for i, mod := range prog.Modules {
		n, err := e.EmitModule(mod)
		total += n
		if err != nil {
			return total, err
		}
		if i < len(prog.Modules)-1 {
			n, err = e.write("\n; ────────────────────────────────────────────\n\n")
			total += n
			if err != nil {
				return total, err
			}
		}
	}
	return total, nil
}

// ── io.WriterTo implementations ───────────────────────────────────────────────

// WriteTo implements io.WriterTo for *Function.
func (f *Function) WriteTo(w io.Writer) (int64, error) {
	n, err := NewEmitter(w).EmitFunction(f)
	return int64(n), err
}

// WriteTo implements io.WriterTo for *Module.
func (m *Module) WriteTo(w io.Writer) (int64, error) {
	n, err := NewEmitter(w).EmitModule(m)
	return int64(n), err
}

// WriteTo implements io.WriterTo for *Program.
func (prog *Program) WriteTo(w io.Writer) (int64, error) {
	n, err := NewEmitter(w).EmitProgram(prog)
	return int64(n), err
}
