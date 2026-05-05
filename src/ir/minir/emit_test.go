package minir

import (
	"bytes"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// buildSampleFn builds the same sample function used in pretty_test.go so the
// emitter tests can compare against FormatFunction output without duplication.
func buildSampleFn() *Function {
	i32 := I32()
	boolT := Bool()
	tempIDCounter = 0

	paramX := NewTemp("x", i32)
	const0 := NewConst("0", 0, i32)
	const1 := NewConst("1", 1, i32)

	entry := &Block{ID: 0, Label: "entry", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	then := &Block{ID: 1, Label: "then", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	els := &Block{ID: 2, Label: "else", Preds: map[int]*Block{}, Succs: map[int]*Block{}}
	merge := &Block{ID: 3, Label: "merge", Preds: map[int]*Block{}, Succs: map[int]*Block{}}

	tmp1 := NewTemp("tmp1", Ptr(i32))
	tmp1.IsAddr = true

	alloca := &AllocaInst{Dst: tmp1, AllocType: i32}
	store := &StoreInst{Val: paramX, Addr: tmp1}
	v := NewTemp("v", i32)
	load := &LoadInst{Dst: v, Addr: tmp1}
	cond := NewTemp("cond", boolT)
	icmp := &ICmpInst{Dst: cond, Pred: "eq", Left: v, Right: const0}
	br := &CondBrInst{Cond: cond, TrueLabel: then.Label, FalseLabel: els.Label}

	a := NewTemp("a", i32)
	add := &BinaryInst{Dst: a, Op: "add", Left: v, Right: const1}
	jmpThen := &JumpInst{Target: merge.Label}

	b := NewTemp("b", i32)
	sub := &BinaryInst{Dst: b, Op: "sub", Left: v, Right: const1}
	jmpElse := &JumpInst{Target: merge.Label}

	r := NewTemp("r", i32)
	phi := &PhiInst{Dst: r, Args: []PhiArm{{BlockLabel: then.Label, Val: a}, {BlockLabel: els.Label, Val: b}}}
	ret := &ReturnInst{Result: r}

	entry.Instrs = []Instr{alloca, store, load, icmp, br}
	entry.Term = br
	then.Instrs = []Instr{add, jmpThen}
	then.Term = jmpThen
	els.Instrs = []Instr{sub, jmpElse}
	els.Term = jmpElse
	merge.Instrs = []Instr{phi, ret}
	merge.Term = ret

	entry.AddSucc(then)
	entry.AddSucc(els)
	then.AddPred(entry)
	els.AddPred(entry)
	then.AddSucc(merge)
	els.AddSucc(merge)
	merge.AddPred(then)
	merge.AddPred(els)

	return &Function{
		FnName: "add1",
		Params: []*Temp{paramX},
		Result: i32,
		Entry:  entry,
		Exit:   merge,
		Blocks: map[int]*Block{entry.ID: entry, then.ID: then, els.ID: els, merge.ID: merge},
	}
}

// ── EmitFunction ─────────────────────────────────────────────────────────────

// TestEmitFunction_BytesBuffer ensures that emitting to a bytes.Buffer
// produces output identical to FormatFunction (the string-based path).
func TestEmitFunction_BytesBuffer(t *testing.T) {
	fn := buildSampleFn()
	want := FormatFunction(fn)

	var buf bytes.Buffer
	n, err := NewEmitter(&buf).EmitFunction(fn)
	if err != nil {
		t.Fatalf("EmitFunction: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if n != len(want) {
		t.Errorf("byte count: got %d, want %d", n, len(want))
	}
}

// TestEmitFunction_WriterTo verifies the io.WriterTo implementation on *Function.
func TestEmitFunction_WriterTo(t *testing.T) {
	fn := buildSampleFn()
	want := FormatFunction(fn)

	var buf bytes.Buffer
	n64, err := fn.WriteTo(&buf)
	if err != nil {
		t.Fatalf("WriteTo: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if int(n64) != len(want) {
		t.Errorf("byte count: got %d, want %d", n64, len(want))
	}
}

// TestEmitFunction_Pipe checks that emitting through an io.Pipe (simulating a
// network / file descriptor) produces the correct output without buffering.
func TestEmitFunction_Pipe(t *testing.T) {
	fn := buildSampleFn()
	want := FormatFunction(fn)

	pr, pw := io.Pipe()

	// write in a goroutine so the pipe doesn't block
	errc := make(chan error, 1)
	go func() {
		_, err := NewEmitter(pw).EmitFunction(fn)
		_ = pw.CloseWithError(err)
		errc <- err
	}()

	var buf bytes.Buffer
	if _, err := io.Copy(&buf, pr); err != nil {
		t.Fatalf("reading pipe: %v", err)
	}
	if err := <-errc; err != nil {
		t.Fatalf("EmitFunction via pipe: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
}

// ── EmitModule ───────────────────────────────────────────────────────────────

func buildSampleModule() *Module {
	fn := buildSampleFn()
	gv := &GlobalVar{
		Name:    "counter",
		Ty:      I32(),
		Linkage: InternalLinkage,
	}
	return &Module{
		Name:      "testmod",
		Globals:   []*GlobalVar{gv},
		Functions: []*Function{fn},
	}
}

// TestEmitModule_BytesBuffer checks module emission into a bytes.Buffer.
func TestEmitModule_BytesBuffer(t *testing.T) {
	m := buildSampleModule()
	want := FormatModule(m)

	var buf bytes.Buffer
	n, err := NewEmitter(&buf).EmitModule(m)
	if err != nil {
		t.Fatalf("EmitModule: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if n != len(want) {
		t.Errorf("byte count: got %d, want %d", n, len(want))
	}
}

// TestEmitModule_WriterTo verifies the io.WriterTo implementation on *Module.
func TestEmitModule_WriterTo(t *testing.T) {
	m := buildSampleModule()
	want := FormatModule(m)

	var buf bytes.Buffer
	n64, err := m.WriteTo(&buf)
	if err != nil {
		t.Fatalf("WriteTo: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if int(n64) != len(want) {
		t.Errorf("byte count: got %d, want %d", n64, len(want))
	}
}

// ── EmitProgram ──────────────────────────────────────────────────────────────

// TestEmitProgram_BytesBuffer checks program emission into a bytes.Buffer.
func TestEmitProgram_BytesBuffer(t *testing.T) {
	prog := &Program{Modules: []*Module{buildSampleModule()}}
	want := FormatProgram(prog)

	var buf bytes.Buffer
	n, err := NewEmitter(&buf).EmitProgram(prog)
	if err != nil {
		t.Fatalf("EmitProgram: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if n != len(want) {
		t.Errorf("byte count: got %d, want %d", n, len(want))
	}
}

// TestEmitProgram_WriterTo verifies the io.WriterTo implementation on *Program.
func TestEmitProgram_WriterTo(t *testing.T) {
	prog := &Program{Modules: []*Module{buildSampleModule()}}
	want := FormatProgram(prog)

	var buf bytes.Buffer
	n64, err := prog.WriteTo(&buf)
	if err != nil {
		t.Fatalf("WriteTo: unexpected error: %v", err)
	}
	if got := buf.String(); got != want {
		t.Errorf("output mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if int(n64) != len(want) {
		t.Errorf("byte count: got %d, want %d", n64, len(want))
	}
}

// ── EmitInstr ────────────────────────────────────────────────────────────────

// TestEmitInstr_MatchesFormatInstr ensures EmitInstr output equals FormatInstr.
func TestEmitInstr_MatchesFormatInstr(t *testing.T) {
	i32 := I32()
	tempIDCounter = 0

	instrs := []Instr{
		&BinaryInst{Dst: NewTemp("r", i32), Op: "add", Left: NewTemp("a", i32), Right: NewConst("1", 1, i32)},
		&ReturnInst{Result: NewTemp("r", i32)},
		&JumpInst{Target: "exit"},
		&AllocaInst{Dst: NewTemp("p", Ptr(i32)), AllocType: i32},
	}

	for _, ins := range instrs {
		want := FormatInstr(ins)
		var sb strings.Builder
		n, err := NewEmitter(&sb).EmitInstr(ins)
		if err != nil {
			t.Errorf("EmitInstr(%T): unexpected error: %v", ins, err)
			continue
		}
		if got := sb.String(); got != want {
			t.Errorf("EmitInstr(%T): got %q, want %q", ins, got, want)
		}
		if n != len(want) {
			t.Errorf("EmitInstr(%T): byte count got %d, want %d", ins, n, len(want))
		}
	}
}

// ── stdout smoke-test ─────────────────────────────────────────────────────────

// TestEmitFunction_Stdout is a manual smoke-test that writes to os.Stdout.
// It only runs when -v is supplied (t.Log is normally suppressed).
func TestEmitFunction_Stdout(t *testing.T) {
	fn := buildSampleFn()
	if _, err := NewEmitter(os.Stdout).EmitFunction(fn); err != nil {
		t.Fatalf("EmitFunction to stdout: %v", err)
	}
}

// ── file emission ─────────────────────────────────────────────────────────────

// projectRoot walks up from this source file's directory until it finds the
// directory that contains "go.mod", which is the project root.
func projectRoot(t *testing.T) string {
	t.Helper()
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller failed")
	}
	dir := filepath.Dir(file)
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatal("could not locate project root (no go.mod found)")
		}
		dir = parent
	}
}

// TestEmitFunction_File writes the sample function IR to
// <project-root>/build/add1.minir and verifies the file content matches
// FormatFunction output.
func TestEmitFunction_File(t *testing.T) {
	fn := buildSampleFn()
	want := FormatFunction(fn)

	outPath := filepath.Join(projectRoot(t), "build", "add1.minir")
	f, err := os.Create(outPath)
	if err != nil {
		t.Fatalf("create %s: %v", outPath, err)
	}
	defer f.Close()

	n, err := NewEmitter(f).EmitFunction(fn)
	if err != nil {
		t.Fatalf("EmitFunction to file: %v", err)
	}
	if err = f.Close(); err != nil {
		t.Fatalf("close %s: %v", outPath, err)
	}

	raw, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read back %s: %v", outPath, err)
	}
	if got := string(raw); got != want {
		t.Errorf("file content mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if n != len(want) {
		t.Errorf("byte count: got %d, want %d", n, len(want))
	}
	t.Logf("wrote %d bytes to %s", n, outPath)
}

// TestEmitModule_File writes testmod IR to <project-root>/build/testmod.minir.
func TestEmitModule_File(t *testing.T) {
	m := buildSampleModule()
	want := FormatModule(m)

	outPath := filepath.Join(projectRoot(t), "build", "testmod.minir")
	f, err := os.Create(outPath)
	if err != nil {
		t.Fatalf("create %s: %v", outPath, err)
	}
	defer f.Close()

	n, err := NewEmitter(f).EmitModule(m)
	if err != nil {
		t.Fatalf("EmitModule to file: %v", err)
	}
	if err = f.Close(); err != nil {
		t.Fatalf("close %s: %v", outPath, err)
	}

	raw, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read back %s: %v", outPath, err)
	}
	if got := string(raw); got != want {
		t.Errorf("file content mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if n != len(want) {
		t.Errorf("byte count: got %d, want %d", n, len(want))
	}
	t.Logf("wrote %d bytes to %s", n, outPath)
}

// TestEmitProgram_File writes the full program IR to
// <project-root>/build/program.minir using the io.WriterTo path.
func TestEmitProgram_File(t *testing.T) {
	prog := &Program{Modules: []*Module{buildSampleModule()}}
	want := FormatProgram(prog)

	outPath := filepath.Join(projectRoot(t), "build", "program.minir")
	f, err := os.Create(outPath)
	if err != nil {
		t.Fatalf("create %s: %v", outPath, err)
	}
	defer f.Close()

	// exercise the io.WriterTo path directly
	n64, err := prog.WriteTo(f)
	if err != nil {
		t.Fatalf("WriteTo file: %v", err)
	}
	if err = f.Close(); err != nil {
		t.Fatalf("close %s: %v", outPath, err)
	}

	raw, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read back %s: %v", outPath, err)
	}
	if got := string(raw); got != want {
		t.Errorf("file content mismatch\ngot:\n%s\nwant:\n%s", got, want)
	}
	if int(n64) != len(want) {
		t.Errorf("byte count: got %d, want %d", n64, len(want))
	}
	t.Logf("wrote %d bytes to %s", n64, outPath)
}
