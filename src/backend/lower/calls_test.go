package lower_test

import (
	"testing"

	"github.com/anthonyabeo/obx/src/backend/lower"
	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

var i64 = mir.NewScalarType("i64", 8)

func callFn(call *mir.CallInstr) *mir.Function {
	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	b.AddInstr(call)
	fn.AddBlock(b)
	return fn
}

func TestLowerCallsVoidNoArgs(t *testing.T) {
	tgt := target.NewRISCV64Target()
	fn := callFn(&mir.CallInstr{Callee: mir.NewSymbol("putc", nil)})

	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	instrs := fn.Blocks[0].Instrs
	if len(instrs) != 1 {
		t.Fatalf("expected 1 instr, got %d", len(instrs))
	}
	c, ok := instrs[0].(*mir.CallInstr)
	if !ok || len(c.Args) != 0 || c.Dst != nil {
		t.Fatalf("expected bare call, got %T args=%v dst=%v", instrs[0], c.Args, c.Dst)
	}
}

func TestLowerCallsThreeArgsWithResult(t *testing.T) {
	// RV64 ABI: args → a0, a1, a2, …; return → a0
	tgt := target.NewRISCV64Target()
	va := mir.NewRegister("va", mir.VirtualReg, i64)
	vb := mir.NewRegister("vb", mir.VirtualReg, i64)
	vc := mir.NewRegister("vc", mir.VirtualReg, i64)
	vr := mir.NewRegister("vr", mir.VirtualReg, i64)

	fn := callFn(&mir.CallInstr{
		Callee: mir.NewSymbol("foo", nil),
		Args:   []mir.Operand{va, vb, vc},
		Dst:    vr,
	})
	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	// mov a0←va, mov a1←vb, mov a2←vc, call foo, mov vr←a0
	if len(instrs) != 5 {
		t.Fatalf("expected 5 instrs, got %d: %v", len(instrs), instrs)
	}

	wantPhys := []string{"a0", "a1", "a2"}
	wantSrcs := []mir.Operand{va, vb, vc}
	for i, want := range wantPhys {
		mv, ok := instrs[i].(*mir.MoveInstr)
		if !ok {
			t.Fatalf("instrs[%d]: expected *mir.MoveInstr, got %T", i, instrs[i])
		}
		if mv.Dst.Name != want || mv.Dst.Kind != mir.PhysicalReg {
			t.Fatalf("instrs[%d]: dst = %s (%v), want phys %s", i, mv.Dst.Name, mv.Dst.Kind, want)
		}
		if mv.Src != wantSrcs[i] {
			t.Fatalf("instrs[%d]: src = %v, want %v", i, mv.Src, wantSrcs[i])
		}
	}

	c, ok := instrs[3].(*mir.CallInstr)
	if !ok || len(c.Args) != 0 || c.Dst != nil {
		t.Fatalf("instrs[3]: expected bare call, got %T args=%v dst=%v", instrs[3], c.Args, c.Dst)
	}
	if sym, ok := c.Callee.(*mir.Symbol); !ok || sym.Name != "foo" {
		t.Fatalf("instrs[3]: callee = %v, want foo", c.Callee)
	}

	ret, ok := instrs[4].(*mir.MoveInstr)
	if !ok {
		t.Fatalf("instrs[4]: expected *mir.MoveInstr, got %T", instrs[4])
	}
	if ret.Dst.Name != "vr" {
		t.Fatalf("instrs[4]: dst = %s, want vr", ret.Dst.Name)
	}
	src, ok := ret.Src.(*mir.Register)
	if !ok || src.Name != "a0" || src.Kind != mir.PhysicalReg {
		t.Fatalf("instrs[4]: src = %v, want phys a0", ret.Src)
	}
}

func TestLowerCallsIndirectCallee(t *testing.T) {
	tgt := target.NewRISCV64Target()
	fptr := mir.NewRegister("fp", mir.VirtualReg, i64)
	vr := mir.NewRegister("vr", mir.VirtualReg, i64)

	fn := callFn(&mir.CallInstr{Callee: fptr, Dst: vr})
	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	// bare call (no args to move), then mov vr←a0
	if len(instrs) != 2 {
		t.Fatalf("expected 2 instrs, got %d", len(instrs))
	}
	c, ok := instrs[0].(*mir.CallInstr)
	if !ok || len(c.Args) != 0 {
		t.Fatalf("instrs[0]: expected bare call")
	}
	reg, ok := c.Callee.(*mir.Register)
	if !ok || reg.Name != "fp" {
		t.Fatalf("instrs[0]: callee = %v, want fp", c.Callee)
	}
	mv, ok := instrs[1].(*mir.MoveInstr)
	if !ok || mv.Dst.Name != "vr" {
		t.Fatalf("instrs[1]: expected result move into vr")
	}
}

func TestLowerCallsStackArgs(t *testing.T) {
	// 10 args on RV64: first 8 in regs (a0–a7), last 2 on stack
	tgt := target.NewRISCV64Target()
	args := make([]mir.Operand, 10)
	for i := range args {
		args[i] = mir.NewImmediate(i, i64)
	}
	fn := callFn(&mir.CallInstr{
		Callee: mir.NewSymbol("many", nil),
		Args:   args,
	})
	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	// 2 stack stores + 8 reg moves + 1 bare call = 11
	if len(instrs) != 11 {
		t.Fatalf("expected 11 instrs, got %d", len(instrs))
	}
	for i := 0; i < 2; i++ {
		if _, ok := instrs[i].(*mir.StoreInstr); !ok {
			t.Fatalf("instrs[%d]: expected StoreInstr (stack arg), got %T", i, instrs[i])
		}
	}
	for i := 2; i < 10; i++ {
		if _, ok := instrs[i].(*mir.MoveInstr); !ok {
			t.Fatalf("instrs[%d]: expected MoveInstr, got %T", i, instrs[i])
		}
	}
	if _, ok := instrs[10].(*mir.CallInstr); !ok {
		t.Fatalf("instrs[10]: expected bare CallInstr, got %T", instrs[10])
	}
}

func TestLowerCallsARM64RegisterCycleUsesTemp(t *testing.T) {
	tgt, err := target.Lookup(target.Arm64Name)
	if err != nil {
		t.Fatalf("lookup arm64 target: %v", err)
	}
	x0 := mir.NewRegister("x0", mir.PhysicalReg, i64)
	x1 := mir.NewRegister("x1", mir.PhysicalReg, i64)
	x2 := mir.NewRegister("x2", mir.PhysicalReg, i64)

	fn := callFn(&mir.CallInstr{
		Callee: mir.NewSymbol("foo", nil),
		Args:   []mir.Operand{x1, x2, x0},
	})
	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	if len(instrs) != 5 {
		t.Fatalf("expected 5 instrs (4 moves + call), got %d", len(instrs))
	}

	sawTempMove := false
	for i := 0; i < 4; i++ {
		mv, ok := instrs[i].(*mir.MoveInstr)
		if !ok {
			t.Fatalf("instrs[%d]: expected MoveInstr, got %T", i, instrs[i])
		}
		if mv.Dst != nil && mv.Dst.Name == "x15" {
			sawTempMove = true
		}
	}
	if !sawTempMove {
		t.Fatalf("expected temp register move to break cycle, got %v", instrs[:4])
	}

	if _, ok := instrs[4].(*mir.CallInstr); !ok {
		t.Fatalf("instrs[4]: expected bare CallInstr, got %T", instrs[4])
	}
}

func TestLowerCallsProgramARM64VariadicExternSpillsUnnamedArgs(t *testing.T) {
	tgt, err := target.Lookup(target.Arm64Name)
	if err != nil {
		t.Fatalf("lookup arm64 target: %v", err)
	}

	mod := mir.NewModule("IO")
	ext := mir.NewExternDecl("fprintf", nil)
	ext.Variadic = true
	ext.FixedArgCount = 2
	mod.AddExtern(ext)

	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	b.AddInstr(&mir.CallInstr{
		Callee: mir.NewSymbol("fprintf", nil),
		Args: []mir.Operand{
			mir.NewRegister("stream", mir.VirtualReg, i64),
			mir.NewRegister("fmt", mir.VirtualReg, i64),
			mir.NewImmediate(610, i64),
		},
	})
	fn.AddBlock(b)
	mod.AddFunction(fn)

	prog := mir.NewProgram()
	prog.AddModule(mod)

	if _, err := lower.LowerCallsInProgram(prog, tgt); err != nil {
		t.Fatalf("LowerCallsInProgram failed: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	if len(instrs) != 4 {
		t.Fatalf("expected 4 instrs (store + 2 moves + call), got %d", len(instrs))
	}
	if _, ok := instrs[0].(*mir.StoreInstr); !ok {
		t.Fatalf("instrs[0]: expected variadic stack StoreInstr, got %T", instrs[0])
	}
	if mv, ok := instrs[1].(*mir.MoveInstr); !ok || mv.Dst.Name != "x0" {
		t.Fatalf("instrs[1]: expected move into x0, got %T %#v", instrs[1], instrs[1])
	}
	if mv, ok := instrs[2].(*mir.MoveInstr); !ok || mv.Dst.Name != "x1" {
		t.Fatalf("instrs[2]: expected move into x1, got %T %#v", instrs[2], instrs[2])
	}
	if _, ok := instrs[3].(*mir.CallInstr); !ok {
		t.Fatalf("instrs[3]: expected bare CallInstr, got %T", instrs[3])
	}
}

func TestLowerCallsProgramARM64VariadicFallbackByName(t *testing.T) {
	tgt, err := target.Lookup(target.Arm64Name)
	if err != nil {
		t.Fatalf("lookup arm64 target: %v", err)
	}

	mod := mir.NewModule("IO")
	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	b.AddInstr(&mir.CallInstr{
		Callee: mir.NewSymbol("fprintf", nil),
		Args: []mir.Operand{
			mir.NewRegister("stream", mir.VirtualReg, i64),
			mir.NewRegister("fmt", mir.VirtualReg, i64),
			mir.NewImmediate(610, i64),
		},
	})
	fn.AddBlock(b)
	mod.AddFunction(fn)

	prog := mir.NewProgram()
	prog.AddModule(mod)

	if _, err := lower.LowerCallsInProgram(prog, tgt); err != nil {
		t.Fatalf("LowerCallsInProgram failed: %v", err)
	}

	instrs := fn.Blocks[0].Instrs
	if len(instrs) != 4 {
		t.Fatalf("expected 4 instrs (store + 2 moves + call), got %d", len(instrs))
	}
	if _, ok := instrs[0].(*mir.StoreInstr); !ok {
		t.Fatalf("instrs[0]: expected variadic stack StoreInstr, got %T", instrs[0])
	}
}

func TestLowerCallsNonCallPassThrough(t *testing.T) {
	tgt := target.NewRISCV64Target()
	fn := mir.NewFunction("f", nil)
	b := mir.NewBlock(0, "entry")
	add := &mir.BinaryInstr{
		Dst:   mir.NewRegister("v0", mir.VirtualReg, i64),
		Op:    "add",
		Left:  mir.NewRegister("v1", mir.VirtualReg, i64),
		Right: mir.NewRegister("v2", mir.VirtualReg, i64),
	}
	b.AddInstr(add)
	fn.AddBlock(b)

	if err := lower.LowerCallsInFunction(fn, tgt); err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if fn.Blocks[0].Instrs[0] != add {
		t.Fatalf("expected non-call instr to pass through unchanged")
	}
}

func TestLowerCallsNilFunctionError(t *testing.T) {
	if err := lower.LowerCallsInFunction(nil, target.NewRISCV64Target()); err == nil {
		t.Fatal("expected error for nil function")
	}
}

func TestLowerCallsNilTargetError(t *testing.T) {
	fn := mir.NewFunction("f", nil)
	if err := lower.LowerCallsInFunction(fn, nil); err == nil {
		t.Fatal("expected error for nil target")
	}
}

func TestLowerCallsNilProgram(t *testing.T) {
	prog, err := lower.LowerCallsInProgram(nil, target.NewRISCV64Target())
	if err != nil {
		t.Fatalf("expected success for nil program, got: %v", err)
	}
	if prog == nil {
		t.Fatal("expected empty program, got nil")
	}
}

func TestLowerCallsProgramNilTarget(t *testing.T) {
	if _, err := lower.LowerCallsInProgram(mir.NewProgram(), nil); err == nil {
		t.Fatal("expected error for nil target")
	}
}
