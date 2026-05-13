package mir

import "testing"

func TestInstrDefsUses(t *testing.T) {
	dst := NewRegister("v0", VirtualReg, NewScalarType("i32", 4))
	lhs := NewRegister("v1", VirtualReg, NewScalarType("i32", 4))
	rhs := NewImmediate(7, NewScalarType("i32", 4))

	bin := &BinaryInstr{Dst: dst, Op: "add", Left: lhs, Right: rhs}
	defs := bin.Defs()
	if len(defs) != 1 || defs[0] != dst {
		t.Fatalf("BinaryInstr.Defs() = %#v, want [%p]", defs, dst)
	}
	uses := bin.Uses()
	if len(uses) != 2 || uses[0] != lhs || uses[1] != rhs {
		t.Fatalf("BinaryInstr.Uses() = %#v, want [%p %p]", uses, lhs, rhs)
	}

	cmp := &CompareInstr{Dst: dst, Pred: "eq", Left: lhs, Right: rhs}
	if got := cmp.String(); got != "cmp.eq v0, v1, 7" {
		t.Fatalf("CompareInstr.String() = %q", got)
	}
}

func TestOperandTypes(t *testing.T) {
	i32 := NewScalarType("i32", 4)
	reg := NewRegister("v0", VirtualReg, i32)
	if reg.Type() != i32 {
		t.Fatalf("Register.Type() = %#v, want %#v", reg.Type(), i32)
	}
	if (*Register)(nil).Type() != nil {
		t.Fatalf("nil Register.Type() should be nil")
	}

	imm := NewImmediate(7, i32)
	if imm.Type() != i32 {
		t.Fatalf("Immediate.Type() = %#v, want %#v", imm.Type(), i32)
	}

	mem := NewMemory(reg, imm, i32)
	if mem.Type() != i32 {
		t.Fatalf("Memory.Type() = %#v, want %#v", mem.Type(), i32)
	}

	sym := NewSymbol("g0", i32)
	if sym.Type() != i32 {
		t.Fatalf("Symbol.Type() = %#v, want %#v", sym.Type(), i32)
	}

	if lbl := NewLabel("L1"); lbl.Type() != nil {
		t.Fatalf("Label.Type() = %#v, want nil", lbl.Type())
	}
}

func TestBlockAndFunctionHelpers(t *testing.T) {
	fn := NewFunction("Foo", NewScalarType("i32", 4))
	entry := NewBlock(0, "entry")
	exit := NewBlock(1, "exit")

	fn.AddBlock(entry)
	fn.AddBlock(exit)
	fn.SetEntry(entry)
	fn.SetExit(exit)

	mov := &MoveInstr{Dst: NewRegister("v0", VirtualReg, NewScalarType("i32", 4)), Src: NewImmediate(1, NewScalarType("i32", 4))}
	entry.AddInstr(mov)
	jump := &JumpInstr{Target: "exit"}
	entry.SetTerminator(jump)
	entry.AddSucc(exit)
	exit.AddPred(entry)

	if fn.BlockByLabel("entry") != entry {
		t.Fatalf("BlockByLabel(entry) failed")
	}
	if !entry.HasSucc(exit) || !exit.HasPred(entry) {
		t.Fatalf("CFG wiring failed: entry=%+v exit=%+v", entry.Succs, exit.Preds)
	}
	if entry.Term != jump {
		t.Fatalf("expected terminator to be set")
	}
}

func TestMachineInstrDefsUses(t *testing.T) {
	dst := NewRegister("a0", PhysicalReg, NewScalarType("i32", 4))
	src1 := NewRegister("a1", PhysicalReg, NewScalarType("i32", 4))
	src2 := NewImmediate(8, NewScalarType("i32", 4))

	mi := NewMachineInstr("addi", []*Register{dst}, []Operand{src1, src2})

	if defs := mi.Defs(); len(defs) != 1 || defs[0] != dst {
		t.Fatalf("MachineInstr.Defs() = %v, want [a0]", defs)
	}
	if uses := mi.Uses(); len(uses) != 2 || uses[0] != src1 || uses[1] != src2 {
		t.Fatalf("MachineInstr.Uses() = %v, want [a1, 8]", uses)
	}
	if got, want := mi.String(), "addi a0, a1, 8"; got != want {
		t.Fatalf("MachineInstr.String() = %q, want %q", got, want)
	}
}

func TestMachineInstrNoOperands(t *testing.T) {
	nop := NewMachineInstr("nop", nil, nil)
	if got := nop.String(); got != "nop" {
		t.Fatalf("nop.String() = %q, want %q", got, "nop")
	}
	if got := len(nop.Defs()); got != 0 {
		t.Fatalf("nop.Defs() len = %d, want 0", got)
	}
}

func TestMachineTermDefsUses(t *testing.T) {
	cond := NewRegister("t0", PhysicalReg, NewScalarType("i1", 1))
	mt := NewMachineTerm("beqz", []Operand{cond}, []string{"then", "else"})

	if mt.Defs() != nil {
		t.Fatalf("MachineTerm.Defs() should be nil")
	}
	if uses := mt.Uses(); len(uses) != 1 || uses[0] != cond {
		t.Fatalf("MachineTerm.Uses() = %v, want [t0]", uses)
	}
	if got, want := mt.String(), "beqz t0, then, else"; got != want {
		t.Fatalf("MachineTerm.String() = %q, want %q", got, want)
	}
}

func TestMachineTermNoOperands(t *testing.T) {
	ret := NewMachineTerm("ret", nil, nil)
	if got := ret.String(); got != "ret" {
		t.Fatalf("ret.String() = %q, want %q", got, "ret")
	}
}

func TestFrameLayout(t *testing.T) {
	fn := NewFunction("main", NewScalarType("i32", 4))
	if fn.Frame != nil {
		t.Fatalf("Frame should be nil before regalloc")
	}

	fn.Frame = NewFrameLayout()
	fn.Frame.TotalSize = 32
	fn.Frame.SavedRegs = []string{"ra", "s0"}
	fn.Frame.SpillSlots["v0"] = -8
	fn.Frame.SpillSlots["v1"] = -16

	if fn.Frame.TotalSize != 32 {
		t.Fatalf("TotalSize = %d, want 32", fn.Frame.TotalSize)
	}
	if off, ok := fn.Frame.SpillSlots["v0"]; !ok || off != -8 {
		t.Fatalf("SpillSlots[v0] = %d, want -8", off)
	}
}

func TestModuleGrouping(t *testing.T) {
	prog := NewProgram()
	mod := NewModule("Main")
	g := NewGlobalDecl("x", NewScalarType("i32", 4), InternalLinkage, NewImmediate(0, NewScalarType("i32", 4)))
	e := NewExternDecl("printf", nil)
	c := NewConstDecl("msg", NewArrayType(NewScalarType("i32", 4), 4), NewImmediate("hello", nil), PrivateLinkage)
	fn := NewFunction("main", nil)

	mod.AddGlobal(g)
	mod.AddExtern(e)
	mod.AddConst(c)
	mod.AddFunction(fn)
	prog.AddModule(mod)

	if prog.ModuleByName("Main") != mod {
		t.Fatalf("ModuleByName(Main) failed")
	}
	if mod.GlobalByName("x") != g || mod.ExternByName("printf") != e || mod.ConstByName("msg") != c || mod.FunctionByName("main") != fn {
		t.Fatalf("module indexes are not wired correctly")
	}
}
