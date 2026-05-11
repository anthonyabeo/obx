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
