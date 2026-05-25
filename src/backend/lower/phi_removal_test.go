package lower

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// a function with a phi over two predecessors should produce MoveInstr on
// each predecessor edge and have the phi stripped from the join block;
// a function with no phis should pass through unchanged.
func TestPhiRemovalInFunctionSinglePhi(t *testing.T) {
	entryBlock := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "Merge"},
		},
	}

	L1 := &mir.Block{
		ID:    1,
		Label: "L1",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "Merge"},
		},
	}

	i32 := mir.NewScalarType("i32", 4)
	t1 := &mir.Register{Name: "t1", Ty: i32}
	MergeBlock := &mir.Block{
		ID:    2,
		Label: "Merge",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: t1,
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: &mir.Register{Name: "t2", Ty: i32}},
					{BlockLabel: "L1", Value: &mir.Register{Name: "t3", Ty: i32}},
				},
			},
			&mir.ReturnInstr{Value: t1},
		},
	}

	entryBlock.Term = entryBlock.Instrs[len(entryBlock.Instrs)-1].(*mir.JumpInstr)
	L1.Term = L1.Instrs[len(L1.Instrs)-1].(*mir.JumpInstr)
	MergeBlock.Term = MergeBlock.Instrs[len(MergeBlock.Instrs)-1].(*mir.ReturnInstr)

	fn := mir.NewFunction("f", i32)
	fn.SetEntry(entryBlock)
	fn.SetExit(MergeBlock)

	fn.AddBlock(entryBlock)
	fn.AddBlock(L1)
	fn.AddBlock(MergeBlock)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	entry := fn.Blocks[0]
	if len(entry.Instrs) != 2 {
		t.Fatalf("entry block: expected 2 instrs, got %d", len(entry.Instrs))
	}

	s1, ok := entry.Instrs[0].(*mir.MoveInstr)
	if !ok {
		t.Fatalf("entry block: expected MoveInstr, got %T", entry.Instrs[0])
	}

	if s1.String() != "mov t1, t2" {
		t.Fatalf("entry block: expected mov t1, t2, got %s", s1.String())
	}

	s2, ok := entry.Instrs[1].(*mir.JumpInstr)
	if !ok {
		t.Fatalf("entry block: expected JumpInstr, got %T", entry.Instrs[1])
	}

	if s2.String() != "jmp Merge" {
		t.Fatalf("entry block: expected jmp Merge, got %s", s2.String())
	}

	L1Instrs := fn.Blocks[1].Instrs
	if len(L1Instrs) != 2 {
		t.Fatalf("L1 block: expected 2 instrs, got %d", len(L1Instrs))
	}

	s3, ok := L1Instrs[0].(*mir.MoveInstr)
	if !ok {
		t.Fatalf("L1 block: expected MoveInstr, got %T", L1Instrs[0])
	}

	if s3.String() != "mov t1, t3" {
		t.Fatalf("L1 block: expected mov t1, t3, got %s", s3.String())
	}

	s4, ok := L1Instrs[1].(*mir.JumpInstr)
	if !ok {
		t.Fatalf("L1 block: expected JumpInstr, got %T", L1Instrs[1])
	}

	if s4.String() != "jmp Merge" {
		t.Fatalf("L1 block: expected jmp Merge, got %s", s4.String())
	}

	MergeInstrs := fn.Blocks[2].Instrs
	if len(MergeInstrs) != 1 {
		t.Fatalf("Merge block: expected 1 instr, got %d", len(MergeInstrs))
	}

	ret, ok := MergeInstrs[0].(*mir.ReturnInstr)
	if !ok {
		t.Fatalf("Merge block: expected ReturnInstr, got %T", MergeInstrs[0])
	}

	if ret.String() != "ret t1" {
		t.Fatalf("Merge block: expected ret t1, got %s", ret.String())
	}
}

// func Main$fib(%n: i32) -> i32
// entry:
//
//	%t33 = icmp.sle i32 %n, 0
//	br %t33, Main$fib_exit, if_elif_1.2
//
// if_elif_1.2:
//
//	%t34 = icmp.eq i32 %n, 1
//	br %t34, Main$fib_exit, loop.while_loop_0
//
// loop.while_loop_0:
//
//	%t54 = phi i32 [ %t44, if_end.7 ], [ 2, if_elif_1.2 ] count
//	%t53 = phi i32 [ %t40, if_end.7 ], [ 0, if_elif_1.2 ] temp
//	%t52 = phi i32 [ %t40, if_end.7 ], [ 1, if_elif_1.2 ] b
//	%t51 = phi i32 [ %t52, if_end.7 ], [ 0, if_elif_1.2 ] a
//	%t36 = icmp.sle i32 %t54, %n
//	%t37 = xor i1 %t36, true
//	br %t37, Main$fib_exit, if_end.7
//
// Main$fib_exit:
//
//	%t46 = phi i32 [ 0, entry ], [ 1, if_elif_1.2 ], [ %t52, loop.while_loop_0 ]
//	ret %t46
//
// if_end.7:
//
//	%t40 = add i32 %t51, %t52
//	%t44 = add i32 %t54, 1
//	jmp loop.while_loop_0
func TestPhiRemovalInFunctionMultiPhi(t *testing.T) {
	i1 := mir.NewScalarType("i1", 1)
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string, ty *mir.Type) *mir.Register {
		return &mir.Register{
			Name: name,
			Kind: mir.VirtualReg,
			Ty:   ty,
		}
	}

	n := &mir.Symbol{Name: "n", Ty: i32}
	zero := mir.NewImmediate(0, i32)
	one := mir.NewImmediate(1, i32)
	t33 := reg("t33", i1)
	t34 := reg("t34", i1)

	entryBlock := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.CompareInstr{Dst: t33, Pred: "sle", Left: n, Right: zero},
			&mir.CondBrInstr{Cond: t33, TrueLabel: "Main$fib_exit", FalseLabel: "if_elif_1.2"},
		},
	}
	entryBlock.Term = entryBlock.Instrs[len(entryBlock.Instrs)-1].(*mir.CondBrInstr)

	if_elif_1_2 := &mir.Block{
		ID:    1,
		Label: "if_elif_1.2",
		Instrs: []mir.Instr{
			&mir.CompareInstr{Dst: t34, Pred: "eq", Left: n, Right: one},
			&mir.CondBrInstr{Cond: t34, TrueLabel: "Main$fib_exit", FalseLabel: "loop.while_loop_0"},
		},
	}
	if_elif_1_2.Term = if_elif_1_2.Instrs[len(if_elif_1_2.Instrs)-1].(*mir.CondBrInstr)

	loop_while_loop_0 := &mir.Block{
		ID:    2,
		Label: "loop.while_loop_0",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t54", i32),
				Arms: []mir.PhiArm{
					{BlockLabel: "if_end.7", Value: reg("t44", i32)},
					{BlockLabel: "if_elif_1.2", Value: mir.NewImmediate(2, i32)},
				},
			},
			&mir.PhiInstr{
				Dst: reg("t53", i32),
				Arms: []mir.PhiArm{
					{BlockLabel: "if_end.7", Value: reg("t40", i32)},
					{BlockLabel: "if_elif_1.2", Value: zero},
				},
			},
			&mir.PhiInstr{
				Dst: reg("t52", i32),
				Arms: []mir.PhiArm{
					{BlockLabel: "if_end.7", Value: reg("t40", i32)},
					{BlockLabel: "if_elif_1.2", Value: one},
				},
			},
			&mir.PhiInstr{
				Dst: reg("t51", i32),
				Arms: []mir.PhiArm{
					{BlockLabel: "if_end.7", Value: reg("t52", i32)},
					{BlockLabel: "if_elif_1.2", Value: zero},
				},
			},
			&mir.CompareInstr{Dst: reg("t36", i1), Pred: "sle", Left: reg("t54", i32), Right: n},
			&mir.BinaryInstr{Dst: reg("t37", i1), Op: "xor", Left: reg("t36", i1), Right: mir.NewImmediate(true, i1)},
			&mir.CondBrInstr{Cond: reg("t37", i1), TrueLabel: "Main$fib_exit", FalseLabel: "if_end.7"},
		},
	}
	loop_while_loop_0.Term = loop_while_loop_0.Instrs[len(loop_while_loop_0.Instrs)-1].(*mir.CondBrInstr)

	main_fib_exit := &mir.Block{
		ID:    3,
		Label: "Main$fib_exit",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t46", i32),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: zero},
					{BlockLabel: "if_elif_1.2", Value: one},
					{BlockLabel: "loop.while_loop_0", Value: reg("t52", i32)},
				},
			},
			&mir.ReturnInstr{Value: reg("t46", i32)},
		},
	}
	main_fib_exit.Term = main_fib_exit.Instrs[len(main_fib_exit.Instrs)-1].(*mir.ReturnInstr)

	if_end_7 := &mir.Block{
		ID:    4,
		Label: "if_end.7",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{Dst: reg("t40", i32), Op: "add", Left: reg("t51", i32), Right: reg("t52", i32)},
			&mir.BinaryInstr{Dst: reg("t44", i32), Op: "add", Left: reg("t54", i32), Right: mir.NewImmediate(1, i32)},
			&mir.JumpInstr{Target: "loop.while_loop_0"},
		},
	}
	if_end_7.Term = if_end_7.Instrs[len(if_end_7.Instrs)-1].(*mir.JumpInstr)

	fn := mir.NewFunction("f", i32)
	fn.SetEntry(entryBlock)
	fn.SetExit(main_fib_exit)
	fn.AddBlock(entryBlock)
	fn.AddBlock(if_elif_1_2)
	fn.AddBlock(loop_while_loop_0)
	fn.AddBlock(main_fib_exit)
	fn.AddBlock(if_end_7)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	entry := fn.Blocks[0]
	if len(entry.Instrs) != 3 {
		t.Fatalf("entry block: expected 3 instructions, got %d", len(entry.Instrs))
	}

	ifelif_1_2 := fn.Blocks[1]
	if len(ifelif_1_2.Instrs) != 7 {
		t.Fatalf("if_elif_1.2: expected 7, got %d instructions", len(if_elif_1_2.Instrs))
	}

	loop_while := fn.Blocks[2]
	if len(loop_while.Instrs) != 4 {
		t.Fatalf("loop_while_loop_0 block: expected 4 instructions, got %d", len(loop_while.Instrs))
	}

	if_end := fn.Blocks[4]
	if len(if_end.Instrs) != 7 {
		t.Fatalf("if_end.7 block: expected 7 instructions, got %d", len(if_end.Instrs))
	}

	exit := fn.Blocks[3]
	if len(exit.Instrs) != 1 {
		t.Fatalf("Main$fib_exit block: expected 1 instruction, got %d", len(exit.Instrs))
	}

	fmt.Println(mir.FormatFunction(fn))
}

// TestPhiRemovalSimpleDiamondControlFlow tests phi removal on a simple
// if-then-else diamond: entry -> if/else -> join
func TestPhiRemovalSimpleDiamondControlFlow(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)
	i1 := mir.NewScalarType("i1", 1)

	reg := func(name string, ty *mir.Type, kind mir.RegKind) *mir.Register {
		return &mir.Register{Name: name, Kind: kind, Ty: ty}
	}

	// entry: cond = lhs < rhs; br cond, then, else
	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.CompareInstr{Dst: reg("cond", i1, mir.VirtualReg), Pred: "slt", Left: reg("lhs", i32, mir.VirtualReg), Right: reg("rhs", i32, mir.VirtualReg)},
			&mir.CondBrInstr{Cond: reg("cond", i1, mir.VirtualReg), TrueLabel: "then", FalseLabel: "else"},
		},
	}
	entry.Term = entry.Instrs[1].(*mir.CondBrInstr)

	// then: thenVal = add lhs, 1; jmp join
	then := &mir.Block{
		ID:    1,
		Label: "then",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{Dst: reg("thenVal", i32, mir.VirtualReg), Op: "add", Left: reg("lhs", i32, mir.VirtualReg), Right: mir.NewImmediate(1, i32)},
			&mir.JumpInstr{Target: "join"},
		},
	}
	then.Term = then.Instrs[1].(*mir.JumpInstr)

	// else: elseVal = sub rhs, 1; jmp join
	elseBlk := &mir.Block{
		ID:    2,
		Label: "else",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{Dst: reg("elseVal", i32, mir.VirtualReg), Op: "sub", Left: reg("rhs", i32, mir.VirtualReg), Right: mir.NewImmediate(1, i32)},
			&mir.JumpInstr{Target: "join"},
		},
	}
	elseBlk.Term = elseBlk.Instrs[1].(*mir.JumpInstr)

	// join: result = phi(thenVal, elseVal); ret result
	join := &mir.Block{
		ID:    3,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("result", i32, mir.VirtualReg),
				Arms: []mir.PhiArm{
					{BlockLabel: "then", Value: reg("thenVal", i32, mir.VirtualReg)},
					{BlockLabel: "else", Value: reg("elseVal", i32, mir.VirtualReg)},
				},
			},
			&mir.ReturnInstr{Value: reg("result", i32, mir.VirtualReg)},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("diamond", i32)
	fn.SetEntry(entry)
	fn.SetExit(join)
	fn.AddBlock(entry)
	fn.AddBlock(then)
	fn.AddBlock(elseBlk)
	fn.AddBlock(join)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// then block should have 2 instrs (binary + jump) -> 3 (binary + move + jump)
	if len(then.Instrs) != 3 {
		t.Fatalf("then block: expected 3 instrs, got %d", len(then.Instrs))
	}
	if _, ok := then.Instrs[1].(*mir.MoveInstr); !ok {
		t.Fatalf("then block: expected MoveInstr at index 1, got %T", then.Instrs[1])
	}

	// else block should have 2 instrs (binary + jump) -> 3 (binary + move + jump)
	if len(elseBlk.Instrs) != 3 {
		t.Fatalf("else block: expected 3 instrs, got %d", len(elseBlk.Instrs))
	}
	if _, ok := elseBlk.Instrs[1].(*mir.MoveInstr); !ok {
		t.Fatalf("else block: expected MoveInstr at index 1, got %T", elseBlk.Instrs[1])
	}

	// join block should have 1 instr (phi removed, only ret remains)
	if len(join.Instrs) != 1 {
		t.Fatalf("join block: expected 1 instr, got %d", len(join.Instrs))
	}
	if _, ok := join.Instrs[0].(*mir.ReturnInstr); !ok {
		t.Fatalf("join block: expected ReturnInstr, got %T", join.Instrs[0])
	}
}

// TestPhiRemovalWithCycleDetection tests that phi removal correctly handles
// circular register dependencies by inserting temporary registers.
func TestPhiRemovalWithCycleDetection(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// Create a cycle: t1 = phi(t2, ...), t2 = phi(t1, ...)
	// This requires temp register insertion.
	backedge := &mir.Block{
		ID:    0,
		Label: "backedge",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{Dst: reg("t2"), Op: "add", Left: reg("t1"), Right: mir.NewImmediate(1, i32)},
			&mir.JumpInstr{Target: "loop"},
		},
	}
	backedge.Term = backedge.Instrs[1].(*mir.JumpInstr)

	entry := &mir.Block{
		ID:    1,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "loop"},
		},
	}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)

	// Phi with circular dependency: to "backedge", we need to move t1 and t2,
	// but t2 comes from t1 within the same edge. This creates a cycle.
	loop := &mir.Block{
		ID:    2,
		Label: "loop",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t1"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: mir.NewImmediate(1, i32)},
					{BlockLabel: "backedge", Value: reg("t2")},
				},
			},
			&mir.PhiInstr{
				Dst: reg("t2"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: mir.NewImmediate(2, i32)},
					{BlockLabel: "backedge", Value: reg("t1")},
				},
			},
			&mir.CondBrInstr{Cond: reg("cond"), TrueLabel: "exit", FalseLabel: "backedge"},
		},
	}
	loop.Term = loop.Instrs[2].(*mir.CondBrInstr)

	exit := &mir.Block{
		ID:    3,
		Label: "exit",
		Instrs: []mir.Instr{
			&mir.ReturnInstr{Value: reg("t1")},
		},
	}
	exit.Term = exit.Instrs[0].(*mir.ReturnInstr)

	fn := mir.NewFunction("cycleTest", i32)
	fn.SetEntry(entry)
	fn.SetExit(exit)
	fn.AddBlock(entry)
	fn.AddBlock(loop)
	fn.AddBlock(backedge)
	fn.AddBlock(exit)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// After phi removal, phis should be gone from loop block
	if len(loop.Instrs) != 1 {
		t.Fatalf("loop block: expected 1 instr after phi removal, got %d", len(loop.Instrs))
	}
	if _, ok := loop.Instrs[0].(*mir.CondBrInstr); !ok {
		t.Fatalf("loop block: expected CondBrInstr remaining, got %T", loop.Instrs[0])
	}

	// backedge should have 2 original instrs + moves for the cycle (at least 2 more)
	// With cycle detection, we need: tmp = t2; t2 = t1; t1 = tmp (3 moves)
	// Plus the original binary + jump
	if len(backedge.Instrs) < 5 {
		t.Fatalf("backedge block: expected at least 5 instrs (with cycle resolution), got %d", len(backedge.Instrs))
	}
}

// TestPhiRemovalNilFunction tests that nil function is rejected
func TestPhiRemovalNilFunction(t *testing.T) {
	err := RemovePhisInFunction(nil, &mockTarget{})
	if err == nil {
		t.Fatalf("expected error for nil function, got nil")
	}
}

// TestPhiRemovalSingleBlockNoPhis tests function with single block and no phis
func TestPhiRemovalSingleBlockNoPhis(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	blk := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{
				Dst:   &mir.Register{Name: "t1", Kind: mir.VirtualReg, Ty: i32},
				Op:    "add",
				Left:  &mir.Register{Name: "a", Kind: mir.VirtualReg, Ty: i32},
				Right: &mir.Register{Name: "b", Kind: mir.VirtualReg, Ty: i32},
			},
			&mir.ReturnInstr{Value: &mir.Register{Name: "t1", Kind: mir.VirtualReg, Ty: i32}},
		},
	}
	blk.Term = blk.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("nophis", i32)
	fn.SetEntry(blk)
	fn.SetExit(blk)
	fn.AddBlock(blk)

	origLen := len(blk.Instrs)
	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Block should be unchanged
	if len(blk.Instrs) != origLen {
		t.Fatalf("block should be unchanged: expected %d instrs, got %d", origLen, len(blk.Instrs))
	}
}

// TestPhiRemovalMultiplePhiBlocksInFunction tests function with phis in multiple blocks
func TestPhiRemovalMultiplePhiBlocksInFunction(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// entry: jmp join1
	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join1"},
		},
	}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)

	// path1: jmp join1
	path1 := &mir.Block{
		ID:    1,
		Label: "path1",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join1"},
		},
	}
	path1.Term = path1.Instrs[0].(*mir.JumpInstr)

	// join1: t1 = phi(entry, path1); jmp join2
	join1 := &mir.Block{
		ID:    2,
		Label: "join1",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t1"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: mir.NewImmediate(1, i32)},
					{BlockLabel: "path1", Value: mir.NewImmediate(2, i32)},
				},
			},
			&mir.JumpInstr{Target: "join2"},
		},
	}
	join1.Term = join1.Instrs[1].(*mir.JumpInstr)

	// path2: jmp join2
	path2 := &mir.Block{
		ID:    3,
		Label: "path2",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join2"},
		},
	}
	path2.Term = path2.Instrs[0].(*mir.JumpInstr)

	// join2: t2 = phi(join1, path2); ret t2
	join2 := &mir.Block{
		ID:    4,
		Label: "join2",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t2"),
				Arms: []mir.PhiArm{
					{BlockLabel: "join1", Value: reg("t1")},
					{BlockLabel: "path2", Value: mir.NewImmediate(3, i32)},
				},
			},
			&mir.ReturnInstr{Value: reg("t2")},
		},
	}
	join2.Term = join2.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("multiPhi", i32)
	fn.SetEntry(entry)
	fn.SetExit(join2)
	fn.AddBlock(entry)
	fn.AddBlock(path1)
	fn.AddBlock(join1)
	fn.AddBlock(path2)
	fn.AddBlock(join2)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// entry and path1 should each have: original jmp + move -> 2 instrs
	if len(entry.Instrs) != 2 {
		t.Fatalf("entry: expected 2 instrs, got %d", len(entry.Instrs))
	}
	if len(path1.Instrs) != 2 {
		t.Fatalf("path1: expected 2 instrs, got %d", len(path1.Instrs))
	}

	// join1 should have: move (for entry/path1 phis) + jmp + move (for join2 phi) -> 3 instrs
	// Actually: phiMoves from phi1 + jmp + phiMoves from phi2
	// The actual structure after phi removal: we add moves before terminators
	if len(join1.Instrs) < 2 {
		t.Fatalf("join1: expected at least 2 instrs, got %d", len(join1.Instrs))
	}

	// path2 should have: jmp + move (for join2 phi) -> 2 instrs
	if len(path2.Instrs) != 2 {
		t.Fatalf("path2: expected 2 instrs, got %d", len(path2.Instrs))
	}

	// join2 should have: ret only (phi removed, moves added to predecessors)
	// The phi destination becomes available through moves in predecessor blocks
	if len(join2.Instrs) != 1 {
		t.Fatalf("join2: expected 1 instr (ret only), got %d", len(join2.Instrs))
	}
}

// TestPhiRemovalMixedSourceTypes tests phi with both register and immediate sources
func TestPhiRemovalMixedSourceTypes(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// entry: jmp join
	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.BinaryInstr{Dst: reg("regVal"), Op: "add", Left: reg("x"), Right: mir.NewImmediate(10, i32)},
			&mir.JumpInstr{Target: "join"},
		},
	}
	entry.Term = entry.Instrs[1].(*mir.JumpInstr)

	// alt: jmp join
	alt := &mir.Block{
		ID:    1,
		Label: "alt",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join"},
		},
	}
	alt.Term = alt.Instrs[0].(*mir.JumpInstr)

	// join: result = phi([regVal from entry], [42 immediate from alt]); ret result
	join := &mir.Block{
		ID:    2,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("result"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: reg("regVal")},
					{BlockLabel: "alt", Value: mir.NewImmediate(42, i32)},
				},
			},
			&mir.ReturnInstr{Value: reg("result")},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("mixedSources", i32)
	fn.SetEntry(entry)
	fn.SetExit(join)
	fn.AddBlock(entry)
	fn.AddBlock(alt)
	fn.AddBlock(join)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// entry: [binary, move, jmp] (move added for phi result)
	if len(entry.Instrs) != 3 {
		t.Fatalf("entry: expected 3 instrs, got %d", len(entry.Instrs))
	}
	if _, ok := entry.Instrs[1].(*mir.MoveInstr); !ok {
		t.Fatalf("entry: expected MoveInstr at index 1, got %T", entry.Instrs[1])
	}

	// alt: [jmp, move] (move added for immediate 42)
	if len(alt.Instrs) != 2 {
		t.Fatalf("alt: expected 2 instrs, got %d", len(alt.Instrs))
	}
	if _, ok := alt.Instrs[0].(*mir.MoveInstr); !ok {
		t.Fatalf("alt: expected MoveInstr at index 0, got %T", alt.Instrs[0])
	}

	// join: [ret] (phi removed)
	if len(join.Instrs) != 1 {
		t.Fatalf("join: expected 1 instr, got %d", len(join.Instrs))
	}
}

// TestPhiRemovalProgram tests RemovePhisInProgram on a multi-module/function program
func TestPhiRemovalProgram(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// Function 1: no phis
	fn1Entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.ReturnInstr{Value: mir.NewImmediate(1, i32)},
		},
	}
	fn1Entry.Term = fn1Entry.Instrs[0].(*mir.ReturnInstr)
	fn1 := mir.NewFunction("fn1", i32)
	fn1.SetEntry(fn1Entry)
	fn1.SetExit(fn1Entry)
	fn1.AddBlock(fn1Entry)

	// Function 2: has phi
	entry := &mir.Block{ID: 0, Label: "entry", Instrs: []mir.Instr{&mir.JumpInstr{Target: "join"}}}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)
	alt := &mir.Block{ID: 1, Label: "alt", Instrs: []mir.Instr{&mir.JumpInstr{Target: "join"}}}
	alt.Term = alt.Instrs[0].(*mir.JumpInstr)
	join := &mir.Block{
		ID:    2,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: mir.NewImmediate(1, i32)},
					{BlockLabel: "alt", Value: mir.NewImmediate(2, i32)},
				},
			},
			&mir.ReturnInstr{Value: reg("t")},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn2 := mir.NewFunction("fn2", i32)
	fn2.SetEntry(entry)
	fn2.SetExit(join)
	fn2.AddBlock(entry)
	fn2.AddBlock(alt)
	fn2.AddBlock(join)

	// Create module and program
	mod := mir.NewModule("TestMod")
	mod.AddFunction(fn1)
	mod.AddFunction(fn2)

	prog := mir.NewProgram()
	prog.AddModule(mod)

	// Apply RemovePhisInProgram
	_, err := RemovePhisInProgram(prog, nil) // Target is nil, but pass-through should work
	if err == nil {
		t.Fatalf("expected error for nil target, got nil")
	}

	// Now with proper target
	btarget := "riscv64" // dummy for now
	prog2 := mir.NewProgram()
	prog2.AddModule(mod)
	_, err = RemovePhisInProgram(prog2, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// fn2's join block should have phi removed
	if len(join.Instrs) != 1 {
		t.Fatalf("fn2 join: expected 1 instr after phi removal, got %d", len(join.Instrs))
	}

	_ = btarget // silence unused variable
}

// mockTarget provides a minimal Target implementation for testing
type mockTarget struct{}

func (m *mockTarget) Name() string { return "mock" }

func (m *mockTarget) ABIInfo() target.ABI {
	return target.ABI{Name: "test", WordSize: 8, Align: 8}
}

func (m *mockTarget) SupportsIntegerScalar(ty *mir.Type) bool { return true }

func (m *mockTarget) LowerCall(c *mir.CallInstr) (*target.CallPlan, error) { return nil, nil }

func (m *mockTarget) LowerSwitch(s *mir.SwitchInstr) (*target.SwitchPlan, error) { return nil, nil }

func (m *mockTarget) LowerPhiBlock(label string, phis []*mir.PhiInstr) (*target.PhiPlan, error) {
	return target.BuildPhiPlan(label, phis)
}

func (m *mockTarget) Emit(mod *mir.Module) string { return "" }

// TestPhiRemovalComplexNesting tests phi removal with deeply nested control flow
func TestPhiRemovalComplexNesting(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// Create nested ifs: entry -> if1 -> if2 -> join2 -> join1
	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "if1_then"},
		},
	}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)

	if1_then := &mir.Block{
		ID:    1,
		Label: "if1_then",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "if2_then"},
		},
	}
	if1_then.Term = if1_then.Instrs[0].(*mir.JumpInstr)

	if2_else := &mir.Block{
		ID:    2,
		Label: "if2_else",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join2"},
		},
	}
	if2_else.Term = if2_else.Instrs[0].(*mir.JumpInstr)

	// join2: inner merge with phi
	join2 := &mir.Block{
		ID:    3,
		Label: "join2",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("a"),
				Arms: []mir.PhiArm{
					{BlockLabel: "if1_then", Value: mir.NewImmediate(1, i32)},
					{BlockLabel: "if2_else", Value: mir.NewImmediate(2, i32)},
				},
			},
			&mir.JumpInstr{Target: "join1"},
		},
	}
	join2.Term = join2.Instrs[1].(*mir.JumpInstr)

	if1_else := &mir.Block{
		ID:    4,
		Label: "if1_else",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join1"},
		},
	}
	if1_else.Term = if1_else.Instrs[0].(*mir.JumpInstr)

	// join1: outer merge with phi
	join1 := &mir.Block{
		ID:    5,
		Label: "join1",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("result"),
				Arms: []mir.PhiArm{
					{BlockLabel: "join2", Value: reg("a")},
					{BlockLabel: "if1_else", Value: mir.NewImmediate(3, i32)},
				},
			},
			&mir.ReturnInstr{Value: reg("result")},
		},
	}
	join1.Term = join1.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("nested", i32)
	fn.SetEntry(entry)
	fn.SetExit(join1)
	fn.AddBlock(entry)
	fn.AddBlock(if1_then)
	fn.AddBlock(if2_else)
	fn.AddBlock(join2)
	fn.AddBlock(if1_else)
	fn.AddBlock(join1)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify both phis are removed
	for _, blk := range fn.Blocks {
		for _, instr := range blk.Instrs {
			if _, ok := instr.(*mir.PhiInstr); ok {
				t.Fatalf("block %s still has phi instruction after removal", blk.Label)
			}
		}
	}

	// Verify joins only have non-phi instructions
	// join2 has: phi removal was executed, but it becomes a source for join1's phi,
	// so join2 will have moves added before its terminator
	if len(join2.Instrs) < 1 {
		t.Fatalf("join2: expected at least 1 instr after phi removal, got %d", len(join2.Instrs))
	}
	// join1 should have moves added plus its terminator
	if len(join1.Instrs) < 1 {
		t.Fatalf("join1: expected at least 1 instr after phi removal, got %d", len(join1.Instrs))
	}
}

// TestPhiRemovalLargePredecesorCount tests phi with many predecessors
func TestPhiRemovalLargePredecesorCount(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	// Create 5 predecessor blocks
	predBlocks := make([]*mir.Block, 5)
	arms := make([]mir.PhiArm, 5)
	for i := 0; i < 5; i++ {
		label := fmt.Sprintf("pred%d", i)
		predBlocks[i] = &mir.Block{
			ID:    i,
			Label: label,
			Instrs: []mir.Instr{
				&mir.JumpInstr{Target: "join"},
			},
		}
		predBlocks[i].Term = predBlocks[i].Instrs[0].(*mir.JumpInstr)

		arms[i] = mir.PhiArm{
			BlockLabel: label,
			Value:      mir.NewImmediate(int64(i+1), i32),
		}
	}

	// join: result = phi(1, 2, 3, 4, 5); ret result
	join := &mir.Block{
		ID:    100,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst:  reg("result"),
				Arms: arms,
			},
			&mir.ReturnInstr{Value: reg("result")},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("multiPred", i32)
	fn.SetEntry(predBlocks[0])
	fn.SetExit(join)
	for _, pb := range predBlocks {
		fn.AddBlock(pb)
	}
	fn.AddBlock(join)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Each predecessor should have: original jump + phi move -> 2 instrs
	for i, pb := range predBlocks {
		if len(pb.Instrs) != 2 {
			t.Fatalf("pred%d: expected 2 instrs, got %d", i, len(pb.Instrs))
		}
		if _, ok := pb.Instrs[0].(*mir.MoveInstr); !ok {
			t.Fatalf("pred%d: expected MoveInstr at index 0, got %T", i, pb.Instrs[0])
		}
	}

	// join should have only ret (phi removed)
	if len(join.Instrs) != 1 {
		t.Fatalf("join: expected 1 instr, got %d", len(join.Instrs))
	}
}

// TestPhiRemovalEmptyProgram tests removing phis from empty program
func TestPhiRemovalEmptyProgram(t *testing.T) {
	prog := mir.NewProgram()
	result, err := RemovePhisInProgram(prog, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error on empty program: %v", err)
	}
	if result == nil {
		t.Fatalf("expected non-nil result for empty program")
	}
}

// TestPhiRemovalImmediateAndRegisterSources tests phi with immediate and register sources
func TestPhiRemovalImmediateAndRegisterSources(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join"},
		},
	}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)

	alt := &mir.Block{
		ID:    1,
		Label: "alt",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join"},
		},
	}
	alt.Term = alt.Instrs[0].(*mir.JumpInstr)

	// phi with immediate and register sources
	join := &mir.Block{
		ID:    2,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: reg("t"),
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: mir.NewImmediate(42, i32)},
					{BlockLabel: "alt", Value: reg("regVal")},
				},
			},
			&mir.ReturnInstr{Value: reg("t")},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("mixedImmAndReg", i32)
	fn.SetEntry(entry)
	fn.SetExit(join)
	fn.AddBlock(entry)
	fn.AddBlock(alt)
	fn.AddBlock(join)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// entry should have: move + jmp -> 2 instrs
	if len(entry.Instrs) != 2 {
		t.Fatalf("entry: expected 2 instrs, got %d", len(entry.Instrs))
	}

	// alt should have: move + jmp -> 2 instrs
	if len(alt.Instrs) != 2 {
		t.Fatalf("alt: expected 2 instrs, got %d", len(alt.Instrs))
	}

	// join should have: ret only -> 1 instr
	if len(join.Instrs) != 1 {
		t.Fatalf("join: expected 1 instr, got %d", len(join.Instrs))
	}
}

// TestPhiRemovalVerifyMoveCorrectness tests that moves use correct registers
func TestPhiRemovalVerifyMoveCorrectness(t *testing.T) {
	i32 := mir.NewScalarType("i32", 4)

	reg := func(name string) *mir.Register {
		return &mir.Register{Name: name, Kind: mir.VirtualReg, Ty: i32}
	}

	entry := &mir.Block{
		ID:    0,
		Label: "entry",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join"},
		},
	}
	entry.Term = entry.Instrs[0].(*mir.JumpInstr)

	alt := &mir.Block{
		ID:    1,
		Label: "alt",
		Instrs: []mir.Instr{
			&mir.JumpInstr{Target: "join"},
		},
	}
	alt.Term = alt.Instrs[0].(*mir.JumpInstr)

	phiDst := reg("phiDst")
	entryVal := reg("entryVal")
	altVal := reg("altVal")

	join := &mir.Block{
		ID:    2,
		Label: "join",
		Instrs: []mir.Instr{
			&mir.PhiInstr{
				Dst: phiDst,
				Arms: []mir.PhiArm{
					{BlockLabel: "entry", Value: entryVal},
					{BlockLabel: "alt", Value: altVal},
				},
			},
			&mir.ReturnInstr{Value: phiDst},
		},
	}
	join.Term = join.Instrs[1].(*mir.ReturnInstr)

	fn := mir.NewFunction("correctMoves", i32)
	fn.SetEntry(entry)
	fn.SetExit(join)
	fn.AddBlock(entry)
	fn.AddBlock(alt)
	fn.AddBlock(join)

	err := RemovePhisInFunction(fn, &mockTarget{})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Check entry block move
	entryMove, ok := entry.Instrs[0].(*mir.MoveInstr)
	if !ok {
		t.Fatalf("entry: expected MoveInstr, got %T", entry.Instrs[0])
	}
	if entryMove.Dst.Name != phiDst.Name {
		t.Fatalf("entry move dst: expected %s, got %s", phiDst.Name, entryMove.Dst.Name)
	}
	if entryMove.Src.(*mir.Register).Name != entryVal.Name {
		t.Fatalf("entry move src: expected %s, got %s", entryVal.Name, entryMove.Src.(*mir.Register).Name)
	}

	// Check alt block move
	altMove, ok := alt.Instrs[0].(*mir.MoveInstr)
	if !ok {
		t.Fatalf("alt: expected MoveInstr, got %T", alt.Instrs[0])
	}
	if altMove.Dst.Name != phiDst.Name {
		t.Fatalf("alt move dst: expected %s, got %s", phiDst.Name, altMove.Dst.Name)
	}
	if altMove.Src.(*mir.Register).Name != altVal.Name {
		t.Fatalf("alt move src: expected %s, got %s", altVal.Name, altMove.Src.(*mir.Register).Name)
	}
}
