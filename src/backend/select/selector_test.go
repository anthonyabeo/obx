package selector

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

func mustSelector(t *testing.T, src string) *Selector {
	t.Helper()
	sel, err := ParseSelectorString(src)
	if err != nil {
		t.Fatalf("ParseSelectorString failed: %v", err)
	}
	return sel
}

func TestSelectorSelectInstr(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule add_rr {
				match {
					out $rd : GPR:virt;
					in $lhs : GPR:virt;
					in $rhs : GPR:virt;
					pattern add($lhs, $rhs);
				}
				cost 1;
				emit {
					instr { opcode: "add"; dst: $rd; src: [$lhs, $rhs]; };
				}
			}
		}
	`)

	inst := &mir.BinaryInstr{
		Dst:   mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Op:    "add",
		Left:  mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Right: mir.NewRegister("v2", mir.VirtualReg, mir.NewScalarType("i64", 8)),
	}

	selected, err := sel.SelectInstr(inst)
	if err != nil {
		t.Fatalf("SelectInstr failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("len(selected) = %d, want 1", len(selected))
	}
	mi, ok := selected[0].(*mir.MachineInstr)
	if !ok {
		t.Fatalf("selected[0] is %T, want *mir.MachineInstr", selected[0])
	}
	if mi.Op != "add" || len(mi.Dsts) != 1 || len(mi.Srcs) != 2 {
		t.Fatalf("selected machine instr = %#v", mi)
	}
}

func TestSelectorSelectMoveInstr(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule mov_copy {
				match {
					out $rd : GPR:virt;
					in $src : any;
					pattern mov($src);
				}
				cost 0;
				emit { instr { opcode: "mov"; dst: $rd; src: [$src]; }; }
			}
		}
	`)

	inst := &mir.MoveInstr{
		Dst: mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Src: mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
	}

	selected, err := sel.SelectInstr(inst)
	if err != nil {
		t.Fatalf("SelectInstr failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("len(selected) = %d, want 1", len(selected))
	}
	mi, ok := selected[0].(*mir.MachineInstr)
	if !ok {
		t.Fatalf("selected[0] is %T, want *mir.MachineInstr", selected[0])
	}
	if mi.Op != "mov" || len(mi.Dsts) != 1 || len(mi.Srcs) != 1 {
		t.Fatalf("selected machine instr = %#v", mi)
	}
}

func TestSelectorABIConditionalReturnLowering(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule ret_generic {
				match {
					in $rs : GPR:virt;
					pattern ret($rs);
				}
				cost 1;
				emit {
					instr { opcode: "ret"; src: [$rs]; uses: [$rs]; };
				}
			}
		}
	`)

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8))}
	out, err := sel.SelectBlock(blk)
	if err != nil {
		t.Fatalf("SelectBlock failed: %v", err)
	}
	if len(out.Instrs) != 0 {
		t.Fatalf("len(out.Instrs) = %d, want 0", len(out.Instrs))
	}
	mt, ok := out.Term.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("out.Term is %T, want *mir.MachineTerm", out.Term)
	}
	if mt.Op != "ret" || len(mt.Srcs) != 1 || mt.Srcs[0].String() != "v0" {
		t.Fatalf("return term = %#v", mt)
	}
}

func TestSelectorLegalizeMoveBeforeReturn(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule ret_legalize {
				match {
					temp $x0 : GPR:phys;
					in $v : GPR:virt;
					pattern ret($v);
				}
				legalize {
					move $v -> $x0;
				}
				cost 0;
				emit {
					instr { opcode: "ret"; src: [$v]; uses: [$v]; };
				}
			}
		}
	`)

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8))}
	out, err := sel.SelectBlock(blk)
	if err != nil {
		t.Fatalf("SelectBlock failed: %v", err)
	}
	if len(out.Instrs) != 1 {
		t.Fatalf("len(out.Instrs) = %d, want 1", len(out.Instrs))
	}
	mi, ok := out.Instrs[0].(*mir.MachineInstr)
	if !ok {
		t.Fatalf("out.Instrs[0] is %T, want *mir.MachineInstr", out.Instrs[0])
	}
	if mi.Op != "mov" || len(mi.Dsts) != 1 || mi.Dsts[0] == nil || mi.Dsts[0].Name != "x0" {
		t.Fatalf("legalize move = %#v, want mov to x0", mi)
	}
	mt, ok := out.Term.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("out.Term is %T, want *mir.MachineTerm", out.Term)
	}
	if mt.Op != "ret" || len(mt.Srcs) != 0 {
		t.Fatalf("return terminator was not normalized: %#v", mt)
	}
}

func TestSelectorLegalizeRewriteSpillReload(t *testing.T) {
	sel := mustSelector(t, `
		header { ABI: "AAPCS64"; }
		target test {
			rule legalize_misc {
				match {
					temp $x0 : GPR:phys;
					in $v : GPR:virt;
					pattern ret($v);
				}
				legalize {
					require ABIIs("AAPCS64");
					spill $v;
					reload $v;
					rewrite expand($v);
					move $v -> $x0;
				}
				cost 0;
				emit { instr { opcode: "ret"; src: [$v]; uses: [$v]; }; }
			}
		}
	`)

	blk := mir.NewBlock(0, "entry")
	blk.Term = &mir.ReturnInstr{Value: mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8))}
	out, err := sel.SelectBlock(blk)
	if err != nil {
		t.Fatalf("SelectBlock failed: %v", err)
	}
	if len(out.Instrs) != 4 {
		t.Fatalf("len(out.Instrs) = %d, want 4", len(out.Instrs))
	}
	if mi, ok := out.Instrs[0].(*mir.MachineInstr); !ok || mi.Op != "spill" {
		t.Fatalf("out.Instrs[0] = %#v, want spill pseudo-op", out.Instrs[0])
	}
	if mi, ok := out.Instrs[1].(*mir.MachineInstr); !ok || mi.Op != "reload" {
		t.Fatalf("out.Instrs[1] = %#v, want reload pseudo-op", out.Instrs[1])
	}
	if mi, ok := out.Instrs[2].(*mir.MachineInstr); !ok || mi.Op != "expand" {
		t.Fatalf("out.Instrs[2] = %#v, want rewrite pseudo-op", out.Instrs[2])
	}
	if mi, ok := out.Instrs[3].(*mir.MachineInstr); !ok || mi.Op != "mov" {
		t.Fatalf("out.Instrs[3] = %#v, want mov to ABI return register", out.Instrs[3])
	}
	mt, ok := out.Term.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("out.Term is %T, want *mir.MachineTerm", out.Term)
	}
	if mt.Op != "ret" || len(mt.Srcs) != 0 {
		t.Fatalf("return terminator was not normalized: %#v", mt)
	}
}

func TestSelectorCommutativeMatch(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule add_rr {
				match {
					commutative;
					out $rd : GPR:virt;
					in $lhs : GPR:virt;
					in $rhs : GPR:virt;
					pattern add($lhs, $rhs);
				}
				cost 1;
				emit { instr { opcode: "add"; dst: $rd; src: [$lhs, $rhs]; }; }
			}
		}
	`)

	inst := &mir.BinaryInstr{
		Dst:   mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Op:    "add",
		Left:  mir.NewRegister("v2", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Right: mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
	}

	selected, err := sel.SelectInstr(inst)
	if err != nil {
		t.Fatalf("SelectInstr failed: %v", err)
	}
	if len(selected) != 1 {
		t.Fatalf("len(selected) = %d, want 1", len(selected))
	}
	if _, ok := selected[0].(*mir.MachineInstr); !ok {
		t.Fatalf("selected[0] is %T, want *mir.MachineInstr", selected[0])
	}
}

func TestSelectorPredicateRejectsRule(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule shift_imm {
				match {
					out $rd : GPR:virt;
					in $rs : GPR:virt;
					in $sh : imm;
					pattern lshl($rs, $sh);
				}
				cost 1;
				cond ShamtFits6($sh);
				emit { instr { opcode: "slli"; dst: $rd; src: [$rs, $sh]; }; }
			}
		}
	`)

	inst := &mir.BinaryInstr{
		Dst:   mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Op:    "lshl",
		Left:  mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Right: mir.NewImmediate(64, mir.NewScalarType("i64", 8)),
	}

	selected, err := sel.SelectInstr(inst)
	if err != nil {
		t.Fatalf("SelectInstr failed: %v", err)
	}
	if len(selected) != 1 || selected[0] != inst {
		t.Fatalf("expected original instruction to be preserved when predicate fails")
	}
}

func TestSelectorSelectTerminator(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule jump {
				match {
					pattern jmp($target);
				}
				cost 1;
				emit { instr { opcode: "j"; src: [$target]; }; }
			}
		}
	`)

	term := &mir.JumpInstr{Target: "L1"}
	selected, err := sel.SelectTerminator(term)
	if err != nil {
		t.Fatalf("SelectTerminator failed: %v", err)
	}
	mt, ok := selected.(*mir.MachineTerm)
	if !ok {
		t.Fatalf("selected term is %T, want *mir.MachineTerm", selected)
	}
	if mt.Op != "j" || len(mt.Targets) != 1 || mt.Targets[0] != "L1" {
		t.Fatalf("machine term = %#v", mt)
	}
}

func TestSelectorSelectProgram(t *testing.T) {
	sel := mustSelector(t, `
		target test {
			rule add_rr {
				match {
					out $rd : GPR:virt;
					in $lhs : GPR:virt;
					in $rhs : GPR:virt;
					pattern add($lhs, $rhs);
				}
				cost 1;
				emit { instr { opcode: "add"; dst: $rd; src: [$lhs, $rhs]; }; }
			}
		}
	`)

	prog := mir.NewProgram()
	mod := mir.NewModule("m")
	fn := mir.NewFunction("f", nil)
	blk := mir.NewBlock(0, "entry")
	blk.AddInstr(&mir.BinaryInstr{
		Dst:   mir.NewRegister("v0", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Op:    "add",
		Left:  mir.NewRegister("v1", mir.VirtualReg, mir.NewScalarType("i64", 8)),
		Right: mir.NewRegister("v2", mir.VirtualReg, mir.NewScalarType("i64", 8)),
	})
	fn.AddBlock(blk)
	mod.AddFunction(fn)
	prog.AddModule(mod)

	out, err := sel.SelectProgram(prog)
	if err != nil {
		t.Fatalf("SelectProgram failed: %v", err)
	}
	if out == nil || len(out.Modules) != 1 {
		t.Fatalf("unexpected output program: %#v", out)
	}
	if len(out.Modules[0].Functions[0].Blocks[0].Instrs) != 1 {
		t.Fatalf("expected one selected instruction, got %d", len(out.Modules[0].Functions[0].Blocks[0].Instrs))
	}
}

func TestParseSelectorFile(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "test.td")
	if err := os.WriteFile(path, []byte(`
		target test {
			rule nop {
				pattern ret;
				cost 1;
			}
		}
	`), 0o644); err != nil {
		t.Fatalf("write temp descriptor: %v", err)
	}
	sel, err := ParseSelectorFile(path)
	if err != nil {
		t.Fatalf("ParseSelectorFile failed: %v", err)
	}
	if sel == nil {
		t.Fatal("expected non-nil selector")
	}
}
