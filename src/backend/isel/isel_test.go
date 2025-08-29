package isel

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/isel/dsl"
	"github.com/anthonyabeo/obx/src/backend/ralloc"
)

func TestDSLParser(t *testing.T) {
	src := `
rule LoadImm {
  out   GPR:$rd;
  in    GPR:$rs1, imm:$offs;
  temps GPR:$t0;
  pattern load(add($rs1, $offs));
  asm {
    "lui $t0, %hi($offs)";
    "add $t0, $rs1, $t0";
    "ld $rd, %lo($offs)($t0)";
  }
  cost 2;
  cond imm_fits_hi_lo;
}
`
	lexer := dsl.NewLexer(src)
	parser := dsl.NewParser(lexer)

	machine := parser.Parse()

	for _, rule := range machine.Rules {
		fmt.Println(rule.String())
	}
}

func TestISelect(t *testing.T) {
	src := `
rule LDri {
	out   GPR:$rd;
	in    GPR:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	asm {
		"ld $rd, $offs($rs1)";
  	}
	cost 1;
  	cond ImmFits12($offs);
}

rule LoadImm {
  out   GPR:$rd;
  in    GPR:$rs1, imm:$offs;
  temps GPR:$t0;
  pattern load(add($rs1, $offs));
  asm {
    "lui $t0, %hi($offs)";
    "addi $t0, $t0, %lo($offs)";
    "add $t0, $rs1, $t0";
    "ld $rd, 0($t0)";
  }
  cost 2;
  cond !SImmFits12($offs);
}
`
	lexer := dsl.NewLexer(src)
	parser := dsl.NewParser(lexer)

	machine := parser.Parse()

	pattern := &dsl.Node{
		Dst: &dsl.Node{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "t0"}},
		Op:  "load",
		Args: []*dsl.Node{
			{
				Op: "add",
				Args: []*dsl.Node{
					{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "t5"}},
					{Val: &dsl.Value{Kind: dsl.KindImm, Imm: 4096}},
				},
			},
		},
	}
	sel := NewSelector(machine.Rules, ralloc.NewRegisterAllocator())
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelectUncondJump(t *testing.T) {
	src := `
rule JmpU {
    in label:$target;
    pattern jmp($target);
    asm { "jal x0, $target"; }
    cost 1;
}
`
	lexer := dsl.NewLexer(src)
	parser := dsl.NewParser(lexer)

	machine := parser.Parse()

	pattern := &dsl.Node{
		Op: "jmp",
		Args: []*dsl.Node{
			{Val: &dsl.Value{Kind: dsl.KindLabel, Label: "while.loop.end.2"}},
		},
	}

	sel := NewSelector(machine.Rules, ralloc.NewRegisterAllocator())
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelectCondBr(t *testing.T) {
	src := `
rule ICMP_gt {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern gt($rs1, rs2);
    asm { "slt $rd, $rs2, $rs1"; }
    cost 1;
}

rule Br_bool {
    in  GPR:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    asm {
        "bne $rs1, x0, $true";
        "jal x0, $false";
    }
    cost 1;
}
`
	lexer := dsl.NewLexer(src)
	parser := dsl.NewParser(lexer)

	machine := parser.Parse()

	// %cmp = icmp.gt %x, %y
	// br %cmp, Ltrue, Lfalse
	patterns := []*dsl.Node{
		{
			Dst: &dsl.Node{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "cmp"}},
			Op:  "gt",
			Args: []*dsl.Node{
				{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "x"}},
				{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "y"}},
			},
		},
		{
			Op: "br",
			Args: []*dsl.Node{
				{Val: &dsl.Value{Kind: dsl.KindGPR, Reg: "cmp"}},
				{Val: &dsl.Value{Kind: dsl.KindLabel, Label: "LTrue"}},
				{Val: &dsl.Value{Kind: dsl.KindLabel, Label: "LFalse"}},
			},
		},
	}

	var instructions []string
	for _, pattern := range patterns {
		sel := NewSelector(machine.Rules, ralloc.NewRegisterAllocator())
		instructions = append(instructions, sel.Select(pattern)...)
	}

	for _, s := range instructions {
		fmt.Println(s)
	}
}
