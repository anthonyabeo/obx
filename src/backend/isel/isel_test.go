package isel

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/isel/bud"
	"github.com/anthonyabeo/obx/src/backend/isel/bud/parser"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

func TestISelect(t *testing.T) {
	src := `
rule LDri {
	out   GPR:virt:$rd;
	in    GPR:virt:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	emit {
		instr { opcode: "ld", operands: GPR:virt:$rd, imm:$offs, GPR:virt:$rs1 };
  	}
	cost 1;
  	cond ImmFits12($offs);
}

rule LoadImm {
  out   GPR:virt:$rd;
  in    GPR:virt:$rs1, imm:$offs;
  temps GPR:virt:$t0;
  pattern load(add($rs1, $offs));
  emit {
	instr { opcode: "lui", operands: GPR:virt:$t0, reloc:hi($offs) };
	instr { opcode: "addi", operands: GPR:virt:$t0, GPR:virt:$t0, reloc:lo($offs) };
	instr { opcode: "add", operands: GPR:virt:$t0, GPR:virt:$rs1, GPR:virt:$t0}
	instr { opcode: "ld", operands: GPR:virt:$rd, mem:{base=GPR:virt:$t0, offset=imm:0} }
  }
  cost 2;
  cond !SImmFits12($offs);
}
`
	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Dst: &bud.Node{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "res.t"}}},
		Op:  "load",
		Args: []*bud.Node{
			{
				Op: "add",
				Args: []*bud.Node{
					{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
					{Val: &bud.Value{Kind: bud.KindImm, Imm: 4096}},
				},
			},
		},
	}
	sel := NewSelector(machine.Rules)
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
    emit {
		instr { opcode: "jal", operands: GPR:phys:$x0, label:$target }
	}
    cost 1;
}
`
	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Op: "jmp",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindLabel, Label: "while.loop.end.2"}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelectCondBr(t *testing.T) {
	src := `
rule ICMP_gt {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern gt($rs1, rs2);
    emit { 
		instr { opcode: "slt", operands: GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1 };
	}
    cost 1;
}

rule Br_bool {
    in  GPR:virt:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    emit {
		instr { opcode: "bne", operands: GPR:virt:$rs1, GPR:phys:x0, label:$true };
		instr { opcode: "jal", operands: GPR:phys:x0, label:$false };
    }
    cost 2;
}
`
	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()
	// %cmp = icmp.gt %x, %y
	// br %cmp, Ltrue, Lfalse
	patterns := []*bud.Node{
		{
			Dst: &bud.Node{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "cmp"}}},
			Op:  "gt",
			Args: []*bud.Node{
				{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "x"}}},
				{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "y"}}},
			},
		},
		{
			Op: "br",
			Args: []*bud.Node{
				{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "cmp"}}},
				{Val: &bud.Value{Kind: bud.KindLabel, Label: "LTrue"}},
				{Val: &bud.Value{Kind: bud.KindLabel, Label: "LFalse"}},
			},
		},
	}

	var instructions []*asm.Instr
	for _, pattern := range patterns {
		sel := NewSelector(machine.Rules)
		instructions = append(instructions, sel.Select(pattern)...)
	}

	for _, s := range instructions {
		fmt.Println(s)
	}
}
