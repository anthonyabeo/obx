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
		instr { 
			opcode: "ld", 
			operands: [GPR:virt:$rd, mem:{base=GPR:virt:$rs1, offset=imm:$offs}]
		}
  	}
	cost 1;
  	cond ImmFits12($offs);
}

rule LD_Large {
  out   GPR:virt:$rd;
  in    GPR:virt:$rs1, imm:$offs;
  temps GPR:virt:$t0, GPR:virt:$t1;
  cost 4;
  pattern load(add($rs1, $offs));
  emit {
	instr { opcode: "lui",  operands: [GPR:virt:$t0, reloc:hi($offs)], def: GPR:virt:$t0 };
    instr { opcode: "addi", operands: [GPR:virt:$t0, GPR:virt:$t0, reloc:lo($offs)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
    instr { opcode: "add",  operands: [GPR:virt:$t1, GPR:virt:$rs1, GPR:virt:$t0], def: GPR:virt:$t1, uses: [GPR:virt:$rs1, GPR:virt:$t0] };
    instr { opcode: "ld",   operands: [GPR:virt:$rd, mem:{base=GPR:virt:$t1, offset=imm:0}], def: GPR:virt:$rd, uses: [GPR:virt:$t1] };
  }
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
    cost 1;
    emit {
      instr { opcode: "j", operands: [label:$target] }
    }
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
    pattern gt($rs1, $rs2);
    emit { instr { opcode: "slt", operands: [GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1], def:GPR:virt:$rd, uses: [GPR:virt:$rs2, GPR:virt:$rs1]  }; }
    cost 1;
}

rule Br_bool {
    in  GPR:virt:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    cost 1;
    emit {
        instr { opcode: "bne", operands: [GPR:virt:$rs1, GPR:phys:x0, label:$true], uses: [GPR:virt:$rs1, GPR:phys:x0] };
        instr { opcode: "j", operands: [label:$false] };
    }
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

func TestISelMovInstr(t *testing.T) {
	src := `
rule MOVrr {
    in GPR:virt:$rd, GPR:virt:$rs;
    pattern mov($rd, $rs);
    cost 1;
    emit {
        instr { opcode: "mv", operands: [GPR:virt:rd, GPR:virt:rs], def: GPR:virt:$rd, uses: [GPR:virt:$rs] };
    }
}

rule MOVri_SmallImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	cost 1;
	emit {
		instr { opcode: "li", operands: [GPR:virt:$rd, imm:$val], def: GPR:virt:$rd };
	}
	cond SImmFits12($val);
}

rule MOVri_LargeImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	cost 2;
	emit {
		instr { opcode: "lui", operands: [GPR:virt:$rd, reloc:hi($val)], def: GPR:virt:$rd };
		instr { opcode: "addi", operands: [GPR:virt:$rd, GPR:virt:$rd, reloc:lo($val)], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
	}
	cond !SImmFits12($val);
}
`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	//pattern := &bud.Node{
	//	Op: "mov",
	//	Args: []*bud.Node{
	//		{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
	//		{Val: &bud.Value{Kind: bud.KindImm, Imm: 9}},
	//	},
	//}

	pattern := &bud.Node{
		Op: "mov",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
			{Val: &bud.Value{Kind: bud.KindImm, Imm: 5000}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelLoadGlobal(t *testing.T) {
	src := `
////////////////////////////////////////
// Load a global variable from memory //
////////////////////////////////////////
rule LoadGlobal {
  out   GPR:virt:$rd;
  in    symbol:$sym;
  temps GPR:virt:$tmp0;
  cost  2;
  pattern load($sym);
  emit {
    instr { opcode: "lui",  operands: [GPR:virt:$tmp0, reloc:hi($sym)] };
    instr { opcode: "addi", operands: [GPR:virt:$tmp0, GPR:virt:$tmp0, reloc:lo($sym)] };
    instr { opcode: "ld",   operands: [GPR:virt:$rd,  mem:{base=GPR:virt:$tmp0, offset=imm:0}] };
  }
}
`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Dst: &bud.Node{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "ld.t"}}},
		Op:  "load",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindSymbol, Symbol: bud.Symbol{Name: "mat"}}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelAssignLocal(t *testing.T) {
	src := `
rule Mov_sym_r {
    in symbol:$sym, GPR:virt:$rs;
    pattern mov($sym, $rs);
    emit {
        instr { opcode: "mv", operands: [symbol:$sym, GPR:virt:$rs], uses: [GPR:virt:$rs] };
    }
}
`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Op: "mov",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindSymbol, Symbol: bud.Symbol{Name: "sum", Kind: bud.LocalSK}}},
			{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelMulLocalImm(t *testing.T) {
	src := `
rule MUL_Symbol_Imm {
    out GPR:virt:$rd;
    in symbol:$sym, imm:$val;
	temps GPR:virt:$t0;
    pattern mul($sym, $val);
    cost 2;
    emit {
		instr { opcode: "li", operands: [GPR:virt:$t0, imm:$val], def: GPR:virt:$rd };
        instr { opcode: "mul", operands: [GPR:virt:$rd, symbol:$sym, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$t0]};
    }
}
`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Dst: &bud.Node{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t10"}}},
		Op:  "mul",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindSymbol, Symbol: bud.Symbol{Name: "sum"}}},
			{Val: &bud.Value{Kind: bud.KindImm, Imm: 2}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelAddRegSymbol(t *testing.T) {
	src := `
rule ADDr_sym {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, symbol:$sym;
  pattern add($rs1, $sym);
  emit {
      instr {
        opcode: "add",
        operands: [GPR:virt:$rd, GPR:virt:$rs1, symbol:$sym],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1]
      };
  }
}
`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Dst: &bud.Node{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t9"}}},
		Op:  "add",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
			{Val: &bud.Value{Kind: bud.KindSymbol, Symbol: bud.Symbol{Name: "sum", Kind: bud.LocalSK}}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelArg(t *testing.T) {
	src := `
rule Arg {
    in GPR:virt:$rs, imm:$idx;
    pattern argument($rs, $idx);
    emit {
        instr { opcode: "mv", operands: [arg{imm:$idx}, GPR:virt:$rs] };
    }
}

`
	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)

	machine := p.Parse()

	pattern := &bud.Node{
		Op: "argument",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
			{Val: &bud.Value{Kind: bud.KindImm, Imm: 2}},
		},
	}

	sel := NewSelector(machine.Rules)
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}
}

func TestISelStoreMem(t *testing.T) {
	src := `rule STri {
  in GPR:virt:$rs1, GPR:virt:$rs2, imm:$offs;
  pattern store($rs1, add($rs2, $offs));
  cost 1;
  emit {
    instr { opcode: "sd", operands: [GPR:virt:$rs1, mem:{base=GPR:virt:$rs2, offset=imm:$offs}], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
  }
  cond SImmFits12($offs);
}`

	lexer := parser.NewLexer(src)
	p := parser.NewParser(lexer)
	machine := p.Parse()
	pattern := &bud.Node{
		Op: "store",
		Args: []*bud.Node{
			{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t5"}}},
			{
				Op: "add",
				Args: []*bud.Node{
					{Val: &bud.Value{Kind: bud.KindGPR, Reg: bud.Reg{Name: "t6"}}},
					{Val: &bud.Value{Kind: bud.KindImm, Imm: 8}},
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
