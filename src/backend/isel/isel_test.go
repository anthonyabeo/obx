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

	rules := parser.Parse()

	for _, rule := range rules {
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
    "add $t0, $rs1, $t0";
    "ld $rd, %lo($offs)($t0)";
  }
  cost 2;
  cond !ImmFits12($offs);
}
`
	lexer := dsl.NewLexer(src)
	parser := dsl.NewParser(lexer)

	rules := parser.Parse()

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
	sel := NewSelector(rules, ralloc.NewRegisterAllocator())
	instr := sel.Select(pattern)

	for _, s := range instr {
		fmt.Println(s)
	}

}
