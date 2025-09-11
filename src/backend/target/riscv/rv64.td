rule MOVrr {
	in GPR:virt:$rd, GPR:virt:$rs;
	pattern mov($rd, $rs);
	emit {
		instr { opcode: "addi", operands: GPR:virt:rd, GPR:virt:rs, imm:0 };
	}
	cost 1;
}

rule MOVri_SmallImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	emit {
		instr { opcode: "addi", operands: GPR:virt:$rd, GPR:phys:x0, imm:$val };
	}
	cost 1;
	cond SImmFits12($val);
}

rule MOVri_LargeImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	emit {
		instr { opcode: "lui", operands: GPR:virt:$rd, reloc:hi($val) };
		instr { opcode: "addi", operands: GPR:virt:$rd, GPR:virt:$rd, reloc:lo($val) };
	}
	cost 1;
	cond !SImmFits12($val);
}

rule ADDri {
  out GPR:$rd;
  in  GPR:$rs1, imm:$imm;
  pattern add($rs1, $imm);
  asm { "addi $rd, $rs1, $imm" }
  cost 1;
  cond SImmFits12($imm);
}

rule ADDrr {
  out GPR:$rd;
  in  GPR:$rs1, GPR:$rs2;
  pattern add($rs1, $rs2);
  asm { "add $rd, $rs1, $rs2" }
  cost 1;
}

rule LDri {
	out   GPR:$rd;
	in    GPR:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	asm {
		"ld $rd, $offs($rs1)";
  	}
	cost 1;
  	cond SImmFits12($offs);
}

rule LD_Large {
  out   GPR:$rd;
  in    GPR:$rs1, imm:$offs;
  temps GPR:$t0;
  pattern load(add($rs1, $offs));
  asm {
    "lui $t0, %hi($offs)";
    "addi $t0, $t0, %lo($ofs)";
    "add $t0, $rs1, $t0";
    "ld $rd, 0($t0)";
  }
  cost 2;
  cond !SImmFits12($offs);
}

rule JmpU {
    in label:$target;
    pattern jmp($target);
    asm { "jal x0, $target"; }
    cost 1;
}



rule ICMP_eq {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern eq($rs1, rs2);
    asm {
        "xor $rd, $rs1, $rs2";
        "seqz $rd, $rd";
    }
    cost 2;
}

rule ICMP_ne {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern ne($rs1, rs2);
    asm {
        "xor $rd, $rs1, $rs2";
        "snez $rd, $rd";
    }
    cost 2;
}

rule ICMP_gt {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern gt($rs1, rs2);
    asm { "slt $rd, $rs2, $rs1"; }
    cost 1;
}

rule ICmp_ge {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern ge($rs1, rs2);
    asm {
        "slt $rd, $rs1, $rs2";
        "seqz $rd, $rd";
    }
    cost 2;
}


rule ICMP_lt {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern lt($rs1, rs2);
    asm { "slt $rd, $rs1, $rs2"; }
    cost 1;
}

rule ICMP_le {
    out GPR:$rd;
    in  GPR:$rs1, GPR:$rs2;
    pattern le($rs1, rs2);
    asm {
        "slt $rd, $rs2, $rs1";
        "seqz $rd, $rd";
    }
    cost 2;
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

rule Ret_void {
    pattern jmp
    asm { "jalr x0, ra, 0"; }
    cost 1;
}

rule Ret_val {
    in  GPR:$rs;
    pattern ret($rs);
    asm {
        "mv a0, $rs";
        "jalr x0, ra, 0";
    }
}

rule NegImm {
    out GPR:$rd;
    in  imm:$imm;
    pattern neg($imm);
    asm { "li $rd, -$imm"; }
    cost 1;
}

rule NegReg {
    out GPR:$rd;
    in  GPR:$rs;
    pattern neg($rs)
    asm { "sub $rd, x0, $rs"; }
    cost 1;
}


// store (sd) with small offset
rule STri {
  in    GPR:$rs1, imm:$ofs, GPR:$rs2;  // store rs2 -> rs1+ofs
  pattern store($rs1, $ofs, $rs2);
  asm { "sd $rs2, $ofs($rs1)" }
  cost 1;
  cond SImmFits12($ofs);
}

rule LI_lui_addi {
  out GPR:$rd;
  in  imm:$val;
  temps GPR:$tmp;
  pattern const($val);
  asm {
    "lui $tmp, %hi($val)";
    "addi $rd, $tmp, %lo($val)";
  }
  cost 3;
  cond UImmFits20($val);
}

rule SLLI {
  out GPR:$rd;
  in  GPR:$rs1, imm:$sh;
  pattern shl($rs1, $sh);
  asm { "slli $rd, $rs1, $sh" }
  cost 1;
  cond ShamtFits6($sh);
}

rule SRLI {
  out GPR:$rd;
  in  GPR:$rs1, imm:$sh;
  pattern lshr($rs1, $sh);
  asm { "srli $rd, $rs1, $sh" }
  cost 1;
  cond ShamtFits6($sh);
}

rule MULrr {
  out GPR:$rd;
  in  GPR:$rs1, GPR:$rs2;
  pattern mul($rs1, $rs2);
  asm { "mul $rd, $rs1, $rs2" }
  cost 2;
  cond hasM();
}

rule DIVrr {
  out GPR:$rd;
  in  GPR:$rs1, GPR:$rs2;
  pattern div($rs1, $rs2);
  asm { "div $rd, $rs1, $rs2" }
  cost 10;
  cond hasM();
}

rule BEQ {
  in  GPR:$rs1, GPR:$rs2;
  pattern br_eq($rs1, $rs2, $label);
  asm { "beq $rs1, $rs2, $label" }
  cost 1;
}

rule BLT {
  in  GPR:$rs1, GPR:$rs2;
  pattern br_lt($rs1, $rs2, $label);
  asm { "blt $rs1, $rs2, $label" }
  cost 1;
}

rule FADD_d {
  out FPR:$fd;
  in  FPR:$fs1, FPR:$fs2;
  pattern fadd.d($fs1, $fs2);
  asm { "fadd.d $fd, $fs1, $fs2" }
  cost 2;
  cond hasD();
}

rule FMUL_d {
  out FPR:$fd;
  in  FPR:$fs1, FPR:$fs2;
  pattern fmul.d($fs1, $fs2);
  asm { "fmul.d $fd, $fs1, $fs2" }
  cost 2;
  cond hasD();
}

rule FMA_d {
  out FPR:$fd;
  in  FPR:$fs1, FPR:$fs2, FPR:$fs3;
  pattern fma.d($fs1, $fs2, $fs3);
  asm { "fmadd.d $fd, $fs1, $fs2, $fs3" }
  cost 2;
  cond hasD();  // also require FMA support (if you want a separate pred)
}

