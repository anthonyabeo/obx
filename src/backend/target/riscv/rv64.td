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
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$val;
  pattern add($rs1, $imm);
  emit {
    instr { opcode: "addi", operands: GPR:virt:$rd, GPR:virt:$rs1, imm:$val };
  }
  cost 1;
  cond SImmFits12($imm);
}

rule ADDrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern add($rs1, $rs2);
  emit {
    instr { opcode: "add", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 }
  }
  cost 1;
}


rule LoadGlobal {
  out   GPR:virt:$rd;
  in    global:$sym;
  temps GPR:virt:$tmp0;
  cost  2;
  pattern load($sym);
  emit {
    instr { opcode: "LUI",  operands: GPR:virt:$tmp0, reloc:hi($sym) };
    instr { opcode: "ADDI", operands: GPR:virt:$tmp0, GPR:virt:$tmp0, reloc:lo($sym) };
    instr { opcode: "LW",   operands: GPR:virt:$rd,  mem:{base=GPR:virt:$tmp0, offset=imm:0} };
  }
}

rule LDri {
	out   GPR:virt:$rd;
	in    GPR:virt:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	emit {
		instr { opcode: "ld", operands: GPR:virt:$rd, mem:{base=GPR:virt:$rs1, offset=imm:$offs};
  	}
	cost 1;
  	cond SImmFits12($offs);
}

rule LD_Large {
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

rule JmpU {
    in label:$target;
    pattern jmp($target);
    emit {
      instr { opcode: "jal", operands: GPR:phys:$x0, label:$target }
    }
    cost 1;
}



rule ICMP_eq {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern eq($rs1, rs2);
    emit {
        instr { opcode: "xor", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 };
        instr { opcode: "seqz", operands: GPR:virt:$rd, GPR:virt:$rd };
    }
    cost 2;
}

rule ICMP_ne {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ne($rs1, rs2);
    emit {
        instr { opcode: "xor", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 };
        instr { opcode: "snez", operands: GPR:virt:$rd, GPR:virt:$rd };
    }
    cost 2;
}

rule ICMP_gt {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern gt($rs1, rs2);
    emit { instr { opcode: "slt", operands: GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1 };
    cost 1;
}

rule ICmp_ge {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ge($rs1, rs2);
    asm {
        instr { opcode: "slt", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 };
        instr { opcode: "seqz", operands: GPR:virt:$rd, GPR:virt:$rd };
    }
    cost 2;
}


rule ICMP_lt {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lt($rs1, rs2);
    emit { instr { opcode: "slt",  GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 };
    cost 1;
}

rule ICMP_le {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern le($rs1, rs2);
    emit {
        instr { opcode: "slt", operands: GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1 };
        instr { opcode: "seqz", operands: GPR:virt:$rd, GPR:virt:$rd };
    }
    cost 2;
}

rule Br_bool {
    in  GPR:virt:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    cost 1;
    emit {
        instr { opcode: "bne", operands: GPR:virt:$rs1, GPR:phys:x0, label:$true };
        instr { opcode: "jal" operands: GPR:phys:x0, label:$false";
    }
}

rule Ret_void {
    pattern jmp
    cost 1;
    emit { opcode: "jalr", operands: GPR:phys:x0, GPR:phys:ra, imm:0 };
}

rule Ret_val {
    in  GPR:virt:$rs;
    pattern ret($rs);
    cost 1;
    emit {
        instr { opcode: "mv", operands: GPR:phys:a0, GPR:virt:$rs };
        instr { opcode: "jalr", operands:  GPR:phys:x0, GPR:phys:ra, imm:0 };
    }
}

rule NegImm {
    out GPR:virt:$rd;
    in  imm:$val;
    pattern neg($val);
    cost 1;
    emit { instr { opcode: "li", operands: GPR:virt:$rd, imm:-$val }; }
}

rule NegReg {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern neg($rs)
    cost 1;
    emit { instr { opcode: "sub" GPR:virt:$rd, GPR:phys:x0, GPR:virt:$rs }; }
}


rule STri {
  in    GPR:virt:$rs1, imm:$offs, GPR:virt:$rs2;
  pattern store($rs1, $offs, $rs2);
  cost 1;
  emit { instr { opcode: "sd", operands: GPR:virt:$rs2, mem:{base=GPR:virt:$rs1, offset=imm:$offs" } }};
  cond SImmFits12($offs);
}

rule SLLI {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern shl($rs1, $sh);
  emit { instr { opcode: "slli", operands: GPR:virt:$rd, $GPR:virt:rs1, imm:$sh } };
  cost 1;
  cond ShamtFits6($sh);
}

rule SRLI {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern lshr($rs1, $sh);
  emit { instr { opcode: "srli", operands: GPR:virt:$rd, GPR:virt:$rs1$rs1, imm:$sh }; }
  cost 1;
  cond ShamtFits6($sh);
}

rule MULrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern mul($rs1, $rs2);
  emit { instr { opcode: "mul", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2" }; }
  cost 2;
}

rule Mulri {
  out   GPR:virt:$rd;
  in    GPR:virt:$rs, imm:$val;
  pattern mul($rs, $val);
  cost  1
  emit  {  instr { opcode: "muli", operands: GPR:virt:$rd, GPR:virt:$rs, imm:$val }; }
}


rule DIVrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern div($rs1, $rs2);
  emit { instr { opcode: "div", operands: GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2 }; }
  cost 10;
}
