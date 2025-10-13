rule Arg {
    in GPR:virt:$rs, imm:$idx;
    pattern argument($rs, $idx);
    emit {
        instr {
            opcode: "mv",
            operands: [arg{imm:$idx}, GPR:virt:$rs],
            uses: [GPR:virt:$rs]
        };
    }
}

rule Call_direct {
    out GPR:virt:$rd;
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "jal", operands: [GPR:phys:ra, label:$target] };
        instr { opcode: "mv", operands: [GPR:virt:$rd, GPR:phys:a0], def: GPR:virt:$rd };
    }
}

rule Call_direct {
    out GPR:virt:$rd;
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "jal", operands: [GPR:phys:ra, label:$target] };
    }
}

rule Mov_sym_r {
    in symbol:$sym, GPR:virt:$rs;
    pattern mov($sym, $rs);
    emit {
        instr { opcode: "mv", operands: [symbol:$sym, GPR:virt:$rs], uses: [GPR:virt:$rs] };
    }
}

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

/////////////////////////////////////////////////////////////////
// Addition Jump Rules
/////////////////////////////////////////////////////////////////

rule ADD_sym_sym {
  commutative;
  out GPR:virt:$rd;
  in  symbol:$sym1, symbol:$sym2;
  pattern add($sym1, $sym2);
  emit {
      instr {
        opcode: "add",
        operands: [GPR:virt:$rd, symbol:$sym1, symbol:$sym2],
        def: GPR:virt:$rd
      };
  }
}

rule ADDr_sym {
  commutative;
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

rule ADDri {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$val;
  pattern add($rs1, $val);
  cost 1;
  emit {
    instr {
        opcode: "addi",
        operands: [GPR:virt:$rd, GPR:virt:$rs1, imm:$val],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1]
    };
  }
  cond SImmFits12($val);
}

rule ADDrr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern add($rs1, $rs2);
  cost 1;
  emit {
    instr {
        opcode: "add",
        operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
}


/////////////////////////////////////////////////////////////////
// Unconditional Jump Rules
/////////////////////////////////////////////////////////////////

rule JmpU {
    in label:$target;
    pattern jmp($target);
    cost 1;
    emit {
      instr { opcode: "j", operands: [label:$target] }
    }
}

/////////////////////////////////////////////////////////////////
// Comparison Rules
/////////////////////////////////////////////////////////////////

rule ICMP_eq {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern eq($rs1, rs2);
    emit {
        instr { opcode: "xor", operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "seqz", operands: [GPR:virt:$rd, GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
    cost 2;
}

rule ICMP_ne {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ne($rs1, rs2);
    emit {
        instr { opcode: "xor", operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "snez", operands: [GPR:virt:$rd, GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
    cost 2;
}

rule ICMP_gt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern gt($rs1, $rs2);
    emit { instr { opcode: "slt", operands: [GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1], def:GPR:virt:$rd, uses: [GPR:virt:$rs2, GPR:virt:$rs1]  }; }
    cost 1;
}

rule ICMP_gt_imm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$val;
    temps GPR:virt:$t0;
    pattern gt($rs1, $val);
    cost 2;
    emit {
        instr { opcode: "li", operands: [GPR:virt:$t0, imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "slt", operands: [GPR:virt:$rd, GPR:virt:$t0, GPR:virt:$rs1], def: GPR:virt:$rd, uses: [GPR:virt:$t0, GPR:virt:$rs1] };
    }
    cond SImmFits12($val);
}

rule ICmp_ge {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ge($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "slt", operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "seqz", operands: [GPR:virt:$rd, GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
}

rule ICMP_lt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lt($rs1, rs2);
    cost 1;
    emit { instr { opcode: "slt",  operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2]  }; }
}

rule ICMP_le {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern le($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "slt", operands: [GPR:virt:$rd, GPR:virt:$rs2, GPR:virt:$rs1], def: GPR:virt:$rd, uses: [GPR:virt:$rs2, GPR:virt:$rs1] };
        instr { opcode: "seqz", operands: [GPR:virt:$rd, GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }

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

/////////////////////////////////////////////////////////////////
// Return Instructions Rules
/////////////////////////////////////////////////////////////////

rule Ret_void {
    pattern ret;
    cost 1;
    emit { instr { opcode: "j" operands: [label:$target="exit"] }; }
}

rule Ret_val {
    in  GPR:virt:$rs;
    pattern ret($rs);
    cost 2;
    emit {
        instr { opcode: "mv", operands: [GPR:phys:a0, GPR:virt:$rs], def: GPR:phys:a0, uses: [GPR:virt:$rs] };
        instr { opcode: "j", operands: [label:$target="exit"], uses: [GPR:phys:a0] };
    }
}

rule Ret {
    in symbol:$sym;
    pattern ret($sym);
    cost 2;
    emit {
        instr { opcode: "mv", operands: [GPR:phys:a0, symbol:$sym], def: GPR:phys:a0 };
        instr { opcode: "j", operands: [label:$target="exit"] };
    }
}


rule Not {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern not($rs);
    cost 1;
    emit {
        instr {
            opcode: "xori",
            operands: [GPR:virt:$rd, GPR:virt:$rs, imm:-1],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
}

rule NegImm {
    out GPR:virt:$rd;
    in  imm:$val;
    pattern neg($val);
    cost 1;
    emit {
        instr {
            opcode: "sub",
            operands: [GPR:virt:$rd, GPR:phys:x0, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:phys:x0]
        };
    }
}

rule NegReg {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern neg($rs);
    cost 1;
    emit {
        instr {
            opcode: "sub",
            operands: [GPR:virt:$rd, GPR:phys:x0, GPR:virt:$rs],
            def: GPR:virt:$rd,
            uses: [GPR:phys:x0, GPR:virt:$rs]
        };
    }
}


rule SLLI {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern shl($rs1, $sh);
  emit {
    instr { opcode: "slli", operands: [GPR:virt:$rd, GPR:virt:$rs1, imm:$sh], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
  }
  cost 1;
  cond ShamtFits6($sh);
}

rule SRLI {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern lshr($rs1, $sh);
  emit {
    instr { opcode: "srli", operands: [GPR:virt:$rd, GPR:virt:$rs1, imm:$sh], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
  }
  cost 1;
  cond ShamtFits6($sh);
}

rule MUL_Symbol_r {
    commutative;
    out GPR:virt:$rd;
    in symbol:$sym, GPR:virt:$rs;
    pattern mul($sym, $rs);
    cost 2;
    emit {
        instr {
            opcode: "mul",
            operands: [GPR:virt:$rd, symbol:$sym, GPR:virt:$rs],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
}

rule MUL_Symbol_Imm {
    commutative;
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

rule MULrr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern mul($rs1, $rs2);
  cost 2;
  emit {
    instr { opcode: "mul", operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
  }
}

rule Mulri {
  commutative;
  out   GPR:virt:$rd;
  in    GPR:virt:$rs, imm:$val;
  pattern mul($rs, $val);
  cost  1;
  emit  {
    instr { opcode: "li", operands: [GPR:virt:$t0, imm:$val], def: GPR:virt:$rd };
    instr { opcode: "muli", operands: [GPR:virt:$rd, GPR:virt:$rs, imm:$val], def:GPR:virt:$rd, uses: [GPR:virt:$rs]  };
  }
}

rule DIVrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern div($rs1, $rs2);
  emit {
    instr { opcode: "div", operands: [GPR:virt:$rd, GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
  }
  cost 10;
}

/////////////////////////////////////////////////////////////////
// Load Instructions Rules
/////////////////////////////////////////////////////////////////

rule LoadGlobal {
  out   GPR:virt:$rd;
  in    symbol:$sym;
  cost  2;
  pattern load($sym);
  emit {
      instr {   opcode: "ld", operands: [GPR:virt:$rd, symbol:$sym], def: GPR:virt:$rd };
  }
}

rule LDri {
	out   GPR:virt:$rd;
	in    GPR:virt:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	cost 1;
	emit {
		instr { opcode: "ld", operands: [GPR:virt:$rd, mem:{base=GPR:virt:$rs1, offset=imm:$offs}], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
  	}
  	cond SImmFits12($offs);
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


/////////////////////////////////////////////////////////////////
// Store Instructions Rules
/////////////////////////////////////////////////////////////////

rule STri {
  in GPR:virt:$rs1, imm:$offs, GPR:virt:$rs2;
  pattern store($rs1, $offs, $rs2);
  cost 1;
  emit {
    instr { opcode: "sd", operands: [GPR:virt:$rs2, mem:{base=GPR:virt:$rs1, offset=imm:$offs}], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
  }
  cond SImmFits12($offs);
}

rule STImmGlobalMem {
    in imm:$c, symbol:$sym;
    temps GPR:virt:$t0;
    pattern store($c, $sym);
    cost 3;
    emit {
        instr { opcode: "li", operands: [GPR:virt:$t0, imm:$c], def: GPR:virt:$t0 };
        instr { opcode: "sd", operands: [GPR:virt:$t0, symbol:$sym], uses: [GPR:virt:$t0] };
    }
}

rule STRegGlobalMem {
    in GPR:virt:$rs1, symbol:$sym;
    pattern store($rs1, $sym);
    cost 3;
    emit {
        instr { opcode: "sd", operands: [GPR:virt:$rs1, symbol:$sym], uses: [GPR:virt:$rs1] };
    }
}