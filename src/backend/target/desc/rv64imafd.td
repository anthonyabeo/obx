rule AddrOf {
    out GPR:virt:$rd;
    in  symbol:$sym;
    pattern addr($sym);
    cost 1;
    emit {
        instr { 
            opcode: "la", 
            dst: GPR:virt:$rd,
            src: [symbol:$sym], 
            def: GPR:virt:$rd 
        };
    }
}

rule Arg {
    in GPR:virt:$rs, imm:$idx;
    pattern argument($rs, $idx);
    emit {
        instr {
            opcode: "mv",
            dst: arg{imm:$idx},
            src: [GPR:virt:$rs],
            uses: [GPR:virt:$rs]
        };
    }
}

rule Arg_sym {
    in symbol:$sym, imm:$idx;
    pattern argument($sym, $idx);
    temps GPR:virt:$t0;
    emit {
        instr { opcode: "mv", dst: arg{imm:$idx}, src: [symbol:$sym] };
    }
}

rule Call_direct {
    out GPR:virt:$rd;
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "jal", dst: GPR:phys:ra, src: [label:$target] };
        instr { opcode: "mv", dst: GPR:virt:$rd, src: [GPR:phys:a0], def: GPR:virt:$rd };
    }
}

rule Call_direct {
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "jal", dst: GPR:phys:ra, src: [label:$target] };
    }
}

rule Mov_r_str_const {
    in str:$name, GPR:virt:$rs;
    pattern mov($rs, $name);
    cost 1;
    emit {
        instr { opcode: "la", dst: GPR:virt:$rs, src: [str:$name], def: GPR:virt:$rs };
    }
}

rule Mov_sym_r {
    commutative;
    in symbol:$sym, GPR:virt:$rs;
    pattern mov($sym, $rs);
    emit {
        instr { opcode: "mv", dst: symbol:$sym, src: [GPR:virt:$rs], uses: [GPR:virt:$rs] };
    }
}

rule MOVrr {
    in GPR:virt:$rd, GPR:virt:$rs;
    pattern mov($rd, $rs);
    cost 1;
    emit {
        instr { opcode: "mv", dst: GPR:virt:rd, src: [GPR:virt:rs], def: GPR:virt:$rd, uses: [GPR:virt:$rs] };
    }
}

rule MOVri_SmallImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	cost 1;
	emit {
		instr { opcode: "li", dst: GPR:virt:$rd, src: [imm:$val], def: GPR:virt:$rd };
	}
	cond SImmFits12($val);
}

rule MOVri_LargeImm {
	in GPR:virt:$rd, imm:$val;
	pattern mov($rd, $val);
	cost 2;
	emit {
		instr { opcode: "lui", dst: GPR:virt:$rd, src: [reloc:hi($val)], def: GPR:virt:$rd };
		instr { opcode: "addi", dst: GPR:virt:$rd, src: [GPR:virt:$rd, reloc:lo($val)], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
	}
	cond !SImmFits12($val);
}

/////////////////////////////////////////////////////////////////
// Addition Rules
/////////////////////////////////////////////////////////////////

rule ADD_sym_sym {
  commutative;
  out GPR:virt:$rd;
  in  symbol:$sym1, symbol:$sym2;
  pattern add($sym1, $sym2);
  emit {
      instr {
        opcode: "add",
        dst: GPR:virt:$rd,
        src: [symbol:$sym1, symbol:$sym2],
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
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, symbol:$sym],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1]
      };
  }
}

rule ADD_Imm_sym {
  commutative;
  out GPR:virt:$rd;
  in  imm:$val, symbol:$sym;
  temps GPR:virt:$t0;
  pattern add($val, $sym);
  emit {
      instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
      instr { opcode: "add", dst: GPR:virt:$rd, src: [GPR:virt:$t0, symbol:$sym], def: GPR:virt:$rd, uses: [GPR:virt:$t0] };
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
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, imm:$val],
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
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
}

/////////////////////////////////////////////////////////////////
// Substration Rules
/////////////////////////////////////////////////////////////////

rule SUBrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern sub($rs1, $rs2);
  cost 1;
  emit {
    instr {
        opcode: "sub",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
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
      instr { opcode: "j", src: [label:$target] }
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
        instr { opcode: "xor", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "seqz", dst: GPR:virt:$rd, src: [GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
    cost 2;
}

rule ICMP_ne {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ne($rs1, rs2);
    emit {
        instr { opcode: "xor", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "snez", dst: GPR:virt:$rd, src: [GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
    cost 2;
}

rule ICMP_gt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern gt($rs1, $rs2);
    emit {
        instr {
            opcode: "slt",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs2, GPR:virt:$rs1],
            def:GPR:virt:$rd,
            uses: [GPR:virt:$rs2, GPR:virt:$rs1]
        };
    }
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
        instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "slt", dst: GPR:virt:$rd, src: [GPR:virt:$t0, GPR:virt:$rs1], def: GPR:virt:$rd, uses: [GPR:virt:$t0, GPR:virt:$rs1] };
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
        instr { opcode: "slt", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, GPR:virt:$rs2], def: GPR:virt:$rd, uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "seqz", dst: GPR:virt:$rd, src: [GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
}

rule ICMP_lt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lt($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "slt",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

rule ICMP_le {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern le($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "slt", dst: GPR:virt:$rd, src: [GPR:virt:$rs2, GPR:virt:$rs1], def: GPR:virt:$rd, uses: [GPR:virt:$rs2, GPR:virt:$rs1] };
        instr { opcode: "seqz", dst: GPR:virt:$rd, src: [GPR:virt:$rd], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
}

rule Br_bool {
    in  GPR:virt:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    cost 1;
    emit {
        instr { opcode: "bne", src: [GPR:virt:$rs1, GPR:phys:x0, label:$true], uses: [GPR:virt:$rs1, GPR:phys:x0] };
        instr { opcode: "j", src: [label:$false] };
    }
}

/////////////////////////////////////////////////////////////////
// Return Instructions Rules
/////////////////////////////////////////////////////////////////

rule Ret_void {
    pattern ret;
    cost 1;
    emit { instr { opcode: "j" src: [label:$target="exit"] }; }
}

rule Ret_val {
    in  GPR:virt:$rs;
    pattern ret($rs);
    cost 2;
    emit {
        instr { opcode: "mv", dst: GPR:phys:a0, src: [GPR:virt:$rs], uses: [GPR:virt:$rs] };
    }
}

rule Ret {
    in symbol:$sym;
    pattern ret($sym);
    cost 2;
    emit {
        instr { opcode: "mv", dst: GPR:phys:a0, src: [symbol:$sym] };
    }
}

rule OR {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern or($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "or",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}


rule ORI {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    pattern or($rs, $val);
    cost 1;
    emit {
        instr {
            opcode: "ori",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
    cond SImmFits12($val);
}

rule ORI_LargeImm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    temps GPR:virt:$t0;
    pattern or($rs, $val);
    cost 2;
    emit {
        instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "or", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
    cond !SImmFits12($val);
}

rule AND {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern and($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "and",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

rule ANDI {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    pattern and($rs, $val);
    cost 1;
    emit {
        instr {
            opcode: "andi",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
    cond SImmFits12($val);
}

rule AND_LargeImm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    temps GPR:virt:$t0;
    pattern and($rs, $val);
    cost 2;
    emit {
        instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "and", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
    cond !SImmFits12($val);
}

rule Not {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern not($rs);
    cost 1;
    emit {
        instr { opcode: "seqz", dst: GPR:virt:$rd, src: [GPR:virt:$rs], def: GPR:virt:$rd, uses: [GPR:virt:$rs] };
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
            dst: GPR:virt:$rd,
            src: [GPR:phys:x0, imm:$val],
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
            dst: GPR:virt:$rd,
            src: [GPR:phys:x0, GPR:virt:$rs],
            def: GPR:virt:$rd,
            uses: [GPR:phys:x0, GPR:virt:$rs]
        };
    }
}

rule SLLrr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern lshl($rs1, $rs2);
  emit {
    instr {
        opcode: "sll",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
  cost 1;
}

rule SRLrr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern lshr($rs1, $rs2);
  emit {
    instr {
        opcode: "srl",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
  cost 1;
}

rule SRArr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern ashr($rs1, $rs2);
  emit {
    instr {
        opcode: "sra",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
  cost 1;
}

rule SLLI {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern lshl($rs1, $sh);
  emit {
    instr {
        opcode: "slli",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, imm:$sh],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1]
    };
  }
  cost 1;
  cond ShamtFits6($sh);
}

rule SRLI {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, imm:$sh;
  pattern lshr($rs1, $sh);
  emit {
    instr { opcode: "srli", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, imm:$sh], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
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
            dst: GPR:virt:$rd,
            src: [symbol:$sym, GPR:virt:$rs],
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
		instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "mul", dst: GPR:virt:$rd, src: [symbol:$sym, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$t0]};
    }
}

rule MULrr {
  commutative;
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern mul($rs1, $rs2);
  cost 2;
  emit {
    instr {
        opcode: "mul",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
}

rule Mulri {
  commutative;
  out   GPR:virt:$rd;
  in    GPR:virt:$rs, imm:$val;
  temps GPR:virt:$t0;
  pattern mul($rs, $val);
  cost  1;
  emit  {
    instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
    instr { opcode: "mul", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def:GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0]  };
  }
}

rule DIVrr {
  out GPR:virt:$rd;
  in  GPR:virt:$rs1, GPR:virt:$rs2;
  pattern div($rs1, $rs2);
  emit {
    instr {
        opcode: "div",
        dst: GPR:virt:$rd,
        src: [GPR:virt:$rs1, GPR:virt:$rs2],
        def: GPR:virt:$rd,
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
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
      instr {opcode: "load", dst: GPR:virt:$rd, src: [symbol:$sym], def: GPR:virt:$rd };
  }
}

rule LDri {
	out   GPR:virt:$rd;
	in    GPR:virt:$rs1, imm:$offs;
	pattern load(add($rs1, $offs));
	cost 1;
	emit {
		instr {
		    opcode: "load",
		    dst: GPR:virt:$rd,
		    src: [mem:{base=GPR:virt:$rs1, offset=imm:$offs}],
		    def: GPR:virt:$rd,
		    uses: [GPR:virt:$rs1]
		};
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
	instr { opcode: "lui",  dst: GPR:virt:$t0, src: [reloc:hi($offs)], def: GPR:virt:$t0 };
    instr { opcode: "addi", dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($offs)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
    instr { opcode: "add",  dst: GPR:virt:$t1, src: [GPR:virt:$rs1, GPR:virt:$t0], def: GPR:virt:$t1, uses: [GPR:virt:$rs1, GPR:virt:$t0] };
    instr { opcode: "load", dst: GPR:virt:$rd, src: [mem:{base=GPR:virt:$t1, offset=imm:0}], def: GPR:virt:$rd, uses: [GPR:virt:$t1] };
  }
  cond !SImmFits12($offs);
}


/////////////////////////////////////////////////////////////////
// Store Instructions Rules
/////////////////////////////////////////////////////////////////

rule STri {
  in GPR:virt:$rs1, GPR:virt:$rs2, imm:$offs;
  pattern store($rs1, add($rs2, $offs));
  cost 1;
  emit {
    instr {
        opcode: "store",
        dst: mem:{base=GPR:virt:$rs2, offset=imm:$offs},
        src: [GPR:virt:$rs1],
        uses: [GPR:virt:$rs1, GPR:virt:$rs2]
    };
  }
  cond SImmFits12($offs);
}

rule STImmGlobalMem {
    in imm:$c, symbol:$sym;
    temps GPR:virt:$t0;
    pattern store($c, $sym);
    cost 3;
    emit {
        instr { opcode: "li", dst: GPR:virt:$t0, src: [imm:$c], def: GPR:virt:$t0 };
        instr { opcode: "store", dst: symbol:$sym, src: [GPR:virt:$t0], uses: [GPR:virt:$t0] };
    }
}

rule STRegGlobalMem {
    in GPR:virt:$rs1, symbol:$sym;
    pattern store($rs1, $sym);
    cost 3;
    emit {
        instr { opcode: "store", dst: symbol:$sym, src: [GPR:virt:$rs1], uses: [GPR:virt:$rs1] };
    }
}