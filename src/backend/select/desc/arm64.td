header {
  ABI: "AAPCS64";
}

target arm64 {

// ---------------------------------------------------------------------------
// Arithmetic lowering: the first and most important tier of rules.
//
// These rules are intentionally organized from the simplest register/register
// forms through immediate, shifted, and comparison patterns.  The selector
// compiler will pick the cheapest matching rule for each MIR node.
// ---------------------------------------------------------------------------

block arithmetic {

  // ----- Addition -----
  // add(rd, lhs, rhs)  ->  add rd, lhs, rhs
  //
  // This is the canonical register/register form.  It is the fallback for
  // add nodes when no immediate or folded form applies.
  rule ADDrr {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern add($lhs, $rhs);
	}
	cost 1;
	emit {
	  instr { opcode: "add"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule ADDri_large {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  temp $tmp : GPR:virt;
	  pattern add($lhs, $imm);
	}
	cost 2;
	cond !FitsAddSubImm($imm);
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "add"; dst: $rd; src: [$lhs, $tmp]; def: $rd; uses: [$lhs, $tmp]; };
	}
  }

  // add(rd, lhs, imm)  ->  add rd, lhs, #imm
  //
  // ARM64 encodes the add-immediate form with a compact immediate field.
  // We keep the constraint conservative here: unsigned 12-bit immediates only
  // (optionally shifted left by 12). If a larger constant is needed, a later
  // materialization rule should be added above this one.
  rule ADDri {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  pattern add($lhs, $imm);
	}
	cost 1;
	cond FitsAddSubImm($imm);
	emit {
	  instr { opcode: "add"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
	}
  }

  // add(rd, lhs, lshl(rhs, sh))  ->  add rd, lhs, rhs, LSL #sh
  //
  // This covers the shifted-register addressing/arithmetic form that ARM64
  // uses extensively for strength-reduced index math.
  rule ADD_shifted_lsl {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  in $sh : imm;
	  pattern add($lhs, lshl($rhs, $sh));
	}
	cost 2;
	cond ShamtFits6($sh);
	emit {
	  instr {
	    opcode: "add";
	    dst: $rd;
	    src: [$lhs, $rhs, lsl ,$sh];
	    def: $rd;
	    uses: [$lhs, $rhs];
	  };
	}
  }

  rule ADD_shifted_lsr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern add($lhs, lshr($rhs, $sh));
  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
  	  instr {
  	    opcode: "add";
  	    dst: $rd;
  	    src: [$lhs, $rhs, lsr, $sh];
  	    def: $rd;
  	    uses: [$lhs, $rhs];
  	  };
  	}
  }

  rule ADD_shifted_asr {
    match {
      commutative;
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      in $sh : imm;
      pattern add($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
      instr {
        opcode: "add";
        dst: $rd;
        src: [$lhs, $rhs, asr, $sh];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  // ----- Subtraction -----
  // sub(rd, lhs, rhs)  ->  sub rd, lhs, rhs
  rule SUBrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern sub($lhs, $rhs);
	}
	cost 1;
	emit {
	  instr { opcode: "sub"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  // sub(rd, lhs, imm)  ->  sub rd, lhs, #imm
  rule SUBri {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  pattern sub($lhs, $imm);
	}
	cost 1;
	cond FitsAddSubImm($imm);
	emit {
	  instr { opcode: "sub"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
	}
  }

  rule SUBri_large {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  temp $tmp : GPR:virt;
	  pattern sub($lhs, $imm);
	}
	cost 5;
	cond !FitsAddSubImm($imm);
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "sub"; dst: $rd; src: [$lhs, $tmp]; def: $rd; uses: [$lhs, $tmp]; };
	}
  }

  rule SUB_shifted_lsl {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern sub($lhs, lshl($rhs, $sh));
  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
  	  instr {
  	    opcode: "sub";
  	    dst: $rd;
  	    src: [$lhs, $rhs, lsl ,$sh];
  	    def: $rd;
  	    uses: [$lhs, $rhs];
  	  };
  	}
  }

  rule SUB_shifted_lsr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern sub($lhs, lshr($rhs, $sh));

  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
  	  instr {
  	    opcode: "sub";
  	    dst: $rd;
  	     src: [$lhs, $rhs, lsr, $sh];
  	     def: $rd;
  	     uses: [$lhs, $rhs];

  	  };
  	}
  }

  rule SUB_shifted_asr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern sub($lhs, ashr($rhs, $sh));
  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
      instr {
        opcode: "sub";
        dst: $rd;
        src: [$lhs, $rhs, asr, $sh];
        def: $rd;
        uses: [$lhs, $rhs];
      };
  	}
  }

  // ----- AND -----
  rule ANDrr {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern and($lhs, $rhs);
	}
	cost 1;
	emit {
	  instr { opcode: "and"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule ANDri {
    match {
      commutative;
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $imm : imm;
      pattern and($lhs, $imm);
    }
    cost 1;
    cond FitsLogicalImm($imm);
    emit {
        instr { opcode: "and"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
    }
  }

  rule ANDri_large {
  	match {
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern and($lhs, $imm);
  	}
  	cost 2;
  	cond !FitsLogicalImm($imm);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "and"; dst: $rd; src: [$lhs, $tmp]; def: $rd; uses: [$lhs, $tmp]; };
  	}
  }

  rule AND_shifted_lsl {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern and($lhs, lshl($rhs, $sh));

  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
  	  instr {
  	    opcode: "and";
  	    dst: $rd;
  	    src: [$lhs, $rhs, lsl, $sh];
  	    def: $rd;
  	    uses: [$lhs, $rhs];
  	  };
  	}
  }

  rule AND_shifted_lsr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern and($lhs, lshr($rhs, $sh));
  	}

  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
      instr {
        opcode: "and";
        dst: $rd;
        src: [$lhs, $rhs, lsr, $sh];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  rule AND_shifted_asr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern and($lhs, ashr($rhs, $sh));
  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit {
      instr {
       opcode: "and";
       dst: $rd;
       src: [$lhs, $rhs, asr, $sh];
       def: $rd;
     uses: [$lhs, $rhs];
     };
  	}
  }

  // ----- OR -----
  rule ORrr {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern or($lhs, $rhs);
	}
	cost 1;
	emit {
	  instr { opcode: "orr"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule ORri {
    match {
      commutative;
        out $rd : GPR:virt;
        in $lhs : GPR:virt;
        in $imm : imm;
        pattern or($lhs, $imm);
    }
    cost 1;
    cond FitsLogicalImm($imm);
    emit { instr { opcode: "orr"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };}
  }

  rule ORri_large {
  	match {
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern or($lhs, $imm);
  	}
  	cost 2;
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "orr"; dst: $rd; src: [$lhs, $tmp]; def: $rd; uses: [$lhs, $tmp]; };
  	}
  }

  rule OR_shifted_lsl {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern or($lhs, lshl($rhs, $sh));
  	}
  	cost 2;
  	cond ShamtFits6($sh);
  	emit { instr { opcode: "orr"; dst: $rd; src: [$lhs, $rhs, lsl, $sh]; def: $rd; uses: [$lhs, $rhs]; };}
  }

  rule OR_shifted_lsr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern or($lhs, lshr($rhs, $sh));
  	}
  	cost 2;
    cond ShamtFits6($sh);
    emit { instr { opcode: "orr"; dst: $rd; src: [$lhs, $rhs, lsr, $sh]; def: $rd; uses: [$lhs, $rhs]; };}
  }

  rule OR_shifted_asr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern or($lhs, ashr($rhs, $sh));
  	}
  	cost 2;
    cond ShamtFits6($sh);
    emit { instr { opcode: "orr"; dst: $rd; src: [$lhs, $rhs, asr, $sh]; def: $rd; uses: [$lhs, $rhs]; };}
  }

  // ----- XOR -----
  rule XORrr {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern xor($lhs, $rhs);
	}
	cost 1;
	emit {
	  instr { opcode: "eor"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule XORri {
    match {
      commutative;
        out $rd : GPR:virt;
        in $lhs : GPR:virt;
        in $imm : imm;
        pattern xor($lhs, $imm);
    }
    cost 1;
    cond FitsLogicalImm($imm);
    emit { instr { opcode: "eor"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; }; }
  }

  rule XORri_large {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  temp $tmp : GPR:virt;
	  pattern xor($lhs, $imm);
	}
	cost 2;
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "eor"; dst: $rd; src: [$lhs, $tmp]; def: $rd; uses: [$lhs, $tmp]; };
	}
  }

  rule XOR_shifted_lsl {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern xor($lhs, lshl($rhs, $sh));
  	}
  	cost 2;
    cond ShamtFits6($sh);
    emit { instr { opcode: "eor"; dst: $rd; src: [$lhs, $rhs, lsl, $sh]; def: $rd; uses: [$lhs, $rhs]; }; }
  }

  rule XOR_shifted_lsr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern xor($lhs, lshr($rhs, $sh));
  	}
  	cost 2;
    cond ShamtFits6($sh);
    emit { instr { opcode: "eor"; dst: $rd; src: [$lhs, $rhs, lsr, $sh]; def: $rd; uses: [$lhs, $rhs]; };}
  }

  rule XOR_shifted_asr {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $rhs : GPR:virt;
  	  in $sh : imm;
  	  pattern xor($lhs, ashr($rhs, $sh));
  	}
  	cost 2;
    cond ShamtFits6($sh);
    emit { instr { opcode: "eor"; dst: $rd; src: [$lhs, $rhs, asr, $sh]; def: $rd; uses: [$lhs, $rhs]; };}
  }

  // neg(rd, rs)  ->  sub rd, xzr, rs
  //
  // ARM64 exposes negation as a subtract-from-zero form.  The zero register
  // is modelled explicitly so the rule stays visible in the selector output.
  rule NEGrr {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  in $xzr : GPR:phys;
	  pattern neg($rs);
	}
	cost 1;
	emit {
	  instr { opcode: "sub"; dst: $rd; src: [$xzr, $rs]; def: $rd; uses: [$xzr, $rs]; };
	}
  }

  // ----- Multiplication / Division -----
  // mul(rd, lhs, rhs)  ->  mul rd, lhs, rhs
  rule MULrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern mul($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "mul"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule mul_add {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  in $addend : GPR:virt;
	  pattern add(mul($lhs, $rhs), $addend);
	}
	cost 2;
    emit {
        instr {
          opcode: "madd";
          dst: $rd;
          src: [$lhs, $rhs, $addend];
          def: $rd;
          uses: [$lhs, $rhs, $addend];
        };
    }
  }

  rule mul_sub {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  in $subend : GPR:virt;
	  pattern sub($subend, mul($lhs, $rhs));
	}
	cost 2;
    emit {
        instr {
          opcode: "msub";
          dst: $rd;
          src: [$lhs, $rhs, $subend];
          def: $rd;
          uses: [$lhs, $rhs, $subend];
        };
    }
  }

  // sdiv(rd, lhs, rhs)  ->  sdiv rd, lhs, rhs
  rule SDIVrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern div($lhs, $rhs);
	}
	cost 4;
	emit {
	  instr { opcode: "sdiv"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  // udiv(rd, lhs, rhs)  ->  udiv rd, lhs, rhs
  //
  // The current MIR does not distinguish signed/unsigned division at the
  // pattern level, so this rule is intentionally conservative and should only
  // be selected if the source IR has already encoded the unsigned intent.
  rule UDIVrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern udiv($lhs, $rhs);
	}
	cost 4;
	emit {
	  instr { opcode: "udiv"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
	}
  }

  rule SREMrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  temp $tmp : GPR:virt;
	  pattern rem($lhs, $rhs);
	}
	cost 4;
	emit {
	  instr { opcode: "sdiv"; dst: $tmp; src: [$lhs, $rhs]; def: $tmp; uses: [$lhs, $rhs]; };
	  instr { opcode: "msub"; dst: $rd; src: [$tmp, $rhs, $lhs]; def: $rd; uses: [$tmp, $rhs, $lhs]; };
    }
  }

  rule UREMrr {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  temp $tmp : GPR:virt;
	  pattern urem($lhs, $rhs);
	}
	cost 4;
	emit {
	  instr { opcode: "udiv"; dst: $tmp; src: [$lhs, $rhs]; def: $tmp; uses: [$lhs, $rhs]; };
	  instr { opcode: "msub"; dst: $rd; src: [$tmp, $rhs, $lhs]; def: $rd; uses: [$tmp, $rhs, $lhs]; };
	}
  }

  // ----- Bitwise complement / logical not -----
  // not(rd, rs)  ->  mvn rd, rs
  rule NOTrr {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  pattern not($rs);
	}
	cost 1;
	emit {
	  instr { opcode: "mvn"; dst: $rd; src: [$rs]; def: $rd; uses: [$rs]; };
	}
  }

  // ----- Shifts -----
  // lshl(rd, rs, sh)  ->  lsl rd, rs, #sh
  rule LSLri {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  in $sh : imm;
	  pattern lshl($rs, $sh);
	}
	cost 1;
	cond ShamtFits6($sh);
	emit {
	  instr { opcode: "lsl"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
	}
  }

  // lshr(rd, rs, sh)  ->  lsr rd, rs, #sh
  rule LSRri {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  in $sh : imm;
	  pattern lshr($rs, $sh);
	}
	cost 1;
	cond ShamtFits6($sh);
	emit {
	  instr { opcode: "lsr"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
	}
  }

  // ashr(rd, rs, sh)  ->  asr rd, rs, #sh
  rule ASRri {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  in $sh : imm;
	  pattern ashr($rs, $sh);
	}
	cost 1;
	cond ShamtFits6($sh);
	emit {
	  instr { opcode: "asr"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
	}
  }

  rule ADDR_sym {
	match {
	  out $rd : GPR:virt;
	  in $sym : symbol;
	  pattern addr($sym);
	}
	cost 0;
	emit {
	  instr { opcode: "adrp"; dst: $rd; src: [pagehi($sym)]; def: $rd; };
	  instr { opcode: "add"; dst: $rd; src: [$rd, pageoff($sym)]; def: $rd; uses: [$rd]; };
	}
  }
}

// ---------------------------------------------------------------------------
// Move lowering.
//
// MIR `MoveInstr` values are represented explicitly in the selector as a
// single-source copy.  We keep the rule intentionally generic so register,
// immediate, and symbol moves can all flow through the same lowering path.
// ---------------------------------------------------------------------------

block move {

  // mov(rd, rs) -> add rd, rs, #0
  //
  // ARM64 has no dedicated copy instruction in the MIR-to-machine lowering
  // sense; the common register-to-register move can be expressed as an add of
  // zero.  This keeps the destination in a register and preserves the source
  // operand as a normal virtual register.
  rule MOVrr {
	match {
	  out $rd : GPR:virt;
	  in $rs : GPR:virt;
	  pattern mov($rs);
	}
	cost 1;
	emit {
	  instr { opcode: "add"; dst: $rd; src: [$rs, 0]; def: $rd; uses: [$rs]; };
	}
  }

  rule MOVimm {
	match {
	  out $rd : GPR:virt;
      in $imm : imm;
      pattern mov($imm);
	}
	cost 1;
	cond UImmFits16($imm);
	emit {
	  instr { opcode: "movz"; dst: $rd; src: [$imm]; def: $rd; };
	}
  }

  rule MOV_large_imm {
    match {
      out $rd : GPR:virt;
      in $imm : imm;
      pattern mov($imm);
    }
    cost 4;
    cond !UImmFits16($imm);
    emit {
      instr { opcode: "movz"; dst: $rd; src: [imm16_0($imm)]; def: $rd; };
      instr { opcode: "movk"; dst: $rd; src: [$rd, imm16_1($imm), 16]; def: $rd; uses: [$rd]; };
      instr { opcode: "movk"; dst: $rd; src: [$rd, imm16_2($imm), 32]; def: $rd; uses: [$rd]; };
      instr { opcode: "movk"; dst: $rd; src: [$rd, imm16_3($imm), 48]; def: $rd; uses: [$rd]; };
    }
  }

  // mov(rd, xzr) -> add rd, xzr, #0
  //
  // This is the zero-register materialization form: copying from the zero
  // register produces an explicit zero in the destination register.
  rule MOVzero {
	match {
	  out $rd : GPR:virt;
	  in $xzr : GPR:phys;
	  pattern mov($xzr);
	}
	cost 1;
	emit {
	  instr { opcode: "add"; dst: $rd; src: [$xzr, 0]; def: $rd; uses: [$xzr]; };
	}
  }

  // Fallback copy materialization for non-register operands (immediates,
  // symbols, or any other source the MIR may carry).
  //rule MOVany {
//	match {
//	  out $rd : GPR:virt;
//	  in $src : any;
//	  pattern mov($src);
//	}
//	cost 1;
//	emit {
//	  instr { opcode: "mov"; dst: $rd; src: [$src]; def: $rd; uses: [$src]; };
//	}
 // }

}

// ---------------------------------------------------------------------------
// Memory and control-flow lowering.
//
// These rules cover the remaining MIR instruction families that are not
// handled by the target plan builders: loads, stores, jumps, conditional
// branches, returns, and halts.
// ---------------------------------------------------------------------------

block memory {

  // Base register
  rule LOADrr {
	match {
	  out $rd : GPR:virt;
	  in $ra : GPR:virt;
	  pattern load($ra);
	}
	cost 1;
	emit {
	  instr { opcode: "load"; dst: $rd; src: [$ra]; def: $rd; uses: [$ra]; };
	}
  }

  // load(rd, mem(base, offset)) -> load rd, [base + offset]
  //
  // This rule matches the MIR's explicit memory operand form and keeps the
  // address as a first-class `mem` operand through selection.
  rule LOADmem {
	match {
	  out $rd : GPR:virt;
	  in $addr : mem;
	  pattern load($addr);
	}
	cost 1;
	emit {
	  instr { opcode: "load"; dst: $rd; src: [$addr]; def: $rd; uses: [$addr]; };
	}
  }

  rule LOADsym {
	match {
	  out $rd : GPR:virt;
	  in $sym : symbol;
	  pattern load($sym);
	}
	cost 1;
	emit {
	  instr { opcode: "load"; dst: $rd; src: [$sym]; def: $rd; uses: [$sym]; };
	}
  }

  rule LOAD_large_off {
  	match {
  	  out $rd : GPR:virt;
  	  in $base : GPR:virt;
  	  in $off : imm;
  	  temp $tmp : GPR:virt;
  	  pattern load(mem($base, $off));
  	}
  	cost 2;
  	cond !SImmFits12($off);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($off)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($off), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($off), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($off), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "add"; dst: $tmp; src: [$base, $tmp]; def: $tmp; uses: [$base, $tmp]; };
  	  instr { opcode: "load"; dst: $rd; src: [$tmp]; def: $rd; uses: [$tmp]; };
  	}
  }

  // store(val, mem(base, offset)) -> store val, [base + offset]
  //
  // The address operand is carried directly as a `mem` value, which lets the
  // selector keep base/offset information intact.
  rule STOREmem {
	match {
	  in $val : any;
	  in $addr : mem;
	  pattern store($val, $addr);
	}
	cost 1;
	emit {
	  instr { opcode: "store"; src: [$val, $addr]; uses: [$val, $addr]; };
	}
  }

  rule STORErr {
	match {
	  in $val : GPR:virt;
	  in $ra : GPR:virt;
	  pattern store($val, $ra);
	}
	cost 1;
	emit {
	  instr { opcode: "store"; src: [$val, $ra]; uses: [$val, $ra]; };
	}
  }

  rule STOREimm_r {
	match {
	  in $val : imm;
	  in $ra : GPR:virt;
	  temp $tmp : GPR:virt;
	  pattern store($val, $ra);
	}
	cost 2;
	emit {
	  instr { opcode: "mov"; dst: $tmp; src: [$val]; def: $tmp; };
	  instr { opcode: "store"; src: [$tmp, $ra]; uses: [$tmp, $ra]; };
	}
  }

  rule STORESym_r {
	match {
	  in $val : symbol;
	  in $ra : GPR:virt;
	  temp $tmp : GPR:virt;
	  pattern store($val, $ra);
	}
	cost 2;
	emit {
	  instr { opcode: "mov"; dst: $tmp; src: [$val]; def: $tmp; };
	  instr { opcode: "store"; src: [$tmp, $ra]; uses: [$tmp, $ra]; };
	}
  }

  rule STORErr_sym {
	match {
	  in $val : GPR:virt;
	  in $sym : symbol;
	  pattern store($val, $sym);
	}
	cost 1;
	emit {
	  instr { opcode: "store"; src: [$val, $sym]; uses: [$val, $sym]; };
	}
  }

  rule STOREimm_sym {
	match {
	  in $val : imm;
	  in $sym : symbol;
	  temp $tmp : GPR:virt;
	  pattern store($val, $sym);
	}
	cost 2;
	emit {
	  instr { opcode: "mov"; dst: $tmp; src: [$val]; def: $tmp; };
	  instr { opcode: "store"; src: [$tmp, $sym]; uses: [$tmp, $sym]; };
	}
  }

  rule STORESym_sym {
	match {
	  in $val : symbol;
	  in $sym : symbol;
	  temp $tmp : GPR:virt;
	  pattern store($val, $sym);
	}
	cost 2;
	emit {
	  instr { opcode: "mov"; dst: $tmp; src: [$val]; def: $tmp; };
	  instr { opcode: "store"; src: [$tmp, $sym]; uses: [$tmp, $sym]; };
	}
  }

  rule STORE_large_off {
	match {
	  in $val : GPR:virt;
	  in $base : GPR:virt;
	  in $off : imm;
	  temp $tmp : GPR:virt;
	  pattern store($val, mem($base, $off));
	}
	cost 2;
	cond !SImmFits12($off);
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($off)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($off), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($off), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($off), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "add"; dst: $tmp; src: [$base, $tmp]; def: $tmp; uses: [$base, $tmp]; };
	  instr { opcode: "store"; src: [$val, $tmp]; uses: [$val, $tmp]; };
	}
  }
}

block control {

  rule JMP {
	match {
	  in $target : label;
	  pattern jmp($target);
	}
	cost 1;
	emit {
	  instr { opcode: "j"; src: [$target]; };
	}
  }

  rule BRnz {
	match {
	  in $cond : GPR:virt;
	  in $true : label;
	  in $false : label;
	  pattern br($cond, $true, $false);
	}
	cost 1;
	emit {
	  instr { opcode: "cmp"; src: [$cond, 0]; uses: [$cond]; };
	  instr { opcode: "bne"; src: [$cond, $true, $false]; uses: [$cond]; };
	}
  }

  rule RET_void {
	match {
	  pattern ret;
	}
	cost 1;
	emit {
	  instr { opcode: "ret"; };
	}
  }

  rule RET_val_r {
	match {
	  in $rs : GPR:virt;
	  pattern ret($rs);
	}
	cost 1;
	emit {
	  instr { opcode: "ret"; src: [$rs]; uses: [$rs]; };
	}
  }

  rule RET_val_imm {
	match {
	  in $val : imm;
	  pattern ret($val);
	}
	cost 2;
	emit {
	  instr { opcode: "ret"; src: [$val]; uses: [$val]; };
	}
  }

  rule RET_val_sym {
	match {
	  in $val : symbol;
	  pattern ret($val);
	}
	cost 2;
	emit {
	  instr { opcode: "ret"; src: [$val]; uses: [$val]; };
	}
  }

  rule HALT {
	match {
	  in $code : imm;
	  pattern halt($code);
	}
	cost 1;
	emit {
	  instr { opcode: "halt"; src: [$code]; };
	}
  }

  rule HALT_nocode {
	match {
	  pattern halt();
	}
	cost 2;
	emit {
	  instr { opcode: "halt"; };
	}
  }
}

// ---------------------------------------------------------------------------
// Comparisons: materialize boolean results in a register.
//
// The rules below use the ARM64 condition-code instructions as a simple and
// readable lowering strategy: compare first, then emit a boolean-producing
// instruction.  They are intentionally kept separate from arithmetic so the
// descriptor can grow in layers.
// ---------------------------------------------------------------------------

block comparisons {

  // ------ EQ --------
  rule ICMP_eq {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern eq($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
	}
  }

  rule ICMP_eq_imm {
    match {
      commutative;
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern eq($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
    }
  }

  rule ICMP_eq_large {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern eq($lhs, $imm);
  	}
  	cost 3;
  	cond !FitsAddSubImm($imm);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
  	  instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
  	}
  }

  rule ICMP_eq_shifted_lsl {
    match {
      commutative;
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern eq($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
    }
  }

  rule ICMP_eq_shifted_lsr {
    match {
      commutative;
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern eq($lhs, lshr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
    }
  }

  rule ICMP_eq_shifted_asr {
    match {
      commutative;
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern eq($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, asr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [eq]; def: $rd; };
    }
  }

  // ------ NE --------
  rule ICMP_ne {
	match {
	  commutative;
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern ne($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
	}
  }

  rule ICMP_ne_imm {
    match {
      commutative;
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern ne($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
    }
  }

  rule ICMP_ne_large {
  	match {
  	  commutative;
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern ne($lhs, $imm);
  	}
  	cost 3;
  	cond !FitsAddSubImm($imm);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
  	  instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
  	}
  }

  rule ICMP_ne_shifted_lsl {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern ne($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
    }
  }

  rule ICMP_ne_shifted_lsr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern ne($lhs, lshr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
    }
  }

  rule ICMP_ne_shifted_asr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern ne($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, asr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ne]; def: $rd; };
    }
  }

  // ------ LT --------
  rule ICMP_lt {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern lt($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
	}
  }

  rule ICMP_lt_imm {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern lt($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
    }
  }

  rule ICMP_lt_large {
  	match {
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern lt($lhs, $imm);
  	}
  	cost 2;
  	cond !FitsAddSubImm($imm);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
  	  instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
  	}
  }

  rule ICMP_lt_shifted_lsl {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern lt($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
    }
  }

  rule ICMP_lt_shifted_lsr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern lt($lhs, lshr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
    }
  }

  rule ICMP_lt_shifted_asr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern lt($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, asr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [lt]; def: $rd; };
    }
  }

  // ------ LE --------
  rule ICMP_le {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern le($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
	}
  }

  rule ICMP_le_imm {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern le($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
    }
  }

  rule ICMP_le_large {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  temp $tmp : GPR:virt;
	  pattern le($lhs, $imm);
	}
	cost 3;
	cond !FitsAddSubImm($imm);
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
	  instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
	}
  }

  rule ICMP_le_shifted_lsl {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern le($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
    }
  }

  rule ICMP_le_shifted_lsr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern le($lhs, lshr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
    }
  }

  rule ICMP_le_shifted_asr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern le($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, asr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [le]; def: $rd; };
    }
  }


  // ------ GT --------
  rule ICMP_gt {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern gt($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
	}
  }

  rule ICMP_gt_imm {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern gt($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
    }
  }

  rule ICMP_gt_large {
  	match {
  	  out $rd : GPR:virt;
  	  in $lhs : GPR:virt;
  	  in $imm : imm;
  	  temp $tmp : GPR:virt;
  	  pattern gt($lhs, $imm);
  	}
  	cost 3;
  	cond !FitsAddSubImm($imm);
  	emit {
  	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
  	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
  	  instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
  	}
  }

  rule ICMP_gt_shifted_lsl {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern gt($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
    }
  }

  rule ICMP_gt_shifted_lsr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern gt($lhs, lshr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
    }
  }

  rule ICMP_gt_shifted_asr {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern gt($lhs, ashr($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, asr, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [gt]; def: $rd; };
    }
  }

  // ------ GE --------
  rule ICMP_ge {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $rhs : GPR:virt;
	  pattern ge($lhs, $rhs);
	}
	cost 2;
	emit {
	  instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs, $rhs]; };
	  instr { opcode: "cset"; dst: $rd; src: [ge]; def: $rd; };
	}
  }

  rule ICMP_ge_imm {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : imm;
      pattern ge($lhs, $rhs);
    }
    cost 2;
    cond FitsAddSubImm($rhs);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs]; uses: [$lhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ge]; def: $rd; };
    }
  }

  rule ICMP_ge_large {
	match {
	  out $rd : GPR:virt;
	  in $lhs : GPR:virt;
	  in $imm : imm;
	  temp $tmp : GPR:virt;
	  pattern ge($lhs, $imm);
	}
	cost 3;
	cond !FitsAddSubImm($imm);
	emit {
	  instr { opcode: "movz"; dst: $tmp; src: [imm16_0($imm)]; def: $tmp; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_1($imm), 16]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_2($imm), 32]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "movk"; dst: $tmp; src: [$tmp, imm16_3($imm), 48]; def: $tmp; uses: [$tmp]; };
	  instr { opcode: "cmp"; src: [$lhs, $tmp]; uses: [$lhs, $tmp]; };
	  instr { opcode: "cset"; dst: $rd; src: [ge]; def: $rd; };
	}
  }

  rule ICMP_ge_shifted_lsl {
    match {
      out $rd  : GPR:virt;
      in  $lhs : GPR:virt;
      in  $rhs : GPR:virt;
      in  $sh  : imm;
      pattern ge($lhs, lshl($rhs, $sh));
    }
    cost 2;
    cond ShamtFits6($sh);
    emit {
        instr { opcode: "cmp"; src: [$lhs, $rhs, lsl, $sh]; uses: [$lhs, $rhs]; };
        instr { opcode: "cset"; dst: $rd; src: [ge]; def: $rd; };
     }
   }

  // ─────────────────────────────────────────────────────────────────────────
  // GEP (GetElementPointer) lowering
  //
  // Computes addresses from base pointers and array indices with strides.
  // ─────────────────────────────────────────────────────────────────────────

  // GEP with scalar base (no array indexing): just move the base
  rule GEP_base {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      pattern gep($base);
    }
    cost 1;
    emit {
      instr { opcode: "mov"; dst: $rd; src: [$base]; def: $rd; uses: [$base]; };
    }
  }

  // GEP with stride = 1: base + idx (no shift or multiply)
  rule GEP_stride_1 {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      pattern gep($base, $idx, 1);
    }
    cost 1;
    emit {
      instr { opcode: "add"; dst: $rd; src: [$base, $idx]; def: $rd; uses: [$base, $idx]; };
    }
  }

  // GEP with stride = 2: base + (idx << 1) using shift
  rule GEP_stride_2 {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      pattern gep($base, $idx, 2);
    }
    cost 1;
    emit {
      instr { opcode: "add"; dst: $rd; src: [$base, $idx, lsl, 1]; def: $rd; uses: [$base, $idx]; };
    }
  }

  // GEP with stride = 4: base + (idx << 2) using shift
  rule GEP_stride_4 {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      pattern gep($base, $idx, 4);
    }
    cost 1;
    emit {
      instr { opcode: "add"; dst: $rd; src: [$base, $idx, lsl, 2]; def: $rd; uses: [$base, $idx]; };
    }
  }

  // GEP with stride = 8: base + (idx << 3) using shift
  rule GEP_stride_8 {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      pattern gep($base, $idx, 8);
    }
    cost 1;
    emit {
      instr { opcode: "add"; dst: $rd; src: [$base, $idx, lsl, 3]; def: $rd; uses: [$base, $idx]; };
    }
  }

  // GEP with 2D indexing on power-of-2 strides (4 and 8)
  rule GEP_2d_4_8 {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $i : GPR:virt;
      in $j : GPR:virt;
      temp $tmp : GPR:virt;
      pattern gep($base, $i, 4, $j, 8);
    }
    cost 2;
    emit {
      instr { opcode: "add"; dst: $tmp; src: [$i, lsl, 2]; def: $tmp; uses: [$i]; };
      instr { opcode: "add"; dst: $tmp; src: [$tmp, $j, lsl, 3]; def: $tmp; uses: [$tmp, $j]; };
      instr { opcode: "add"; dst: $rd; src: [$base, $tmp]; def: $rd; uses: [$base, $tmp]; };
    }
  }

  // GEP with arbitrary stride: general mul+add sequence
  rule GEP_general {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      in $stride : imm;
      temp $tmp : GPR:virt;
      pattern gep($base, $idx, $stride);
    }
    cost 2;
    emit {
      instr { opcode: "mul"; dst: $tmp; src: [$idx, $stride]; def: $tmp; uses: [$idx]; };
      instr { opcode: "add"; dst: $rd; src: [$base, $tmp]; def: $rd; uses: [$base, $tmp]; };
    }
  }

  // Fallback: single dynamic index
  rule GEP_single_idx {
    match {
      out $rd : GPR:virt;
      in $base : GPR:virt;
      in $idx : GPR:virt;
      in $stride : imm;
      pattern gep($base, $idx, $stride);
    }
    cost 2;
    cond FitsAddSubImm($stride);
    emit {
      instr { opcode: "mul"; dst: $rd; src: [$idx, $stride]; def: $rd; uses: [$idx]; };
      instr { opcode: "add"; dst: $rd; src: [$base, $rd]; def: $rd; uses: [$base, $rd]; };
    }
  }


}

// Call lowering.
//
// After the call-lowering pre-pass (lower.LowerCallsInProgram) every
// *mir.CallInstr has empty Args and nil Dst.  classifyInstr therefore
// produces matchNode{op:"call", args:[callee]}, so the single-element
// pattern call($callee) matches every call regardless of original arity.
// ---------------------------------------------------------------------------

block calls {

  // Direct call to a named symbol:  bl _foo
  rule CALL_direct {
    match {
      in $callee : symbol;
      pattern call($callee);
    }
    cost 1;
    emit {
      instr { opcode: "bl"; src: [$callee]; uses: []; };
    }
  }

  // Indirect call through a register:  blr $fp
  rule CALL_indirect {
    match {
      in $callee : GPR:virt;
      pattern call($callee);
    }
    cost 1;
    emit {
      instr { opcode: "blr"; src: [$callee]; uses: [$callee]; };
    }
  }

}

}
