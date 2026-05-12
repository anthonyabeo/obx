target rv64imafd {

// ---------------------------------------------------------------------------
// Arithmetic lowering for RV64IMAFD.
//
// This file starts with the core integer arithmetic forms and then extends
// into shifts, bitwise logic, negation, multiplication/division, and boolean
// comparisons.  The rule ordering is deliberate: the cheapest/most direct
// encodings are listed before fallbacks that need extra immediates or temps.
// ---------------------------------------------------------------------------

block arithmetic {

  // ----- Addition -----
  // add(rd, lhs, rhs) -> add rd, lhs, rhs
  //
  // Canonical register/register addition.  This is the default form when both
  // operands are already in registers.
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
      instr {
        opcode: "add";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  // add(rd, lhs, imm) -> addi rd, lhs, imm
  //
  // The immediate form is cheaper than materializing a constant into a temp.
  // We keep it constrained to signed 12-bit immediates, which matches the
  // common RV64 addi encoding range.
  rule ADDri {
    match {
      commutative;
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $imm : imm;
      pattern add($lhs, $imm);
    }
    cost 1;
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "addi";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
    }
  }

  // add(rd, lhs, add(rhs, imm)) -> addi/add folding form.
  //
  // This form captures the common case where a small offset appears on the
  // right-hand side of a nested add tree.  It is kept slightly more expensive
  // than the plain immediate form so the selector prefers the direct form when
  // possible.
  rule ADD_nested_ri {
    match {
      commutative;
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      in $imm : imm;
      pattern add($lhs, add($rhs, $imm));
    }
    cost 2;
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "addi";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
      instr {
        opcode: "add";
        dst: $rd;
        src: [$rd, $rhs];
        def: $rd;
        uses: [$rd, $rhs];
      };
    }
  }

  // ----- Subtraction -----
  rule SUBrr {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern sub($lhs, $rhs);
    }
    cost 1;
    emit {
      instr {
        opcode: "sub";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  rule SUBri {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $imm : imm;
      pattern sub($lhs, $imm);
    }
    cost 1;
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "addi";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
    }
  }

  // neg(rd, rs) -> sub rd, x0, rs
  //
  // RV64 does not have a dedicated negate opcode; subtracting from the zero
  // register is the standard lowering.
  rule NEGrr {
    match {
      out $rd : GPR:virt;
      in $rs : GPR:virt;
      pattern neg($rs);
    }
    cost 1;
    emit {
      instr {
        opcode: "sub";
        dst: $rd;
        src: [x0, $rs];
        def: $rd;
        uses: [x0, $rs];
      };
    }
  }

  // ----- Bitwise logic -----
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
      instr {
        opcode: "and";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
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
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "andi";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
    }
  }

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
      instr {
        opcode: "or";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
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
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "ori";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
    }
  }

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
      instr {
        opcode: "xor";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
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
    cond SImmFits12($imm);
    emit {
      instr {
        opcode: "xori";
        dst: $rd;
        src: [$lhs, $imm];
        def: $rd;
        uses: [$lhs];
      };
    }
  }

  // ----- Multiplication / Division -----
  rule MULrr {
    match {
      commutative;
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern mul($lhs, $rhs);
    }
    cost 2;
    emit {
      instr {
        opcode: "mul";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  rule DIVrr {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern div($lhs, $rhs);
    }
    cost 4;
    emit {
      instr {
        opcode: "div";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  // ----- Shifts -----
  rule SLLri {
    match {
      out $rd : GPR:virt;
      in $rs : GPR:virt;
      in $sh : imm;
      pattern lshl($rs, $sh);
    }
    cost 1;
    cond ShamtFits6($sh);
    emit {
      instr {
        opcode: "slli";
        dst: $rd;
        src: [$rs, $sh];
        def: $rd;
        uses: [$rs];
      };
    }
  }

  rule SRLri {
    match {
      out $rd : GPR:virt;
      in $rs : GPR:virt;
      in $sh : imm;
      pattern lshr($rs, $sh);
    }
    cost 1;
    cond ShamtFits6($sh);
    emit {
      instr {
        opcode: "srli";
        dst: $rd;
        src: [$rs, $sh];
        def: $rd;
        uses: [$rs];
      };
    }
  }

  rule SRAri {
    match {
      out $rd : GPR:virt;
      in $rs : GPR:virt;
      in $sh : imm;
      pattern ashr($rs, $sh);
    }
    cost 1;
    cond ShamtFits6($sh);
    emit {
      instr {
        opcode: "srai";
        dst: $rd;
        src: [$rs, $sh];
        def: $rd;
        uses: [$rs];
      };
    }
  }
}

// ---------------------------------------------------------------------------
// Comparisons: boolean results are produced with compare + set-less-than
// style instruction sequences.
// ---------------------------------------------------------------------------

block comparisons {

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
      instr {
        opcode: "xor";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
      instr {
        opcode: "seqz";
        dst: $rd;
        src: [$rd];
        def: $rd;
        uses: [$rd];
      };
    }
  }

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
      instr {
        opcode: "xor";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
      instr {
        opcode: "snez";
        dst: $rd;
        src: [$rd];
        def: $rd;
        uses: [$rd];
      };
    }
  }

  rule ICMP_lt {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern lt($lhs, $rhs);
    }
    cost 1;
    emit {
      instr {
        opcode: "slt";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
    }
  }

  rule ICMP_le {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern le($lhs, $rhs);
    }
    cost 2;
    emit {
      instr {
        opcode: "slt";
        dst: $rd;
        src: [$rhs, $lhs];
        def: $rd;
        uses: [$rhs, $lhs];
      };
      instr {
        opcode: "seqz";
        dst: $rd;
        src: [$rd];
        def: $rd;
        uses: [$rd];
      };
    }
  }

  rule ICMP_gt {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern gt($lhs, $rhs);
    }
    cost 2;
    emit {
      instr {
        opcode: "slt";
        dst: $rd;
        src: [$rhs, $lhs];
        def: $rd;
        uses: [$rhs, $lhs];
      };
    }
  }

  rule ICMP_ge {
    match {
      out $rd : GPR:virt;
      in $lhs : GPR:virt;
      in $rhs : GPR:virt;
      pattern ge($lhs, $rhs);
    }
    cost 2;
    emit {
      instr {
        opcode: "slt";
        dst: $rd;
        src: [$lhs, $rhs];
        def: $rd;
        uses: [$lhs, $rhs];
      };
      instr {
        opcode: "seqz";
        dst: $rd;
        src: [$rd];
        def: $rd;
        uses: [$rd];
      };
    }
  }
}

}
