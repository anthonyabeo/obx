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
	  instr {
		opcode: "add";
		dst: $rd;
		src: [$lhs, $rhs];
		def: $rd;
		uses: [$lhs, $rhs];
	  };
	}
  }

  // add(rd, lhs, imm)  ->  add rd, lhs, #imm
  //
  // ARM64 encodes the add-immediate form with a compact immediate field.
  // We keep the constraint conservative here: signed 12-bit immediates only.
  // If a larger constant is needed, a later materialization rule should be
  // added above this one.
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
		opcode: "add";
		dst: $rd;
		src: [$lhs, $imm];
		def: $rd;
		uses: [$lhs];
	  };
	}
  }

  // add(rd, lhs, lshl(rhs, sh))  ->  add rd, lhs, rhs, LSL #sh
  //
  // This covers the shifted-register addressing/arithmetic form that ARM64
  // uses extensively for strength-reduced index math.
  rule ADD_shifted {
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
		src: [$lhs, $rhs, $sh];
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
	  instr {
		opcode: "sub";
		dst: $rd;
		src: [$lhs, $rhs];
		def: $rd;
		uses: [$lhs, $rhs];
	  };
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
	cond SImmFits12($imm);
	emit {
	  instr {
		opcode: "sub";
		dst: $rd;
		src: [$lhs, $imm];
		def: $rd;
		uses: [$lhs];
	  };
	}
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
	  instr {
		opcode: "sub";
		dst: $rd;
		src: [$xzr, $rs];
		def: $rd;
		uses: [$xzr, $rs];
	  };
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
	  instr {
		opcode: "mul";
		dst: $rd;
		src: [$lhs, $rhs];
		def: $rd;
		uses: [$lhs, $rhs];
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
	  instr {
		opcode: "sdiv";
		dst: $rd;
		src: [$lhs, $rhs];
		def: $rd;
		uses: [$lhs, $rhs];
	  };
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
	  instr {
		opcode: "udiv";
		dst: $rd;
		src: [$lhs, $rhs];
		def: $rd;
		uses: [$lhs, $rhs];
	  };
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
	  instr {
		opcode: "mvn";
		dst: $rd;
		src: [$rs];
		def: $rd;
		uses: [$rs];
	  };
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
	  instr {
		opcode: "lsl";
		dst: $rd;
		src: [$rs, $sh];
		def: $rd;
		uses: [$rs];
	  };
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
	  instr {
		opcode: "lsr";
		dst: $rd;
		src: [$rs, $sh];
		def: $rd;
		uses: [$rs];
	  };
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
	  instr {
		opcode: "asr";
		dst: $rd;
		src: [$rs, $sh];
		def: $rd;
		uses: [$rs];
	  };
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
}

}

