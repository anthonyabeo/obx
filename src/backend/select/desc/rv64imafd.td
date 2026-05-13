header {
  ABI: "LP64D";
}

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
      instr { opcode: "add"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "addi"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
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
      instr { opcode: "addi"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
      instr { opcode: "add"; dst: $rd; src: [$rd, $rhs]; def: $rd; uses: [$rd, $rhs]; };
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
      instr { opcode: "sub"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "addi"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
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
      instr { opcode: "sub"; dst: $rd; src: [x0, $rs]; def: $rd; uses: [x0, $rs]; };
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
    cond SImmFits12($imm);
    emit {
      instr { opcode: "andi"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
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
      instr { opcode: "or"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "ori"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
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
      instr { opcode: "xor"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "xori"; dst: $rd; src: [$lhs, $imm]; def: $rd; uses: [$lhs]; };
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
      instr { opcode: "mul"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "div"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "slli"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
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
      instr { opcode: "srli"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
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
      instr { opcode: "srai"; dst: $rd; src: [$rs, $sh]; def: $rd; uses: [$rs]; };
    }
  }
}

// ---------------------------------------------------------------------------
// Move lowering.
//
// MIR `MoveInstr` values are represented explicitly in the selector as a
// single-source copy. We keep the rule intentionally generic so register,
// immediate, and symbol moves can all flow through the same lowering path.
// ---------------------------------------------------------------------------

block move {

  // mov(rd, rs) -> addi rd, rs, 0
  //
  // RV64 uses addi with a zero immediate for the canonical register copy.
  // The selector can emit this directly without going through a generic mov
  // pseudo-op, which keeps the lowering target-specific.
  rule MOVrr {
    match {
      out $rd : GPR:virt;
      in $rs : GPR:virt;
      pattern mov($rs);
    }
    cost 0;
    emit {
      instr { opcode: "addi"; dst: $rd; src: [$rs, 0]; def: $rd; uses: [$rs]; };
    }
  }

  // mov(rd, x0) -> addi rd, x0, 0
  //
  // Copying from the architectural zero register materializes an explicit
  // zero in the destination register.
  rule MOVzero {
    match {
      out $rd : GPR:virt;
      in $x0 : GPR:phys;
      pattern mov($x0);
    }
    cost 0;
    emit {
      instr { opcode: "addi"; dst: $rd; src: [$x0, 0]; def: $rd; uses: [$x0]; };
    }
  }

  // Fallback copy materialization for non-register operands (immediates,
  // symbols, or any other source the MIR may carry).
  rule MOVany {
    match {
      out $rd : GPR:virt;
      in $src : any;
      pattern mov($src);
    }
    cost 1;
    emit {
      instr { opcode: "mov"; dst: $rd; src: [$src]; def: $rd; uses: [$src]; };
    }
  }
}

// ---------------------------------------------------------------------------
// Memory and control-flow lowering.
//
// These rules cover the remaining MIR instruction families that are not
// handled by the target plan builders: loads, stores, jumps, conditional
// branches, returns, and halts.
// ---------------------------------------------------------------------------

block memory {

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
      instr { opcode: "xor"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
      instr { opcode: "seqz"; dst: $rd; src: [$rd]; def: $rd; uses: [$rd]; };
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
      instr { opcode: "xor"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
      instr { opcode: "snez"; dst: $rd; src: [$rd]; def: $rd; uses: [$rd]; };
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
      instr { opcode: "slt"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
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
      instr { opcode: "slt"; dst: $rd; src: [$rhs, $lhs]; def: $rd; uses: [$rhs, $lhs]; };
      instr { opcode: "seqz"; dst: $rd; src: [$rd]; def: $rd; uses: [$rd]; };
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
      instr { opcode: "slt"; dst: $rd; src: [$rhs, $lhs]; def: $rd; uses: [$rhs, $lhs]; };
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
      instr { opcode: "slt"; dst: $rd; src: [$lhs, $rhs]; def: $rd; uses: [$lhs, $rhs]; };
      instr { opcode: "seqz"; dst: $rd; src: [$rd]; def: $rd; uses: [$rd]; };
    }
  }
}

}
