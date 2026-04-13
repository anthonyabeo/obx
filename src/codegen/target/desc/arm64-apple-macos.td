// arm64-apple-macos.td
// Instruction-selection rules for the AArch64 Apple Silicon / macOS target.
//
// Every rule group from rv64imafd.td is mirrored here with the equivalent
// AArch64 opcodes.  Condition predicate names match those registered in
// src/codegen/bud/ast/predicate.go:
//
//   UImmFits16($v)  – value fits in unsigned 16-bit range  (mov  immediate)
//   UImmFits12($v)  – value fits in unsigned 12-bit range  (add/and/orr imm)
//   ShamtFits6($s)  – shift amount fits in 6-bit range     (lsl/lsr/asr imm)
//
// Assembly-output conventions (Mach-O / Apple ARM64):
//   • Global symbols prefixed with '_'  – handled by helper.go formatters.
//   • Immediates prefixed with '#'       – handled by formatOperand.
//   • Memory operands as [base, #off]   – handled by formatMemArg.
//   • @PAGE / @PAGEOFF relocs            – handled by formatRelocArm64.


/////////////////////////////////////////////////////////////////
// Address-of Rules
/////////////////////////////////////////////////////////////////

// AddrOf – materialise the address of a global symbol into a register.
// ARM64 requires a two-instruction page + page-offset sequence.
//   adrp rd, _sym@PAGE
//   add  rd, rd, _sym@PAGEOFF
rule AddrOf {
    out GPR:virt:$rd;
    in  symbol:$sym;
    pattern addr($sym);
    cost 2;
    emit {
        instr { opcode: "adrp", dst: GPR:virt:$rd, src: [reloc:hi($sym)], def: GPR:virt:$rd };
        instr { opcode: "add",  dst: GPR:virt:$rd, src: [GPR:virt:$rd, reloc:lo($sym)], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
}


/////////////////////////////////////////////////////////////////
// Argument-passing Rules
/////////////////////////////////////////////////////////////////

rule Arg {
    in GPR:virt:$rs, imm:$idx;
    pattern argument($rs, $idx);
    emit {
        instr {
            opcode: "mov",
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
        instr { opcode: "mov", dst: arg{imm:$idx}, src: [symbol:$sym] };
    }
}


/////////////////////////////////////////////////////////////////
// Call Rules
/////////////////////////////////////////////////////////////////

// Direct call with return value  (bl + capture x0)
rule Call_direct {
    out GPR:virt:$rd;
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "bl", dst: GPR:phys:x30, src: [label:$target] };
        instr { opcode: "mov", dst: GPR:virt:$rd, src: [GPR:phys:x0], def: GPR:virt:$rd };
    }
}

// Direct call without return value
rule Call_direct {
    in  label:$target;
    pattern call($target);
    cost 2;
    emit {
        instr { opcode: "bl", src: [label:$target] };
    }
}


/////////////////////////////////////////////////////////////////
// Move Rules
/////////////////////////////////////////////////////////////////

// Load the address of a string constant.
// The helper emits this as an adrp+add pair via the "la_str" pseudo-opcode.
rule Mov_r_str_const {
    in str:$name, GPR:virt:$rs;
    pattern mov($rs, $name);
    cost 2;
    emit {
        instr { opcode: "la_str", dst: GPR:virt:$rs, src: [str:$name], def: GPR:virt:$rs };
    }
}

// Store a register value into a global symbol.
//   adrp  t0, _sym@PAGE
//   add   t0, t0, _sym@PAGEOFF
//   store [t0, #0], rs
rule Mov_sym_r {
    commutative;
    in symbol:$sym, GPR:virt:$rs;
    pattern mov($sym, $rs);
    temps GPR:virt:$t0;
    cost 3;
    emit {
        instr { opcode: "adrp",  dst: GPR:virt:$t0, src: [reloc:hi($sym)], def: GPR:virt:$t0 };
        instr { opcode: "add",   dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($sym)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
        instr { opcode: "store", dst: mem:{base=GPR:virt:$t0, offset=imm:0}, src: [GPR:virt:$rs], uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
}

// Register-to-register move.
rule MOVrr {
    in GPR:virt:$rd, GPR:virt:$rs;
    pattern mov($rd, $rs);
    cost 1;
    emit {
        instr { opcode: "mov", dst: GPR:virt:rd, src: [GPR:virt:rs], def: GPR:virt:$rd, uses: [GPR:virt:$rs] };
    }
}

// Small immediate (fits in 16 bits):  mov rd, #imm
rule MOVri_SmallImm {
    in GPR:virt:$rd, imm:$val;
    pattern mov($rd, $val);
    cost 1;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$rd, src: [imm:$val], def: GPR:virt:$rd };
    }
    cond UImmFits16($val);
}

// Large immediate (> 16 bits):  movz rd, #hi16, lsl #16  +  movk rd, #lo16
rule MOVri_LargeImm {
    in GPR:virt:$rd, imm:$val;
    pattern mov($rd, $val);
    cost 2;
    emit {
        instr { opcode: "movz", dst: GPR:virt:$rd, src: [reloc:hi($val)], def: GPR:virt:$rd };
        instr { opcode: "movk", dst: GPR:virt:$rd, src: [GPR:virt:$rd, reloc:lo($val)], def: GPR:virt:$rd, uses: [GPR:virt:$rd] };
    }
    cond !UImmFits16($val);
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
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "add", dst: GPR:virt:$rd, src: [GPR:virt:$t0, symbol:$sym], def: GPR:virt:$rd, uses: [GPR:virt:$t0] };
    }
}

// add rd, rs, #imm  (12-bit unsigned immediate)
rule ADDri {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$val;
    pattern add($rs1, $val);
    cost 1;
    emit {
        instr {
            opcode: "add",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1]
        };
    }
    cond UImmFits12($val);
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
// Subtraction Rules
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
        instr { opcode: "b", src: [label:$target] }
    }
}


/////////////////////////////////////////////////////////////////
// Comparison Rules
// ARM64 uses  cmp rs1, rs2  (sets flags) followed by  cset rd, <cc>.
/////////////////////////////////////////////////////////////////

rule ICMP_eq {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern eq($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="eq"], def: GPR:virt:$rd };
    }
}

rule ICMP_ne {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ne($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="ne"], def: GPR:virt:$rd };
    }
}

rule ICMP_gt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern gt($rs1, $rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="gt"], def: GPR:virt:$rd };
    }
}

rule ICMP_gt_imm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$val;
    temps GPR:virt:$t0;
    pattern gt($rs1, $val);
    cost 3;
    emit {
        instr { opcode: "mov",  dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$t0], uses: [GPR:virt:$rs1, GPR:virt:$t0] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="gt"], def: GPR:virt:$rd };
    }
    cond UImmFits12($val);
}

rule ICmp_ge {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ge($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="ge"], def: GPR:virt:$rd };
    }
}

rule ICMP_lt {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lt($rs1, $rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="lt"], def: GPR:virt:$rd };
    }
}

rule ICMP_le {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern le($rs1, rs2);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs1, GPR:virt:$rs2], uses: [GPR:virt:$rs1, GPR:virt:$rs2] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="le"], def: GPR:virt:$rd };
    }
}

// Conditional branch: cbnz rs, true_label ; b false_label
rule Br_bool {
    in  GPR:virt:$rs1, label:$true, label:$false;
    pattern br($rs1, $true, $false);
    cost 2;
    emit {
        instr { opcode: "cbnz", src: [GPR:virt:$rs1, label:$true],  uses: [GPR:virt:$rs1] };
        instr { opcode: "b",    src: [label:$false] };
    }
}


/////////////////////////////////////////////////////////////////
// Return Instruction Rules
/////////////////////////////////////////////////////////////////

// Void return – jump to the exit block (prologue/epilogue emitter adds ret).
rule Ret_void {
    pattern ret;
    cost 1;
    emit { instr { opcode: "b" src: [label:$target="exit"] }; }
}

// Return a register value: move into x0 then fall through to exit.
rule Ret_val {
    in  GPR:virt:$rs;
    pattern ret($rs);
    cost 1;
    emit {
        instr { opcode: "mov", dst: GPR:phys:x0, src: [GPR:virt:$rs], uses: [GPR:virt:$rs] };
    }
}

// Return the value of a global symbol.
//   adrp  t0, _sym@PAGE
//   add   t0, t0, _sym@PAGEOFF
//   load  x0, [t0, #0]
rule Ret {
    in symbol:$sym;
    pattern ret($sym);
    cost 3;
    temps GPR:virt:$t0;
    emit {
        instr { opcode: "adrp", dst: GPR:virt:$t0, src: [reloc:hi($sym)], def: GPR:virt:$t0 };
        instr { opcode: "add",  dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($sym)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
        instr { opcode: "load", dst: GPR:phys:x0,  src: [mem:{base=GPR:virt:$t0, offset=imm:0}], uses: [GPR:virt:$t0] };
    }
}


/////////////////////////////////////////////////////////////////
// Bitwise OR Rules
/////////////////////////////////////////////////////////////////

rule OR {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern or($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "orr",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

// orr rd, rs, #imm  (12-bit unsigned immediate)
rule ORI {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    pattern or($rs, $val);
    cost 1;
    emit {
        instr {
            opcode: "orr",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
    cond UImmFits12($val);
}

rule ORI_LargeImm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    temps GPR:virt:$t0;
    pattern or($rs, $val);
    cost 2;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "orr", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
    cond !UImmFits12($val);
}


/////////////////////////////////////////////////////////////////
// Bitwise AND Rules
/////////////////////////////////////////////////////////////////

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
            opcode: "and",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs, imm:$val],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
    cond UImmFits12($val);
}

rule AND_LargeImm {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs, imm:$val;
    temps GPR:virt:$t0;
    pattern and($rs, $val);
    cost 2;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "and", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
    cond !UImmFits12($val);
}


/////////////////////////////////////////////////////////////////
// Logical NOT / NEG Rules
/////////////////////////////////////////////////////////////////

// Logical NOT: rd = (rs == 0) ? 1 : 0
//   cmp  rs, xzr
//   cset rd, eq
rule Not {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern not($rs);
    cost 2;
    emit {
        instr { opcode: "cmp",  src: [GPR:virt:$rs, GPR:phys:xzr], uses: [GPR:virt:$rs, GPR:phys:xzr] };
        instr { opcode: "cset", dst: GPR:virt:$rd, src: [label:$cc="eq"], def: GPR:virt:$rd };
    }
}

// Negate immediate:  mov t0, #val ; neg rd, t0
rule NegImm {
    out GPR:virt:$rd;
    in  imm:$val;
    temps GPR:virt:$t0;
    pattern neg($val);
    cost 2;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr {
            opcode: "neg",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$t0],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$t0]
        };
    }
}

// Negate register:  neg rd, rs
rule NegReg {
    out GPR:virt:$rd;
    in  GPR:virt:$rs;
    pattern neg($rs);
    cost 1;
    emit {
        instr {
            opcode: "neg",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs]
        };
    }
}


/////////////////////////////////////////////////////////////////
// Shift Rules
/////////////////////////////////////////////////////////////////

rule SLLrr {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lshl($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "lsl",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

rule SRLrr {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern lshr($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "lsr",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

rule SRArr {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern ashr($rs1, $rs2);
    cost 1;
    emit {
        instr {
            opcode: "asr",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}

// lsl rd, rs, #shamt  (shift amount 0-63)
rule SLLI {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$sh;
    pattern lshl($rs1, $sh);
    cost 1;
    emit {
        instr {
            opcode: "lsl",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, imm:$sh],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1]
        };
    }
    cond ShamtFits6($sh);
}

// lsr rd, rs, #shamt
rule SRLI {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$sh;
    pattern lshr($rs1, $sh);
    cost 1;
    emit {
        instr { opcode: "lsr", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, imm:$sh], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
    }
    cond ShamtFits6($sh);
}

// asr rd, rs, #shamt
rule SRAI {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, imm:$sh;
    pattern ashr($rs1, $sh);
    cost 1;
    emit {
        instr { opcode: "asr", dst: GPR:virt:$rd, src: [GPR:virt:$rs1, imm:$sh], def: GPR:virt:$rd, uses: [GPR:virt:$rs1] };
    }
    cond ShamtFits6($sh);
}


/////////////////////////////////////////////////////////////////
// Multiply Rules
/////////////////////////////////////////////////////////////////

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
    cost 3;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "mul", dst: GPR:virt:$rd, src: [symbol:$sym, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$t0] };
    }
}

rule MULrr {
    commutative;
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern mul($rs1, $rs2);
    cost 3;
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
    cost 2;
    emit {
        instr { opcode: "mov", dst: GPR:virt:$t0, src: [imm:$val], def: GPR:virt:$t0 };
        instr { opcode: "mul", dst: GPR:virt:$rd, src: [GPR:virt:$rs, GPR:virt:$t0], def: GPR:virt:$rd, uses: [GPR:virt:$rs, GPR:virt:$t0] };
    }
}


/////////////////////////////////////////////////////////////////
// Division Rules
/////////////////////////////////////////////////////////////////

rule DIVrr {
    out GPR:virt:$rd;
    in  GPR:virt:$rs1, GPR:virt:$rs2;
    pattern div($rs1, $rs2);
    cost 10;
    emit {
        instr {
            opcode: "sdiv",
            dst: GPR:virt:$rd,
            src: [GPR:virt:$rs1, GPR:virt:$rs2],
            def: GPR:virt:$rd,
            uses: [GPR:virt:$rs1, GPR:virt:$rs2]
        };
    }
}


/////////////////////////////////////////////////////////////////
// Load Instruction Rules
/////////////////////////////////////////////////////////////////

// Load from a global symbol address:
//   adrp t0, _sym@PAGE
//   add  t0, t0, _sym@PAGEOFF
//   ldr  rd, [t0, #0]
rule LoadGlobal {
    out   GPR:virt:$rd;
    in    symbol:$sym;
    cost  3;
    pattern load($sym);
    temps GPR:virt:$t0;
    emit {
        instr { opcode: "adrp", dst: GPR:virt:$t0, src: [reloc:hi($sym)], def: GPR:virt:$t0 };
        instr { opcode: "add",  dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($sym)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
        instr { opcode: "load", dst: GPR:virt:$rd, src: [mem:{base=GPR:virt:$t0, offset=imm:0}], def: GPR:virt:$rd, uses: [GPR:virt:$t0] };
    }
}

// Load with base + 12-bit unsigned offset:  ldr rd, [rs, #off]
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
    cond UImmFits12($offs);
}

// Load with large offset (> 12 bits):  adrp+add to compute address, then ldr
rule LD_Large {
    out   GPR:virt:$rd;
    in    GPR:virt:$rs1, imm:$offs;
    temps GPR:virt:$t0, GPR:virt:$t1;
    cost 4;
    pattern load(add($rs1, $offs));
    emit {
        instr { opcode: "movz", dst: GPR:virt:$t0, src: [reloc:hi($offs)], def: GPR:virt:$t0 };
        instr { opcode: "movk", dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($offs)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
        instr { opcode: "add",  dst: GPR:virt:$t1, src: [GPR:virt:$rs1, GPR:virt:$t0], def: GPR:virt:$t1, uses: [GPR:virt:$rs1, GPR:virt:$t0] };
        instr { opcode: "load", dst: GPR:virt:$rd, src: [mem:{base=GPR:virt:$t1, offset=imm:0}], def: GPR:virt:$rd, uses: [GPR:virt:$t1] };
    }
    cond !UImmFits12($offs);
}


/////////////////////////////////////////////////////////////////
// Store Instruction Rules
/////////////////////////////////////////////////////////////////

// Store with base + 12-bit unsigned offset:  str rs, [rs2, #off]
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
    cond UImmFits12($offs);
}

// Store immediate constant to a global symbol:
//   mov t0, #c
//   adrp t1, _sym@PAGE ; add t1, t1, _sym@PAGEOFF
//   str t0, [t1, #0]
rule STImmGlobalMem {
    in imm:$c, symbol:$sym;
    temps GPR:virt:$t0, GPR:virt:$t1;
    pattern store($c, $sym);
    cost 4;
    emit {
        instr { opcode: "mov",   dst: GPR:virt:$t0, src: [imm:$c], def: GPR:virt:$t0 };
        instr { opcode: "adrp",  dst: GPR:virt:$t1, src: [reloc:hi($sym)], def: GPR:virt:$t1 };
        instr { opcode: "add",   dst: GPR:virt:$t1, src: [GPR:virt:$t1, reloc:lo($sym)], def: GPR:virt:$t1, uses: [GPR:virt:$t1] };
        instr { opcode: "store", dst: mem:{base=GPR:virt:$t1, offset=imm:0}, src: [GPR:virt:$t0], uses: [GPR:virt:$t0, GPR:virt:$t1] };
    }
}

// Store register to a global symbol:
//   adrp t0, _sym@PAGE ; add t0, t0, _sym@PAGEOFF
//   str rs, [t0, #0]
rule STRegGlobalMem {
    in GPR:virt:$rs1, symbol:$sym;
    temps GPR:virt:$t0;
    pattern store($rs1, $sym);
    cost 3;
    emit {
        instr { opcode: "adrp",  dst: GPR:virt:$t0, src: [reloc:hi($sym)], def: GPR:virt:$t0 };
        instr { opcode: "add",   dst: GPR:virt:$t0, src: [GPR:virt:$t0, reloc:lo($sym)], def: GPR:virt:$t0, uses: [GPR:virt:$t0] };
        instr { opcode: "store", dst: mem:{base=GPR:virt:$t0, offset=imm:0}, src: [GPR:virt:$rs1], uses: [GPR:virt:$rs1, GPR:virt:$t0] };
    }
}

