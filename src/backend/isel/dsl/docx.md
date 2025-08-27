# Instruction Selection DSL Reference

## Overview
This DSL describes **tree-pattern matching rules** for instruction selection in an RV64IFMD backend.

- Each **rule** matches an IR pattern, checks optional conditions, and emits one or more assembly instructions.
- Rules declare inputs, outputs, and temporary registers.
- Conditions call Go predicates (registered at runtime) to constrain applicability.
- A **cost** value selects the cheapest applicable rule.
- Instruction templates use operand variables (`$rd`, `$rs1`, `$imm`) and assembler macros (`%hi`, `%lo`) that pass through unchanged.

---

## Grammar (EBNF)
```ebnf
Header      = "header" "{" { HeaderField } "}" ;
HeaderField = Ident ":" Value ;
Value       = String | Number | Ident ;

Rule        = "rule" Ident "{" 
                Pattern
                OutDecl
                InDecl
                [TempDecl]
                [CondDecl]
                CostDecl
                InstrList
              "}" ;

Pattern     = "pattern" PatternExpr ";" ;
PatternExpr = Ident [ "(" [PatternArgs] ")" ] ;
PatternArgs = PatternExpr { "," PatternExpr } ;

OutDecl     = "out" OperandList ";" ;
InDecl      = "in" OperandList ";" ;
TempDecl    = "temps" OperandList ";" ;
OperandList = Operand { "," Operand } ;
Operand     = Ident ":" Ident ;

CondDecl    = "cond" CondList ";" ;
CondList    = Cond { "," Cond } ;
Cond        = Ident [ "(" [CondArgs] ")" ] ;
CondArgs    = Ident { "," Ident } ;

CostDecl    = "cost" Number ";" ;

InstrList   = "insn" "{" { Instr } "}" ;
Instr       = String ;
```
---
## Header
The header contains structured metadata as free-form key/value pairs.
It does not affect rule matching directly, but provides context such as target ISA, ABI, or version.

### Grammar
```ebnf
Header      = "header" "{" { HeaderField } "}" ;
HeaderField = Ident ":" Value ;
Value       = String | Number | Ident ;
```
- **Ident** → a bare identifier (ISA, ABI, Author, etc.)
- **String** → quoted text ("RV64IFMD", "Alice")
- **Number** → integer or float (0.3, 64)

### Example
```ebnf
header {
    ISA: "RV64IFMD"
    ABI: "LP64D"
    Version: 0.3
    Author: "Alice"
    Notes: "Imm offset must fit 12-bit"
    Notes: "TODO: add AMOs"
}
```
This would be parsed into:
```ebnf
ISA     → ["RV64IFMD"]
ABI     → ["LP64D"]
Version → ["0.3"]
Author  → ["Alice"]
Notes   → ["Imm offset must fit 12-bit", "TODO: add AMOs"]
```
---
## Operands
Operands bind symbolic variables to operand classes.
### Format:
```ebnf
Kind:Name
```

### Examples:
- `GPR:$rd` → general-purpose register bound to $rd.
- `imm:$offs` → immediate operand bound to $ofs.
- `FPR:$f1` → floating-point register bound to $f1.

### Sections
- **out**: destination operands.
- **in**: source operands.
- **temps**: compiler-allocated temporaries (used when an instruction requires decomposition).

| Class   | Meaning                   | Notes                                    |
| ------- | ------------------------- | ---------------------------------------- |
| `GPR`   | General-purpose register  | Integer ops (`x0`–`x31`)                 |
| `FPR`   | Floating-point register   | Float ops (`f0`–`f31`)                   |
| `imm`   | Immediate                 | Literal constant, may require size check |
| `csr`   | Control & status register | For privileged ops                       |
| `addr`  | Address expression        | Usually decomposed into `GPR + offset`   |
| `label` | Label/symbol              | For jumps/calls                          |

---

## Conditions
Optional predicates that restrict when a rule is valid.
- Declared with `cond`.
- Multiple conditions may be listed, separated by commas.
- Conditions correspond to Go functions registered in a predicate registry.

### Examples:
```ebnf
cond SImmFits12($offs);
cond isPowerOf2($imm), nonZero($imm);
```

| Condition        | Arguments | Meaning                                        |
|------------------| --------- | ---------------------------------------------- |
| `SImmFits12(x)`  | imm       | Checks if immediate fits signed 12-bit field   |
| `UImmFits12(x)`  | imm       | Checks if immediate fits unsigned 12-bit field |
| `immTooLarge(x)` | imm       | True if value does not fit simple encoding     |
| `isPowerOf2(x)`  | imm       | True if `x` is power of 2                      |
| `nonZero(x)`     | imm/reg   | Disallows zero                                 |
| `ShamtFits6(x)`  | imm       | Valid shift amount (0–63)                      |
| `aligned(x, n)`  | addr/imm  | True if `x` is aligned to `n` bytes            |


---

## Cost
Each rule has an integer cost.
- Lower = more desirable.
- Used when multiple rules match the same pattern.

### Example:
```ebnf
cost 1;
```
---

## Instruction Templates
- Declared inside insn { ... }.
- Each string represents one emitted instruction.
- Variables are substituted at emission time.
- Assembler macros like %hi and %lo are passed through unchanged.

### Example:
```ebnf
insn {
    "LUI $t0, %hi($offs)",
    "ADDI $t0, $t0, %lo($offs)",
    "ADD  $t0, $rs1, $t0",
    "LD   $rd, 0($t0)"
}
```
---

## Examples
### Example 1: Load with small offset
```ebnf
rule LoadSmall {
    pattern load(add($rs1, $ofs));
    out   GPR:$rd;
    in    GPR:$rs1, imm:$ofs;
    cost  1;
    insn { "LD $rd, $ofs($rs1)" }
}
```

### Example 2: Load with large offset
```ebnf
rule LoadLarge {
    pattern load(add($rs1, $offs));
    out   GPR:$rd;
    in    GPR:$rs1, imm:$offs;
    temps GPR:$t0;
    cond  immTooLarge($ofs);
    cost  2;
    insn {
        "LUI $t0, %hi($ofs)",
        "ADDI $t0, $t0, %lo($offs)",
        "ADD  $t0, $rs1, $t0",
        "LD   $rd, 0($t0)"
    }
}
```
---

## Best Practices
- Always provide a cheap rule (fast path) and a fallback rule (decomposed with temps).
- Keep costs proportional (1 for single instruction, 2+ for multi-instruction).
- Use temps sparingly: they increase register pressure.
- Keep conditions pure: no side effects, deterministic for same inputs.