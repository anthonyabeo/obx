# Obx — Oberon+ Compiler

Obx is a compiler for the [Oberon+](https://oberon-lang.github.io/) programming language, currently
targeting **RISC-V (rv64imafd)** on Linux.  It is written in Go and implements the full compilation
pipeline: module discovery → parsing → semantic analysis → IR lowering → SSA optimisation →
instruction selection → register allocation → assembly emission.

---

## Table of Contents

- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Project Manifest — `obx.mod`](#project-manifest--obxmod)
- [Commands](#commands)
  - [obx new](#obx-new)
  - [obx check](#obx-check)
  - [obx build](#obx-build)
- [Examples](#examples)
- [Compilation Pipeline](#compilation-pipeline)
- [Optimisation Passes](#optimisation-passes)
- [Source Layout](#source-layout)

---

## Prerequisites

| Requirement | Version |
|---|---|
| [Go toolchain](https://go.dev/doc/install) | ≥ 1.20 |
| RISC-V GCC cross-toolchain *(optional — only needed to assemble/link output)* | any recent |

---

## Installation

```shell
# 1. Clone the repository
git clone https://github.com/anthonyabeo/obx.git
cd obx

# 2. Run the test suite
go test ./...

# 3. Install the binary to $GOBIN
go install ./cmd/obx.go
```

Make sure `$GOBIN` (or `$GOPATH/bin`) is on your `$PATH`.

---

## Project Manifest — `obx.mod`

Every Obx project is described by an `obx.mod` file at its root.  It is a small JSON file:

```json
{
  "name":  "calculator",
  "roots": ["src"],
  "entry": "Main"
}
```

| Field | Description |
|---|---|
| `name` | Human-readable project name. |
| `roots` | List of source root directories (relative to the manifest). All `.obx` files under these roots are part of the project. |
| `entry` | Default entry module for `obx build`. Can be overridden with `--entry`. |

`obx` walks up from the working directory to locate `obx.mod` automatically, so commands can be run
from any subdirectory of the project.

---

## Commands

### `obx new`

Bootstraps a new Oberon+ project directory.

```
obx new <project-name> [flags]
```

**Flags**

| Flag | Short | Default | Description |
|---|---|---|---|
| `--src` | `-s` | `src` | Name of the source root directory to create inside the project. |
| `--entry` | `-e` | `Main` | Name of the entry module to scaffold. |

**Example**

```shell
$ obx new calculator
Created "calculator"

  calculator/obx.mod          project manifest
  calculator/src/Main.obx     entry module

Next steps:
  cd calculator
  obx check
  obx build
```

Generated layout:

```
calculator/
├── obx.mod
└── src/
    └── Main.obx
```

---

### `obx check`

Parses and type-checks all modules without producing any output files.  Runs the full front-end
pipeline (module discovery, parsing, name resolution, type checking, and control-flow analysis).

```
obx check [flags]
```

**Flags**

| Flag | Short | Default | Description |
|---|---|---|---|
| `--path` | `-p` | *(obx.mod)* | Source root directory. Falls back to roots listed in `obx.mod`. |
| `--tabWidth` | `-t` | `4` | Number of spaces per tab for diagnostic rendering. |
| `--max-errors` | | `32` | Stop after this many errors. |
| `--quiet` | `-q` | `false` | Suppress progress output; print diagnostics only. |

Exit code is `0` when all modules are clean, `1` when any errors are found.

**Example**

```shell
$ obx check
Checking 3 module(s) in /home/user/calculator
  calculator/Main       src/Main.obx
  calculator/Math       src/Math.obx
  calculator/Utils      src/Utils.obx

ok      3 module(s) checked, no errors
```

---

### `obx build`

Runs the full compilation pipeline and writes assembly files to `out/`.

```
obx build [flags]
```

**Flags**

| Flag | Short | Default | Description |
|---|---|---|---|
| `--root` | `-r` | *(obx.mod)* | Source root directory. Repeatable. Falls back to `roots` in `obx.mod`. |
| `--entry` | `-e` | *(obx.mod)* | Entry module. Falls back to `entry` in `obx.mod`; omit to build all modules. |
| `--output` | `-o` | | Name of the output file to produce. |
| `--asm` | `-S` | `false` | Print generated assembly to stdout in addition to writing it to `out/`. |
| `--optlevel` | `-O` | `2` | Optimisation level `0`–`3` (see [Optimisation Passes](#optimisation-passes)). |
| `--passes` | `-P` | | Comma-separated passes to enable, overriding `-O`. |
| `--disable-passes` | `-D` | | Comma-separated passes to disable from the selected level. |
| `--verbose` | `-V` | `false` | Print per-pass IR diffs and optimisation details. |
| `--tabWidth` | `-t` | `4` | Number of spaces per tab for diagnostic rendering. |

Assembly is written to `out/<ModuleName>.s` relative to the project root.

---

## Examples

### Loop — `examples/loop/loop.obx`

```oberon
MODULE LoopTest;
    VAR x, y: INTEGER;

    BEGIN
        x := 0;
        y := 20;
        WHILE x < y DO
            x := x + 1
        END
        printf("Final x: %d\n", x)
END LoopTest.
```

```shell
$ obx build -S -r examples/loop
Building 1 module(s)  (entry: LoopTest)
  loop/LoopTest           examples/loop/loop.obx
```

<details>
<summary>Generated RISC-V assembly (rv64imafd)</summary>

```asm
	.section .bss
	.align 2
x: .skip 4

	.align 2
y: .skip 4

	.section .rodata
str_const_0: .string "Final x: %d\n"

	.section .text
	.align 2
	.globl main
	.type main, @function
main:
	addi sp, sp, -48
	sd fp, 40(sp)
	sd ra, 32(sp)
	sd s11, 24(sp)
	sd s10, 16(sp)
	sd s9, 8(sp)
	addi fp, sp, 48
	li s11, 0
	la t0, x
	sw s11, 0(t0)
	li s11, 20
	la t1, y
	sw s11, 0(t1)
	j while_loop_0_1

while_loop_0_1:
	la t2, x
	lw s11, 0(t2)
	la t3, y
	lw s10, 0(t3)
	slt s9, s11, s10
	seqz s10, s9
	bne s10, x0, while_loop_0_exit_2
	j if_end_3

if_end_3:
	li s10, 1
	la t4, x
	lw s9, 0(t4)
	add s11, s9, s10
	la t5, x
	sw s11, 0(t5)
	j while_loop_0_1

while_loop_0_exit_2:
	la s11, str_const_0
	mv a0, s11
	la t6, x
	lw s11, 0(t6)
	mv a1, s11
	jal ra, printf
	mv s11, a0
	j __init_LoopTest_exit

__init_LoopTest_exit:
	ld s9, 8(sp)
	ld s10, 16(sp)
	ld s11, 24(sp)
	ld ra, 32(sp)
	ld fp, 40(sp)
	addi sp, sp, 48
	ret
	.size main, .-main
```

</details>

### Fibonacci — `examples/demo/Fibonacci.obx`

```oberon
module Fibonacci
  proc calc*(n : integer): integer
    var a, b: integer
  begin
    if n > 1 then
      a := calc(n - 1)
      b := calc(n - 2)
      return a + b
    elsif n = 0 then
      return 0
    else
      return 1
    end
  end calc
  var res: integer
begin
  res := calc(21)
  assert(res = 10946)
end Fibonacci
```

```shell
$ obx build -S -r examples/demo -e Fibonacci
```

---

## Compilation Pipeline

```
Source files
    │
    ▼
┌──────────────────────────────────────────────────────────────────┐
│  1. Module discovery & topological sort  (src/project)           │
│     Walk source roots, read module headers, build the import     │
│     graph, and return modules in dependency-first order.         │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  2. Parsing  (src/syntax)                                        │
│     Scanner tokenises each file; the recursive-descent parser    │
│     produces an AST whose nodes carry position info and will     │
│     later be annotated with resolved types.                      │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  3. Semantic analysis  (src/sema)                                │
│     Name resolution, type checking (src/sema/types), and        │
│     control-flow analysis.  Diagnostics are emitted through      │
│     the buffered reporter (src/support/diag).                    │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  4. Desugaring — AST → Desugared IR  (src/ir/desugar)            │
│     Eliminates syntactic sugar (FOR, CASE, WITH, string ops,     │
│     built-in procedure calls) into a simpler, explicitly-typed   │
│     intermediate tree.                                           │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  5. MIR construction  (src/ir/mir)                               │
│     IRBuilder lowers the desugared IR into SSA-form MIR:         │
│     three-address instructions organised into basic blocks with  │
│     explicit φ-nodes and a CFG.                                  │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  6. Optimisation  (src/opt)                                      │
│     The PassManager runs the configured set of passes (constant  │
│     folding, DCE, SCCP, loop unrolling) to a fixed point.        │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  7. Instruction selection  (src/codegen/isel)                    │
│     MIR trees are matched against patterns described in the bud  │
│     DSL (src/codegen/bud) to produce target instructions.        │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  8. Register allocation  (src/codegen/ralloc)                    │
│     Live-range analysis builds intervals; linear-scan assigns    │
│     virtual registers to physical ones and inserts spill code.   │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  9. Assembly emission  (src/codegen/target/riscv)                │
│     The rv64imafd target formats instructions, computes the      │
│     frame layout, and writes .s files to out/.                   │
└──────────────────────────────────────────────────────────────────┘
```

---

## Optimisation Passes

Passes are selected by `-O` level or overridden with `--passes` / `--disable-passes`.

| Level | Passes enabled |
|---|---|
| `-O0` | *(none)* |
| `-O1` | `constprop`, `dce` |
| `-O2` *(default)* | `constprop`, `dce`, `sccp` |
| `-O3` | `constprop`, `dce`, `sccp`, `loopunroll` |

| Pass | Description |
|---|---|
| `constprop` | **Constant folding** — replaces instructions whose operands are all compile-time constants with a direct move of the folded value. |
| `dce` | **Dead-code elimination** — removes unreachable basic blocks and instructions whose results are never used. |
| `sccp` | **Sparse conditional constant propagation** — propagates constants through branch conditions, pruning paths that are statically unreachable. |
| `loopunroll` | **Loop unrolling** — replicates loop bodies to reduce branch overhead and expose more instruction-level parallelism. |

**Custom pass selection examples**

```shell
# Only constant folding, no DCE
$ obx build -O0 --passes constprop

# Full O3 but skip loop unrolling
$ obx build -O3 --disable-passes loopunroll

# Verbose output showing IR before and after each pass
$ obx build -O2 --verbose
```

---

## Source Layout

```
src/
├── support/            Infrastructure shared across all stages
│   ├── adt/            Generic data structures (Stack, Queue, Set)
│   ├── source/         Source-file and source-location tracking
│   ├── diag/           Diagnostic context, buffered reporter, and
│   │   └── formatter/  formatters (plain text, JSON)
│   └── testutil/       Shared test helpers for pipeline tests
│
├── syntax/             Frontend
│   ├── token/          Token kinds and literal values
│   ├── scan/           Scanner / lexer
│   ├── ast/            Parse-tree node definitions and visitor interface
│   └── parser/         Recursive-descent parser
│
├── sema/               Semantic analysis
│   ├── types/          Type system (basic, array, record, pointer,
│   │                   procedure, enum, named types)
│   ├── resolve.go      Name resolution and symbol mangling
│   ├── typecheck.go    Type checker (visitor over the AST)
│   └── flow.go         Control-flow analysis
│
├── ir/                 Intermediate representations
│   ├── desugar/        Desugared IR — explicit lowering of AST sugar
│   └── mir/            MIR — SSA three-address code with basic blocks,
│                       φ-nodes, and CFG; includes the IRBuilder
│
├── opt/                Optimisation passes (all operate on MIR)
│   ├── pass.go         Pass interface, PassManager, pass registry
│   ├── fold.go         Constant folding
│   ├── dce.go          Dead-code elimination
│   ├── ssa.go          SSA construction (dominators, φ-node placement)
│   └── control.go      CFG construction and dominance computation
│
├── codegen/            Code generation
│   ├── asm/            Assembly-level IR (symbols, instructions, blocks)
│   ├── bud/            Pattern-description DSL and its lexer/parser
│   │   ├── ast/        AST nodes for bud rules and predicates
│   │   └── parser/     bud parser
│   ├── isel/           Instruction selection via bud pattern matching
│   ├── ralloc/         Register allocation
│   │                   (liveness analysis, live intervals, linear scan,
│   │                    graph colouring)
│   └── target/         Target machine abstraction
│       ├── desc/       Machine description files (*.td)
│       └── riscv/      RISC-V rv64imafd implementation
│
└── project/            Project manifest (obx.mod) — loading, writing,
                        project-root discovery, module-graph resolution
```
