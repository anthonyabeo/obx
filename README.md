# Obx — Oberon+ Compiler

Obx is a compiler for the [Oberon+](https://oberon-lang.github.io/) programming language, targeting
**RISC-V (rv64imafd)** on Linux and **ARM64** on macOS.  It is written in Go and implements the
full compilation pipeline: module discovery → parsing → semantic analysis → IR lowering →
SSA optimisation → instruction selection → register allocation → assembly emission.

It also ships a built-in HTTP server (`obx web`) that provides a browser-based editor and a
JSON API for interactive type-checking and diagnostics.

---

## Table of Contents

- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Project Manifest — `obx.mod`](#project-manifest--obxmod)
- [Commands](#commands)
  - [obx new](#obx-new)
  - [obx check](#obx-check)
  - [obx build](#obx-build)
  - [obx web](#obx-web)
- [Standard Library](#standard-library)
- [Examples](#examples)
- [Compilation Pipeline](#compilation-pipeline)
- [Optimisation Passes](#optimisation-passes)
- [Source Layout](#source-layout)

---

## Prerequisites

| Requirement | Version |
|---|---|
| [Go toolchain](https://go.dev/doc/install) | ≥ 1.20 |
| RISC-V GCC cross-toolchain *(optional — only needed to assemble/link RISC-V output)* | any recent |
| Apple Clang / `as` *(optional — only needed to assemble/link ARM64 output on macOS)* | any recent |

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
  "name":   "calculator",
  "roots":  ["src"],
  "entry":  "Main",
  "stdlib": "/opt/obx/stdlib"
}
```

| Field | Required | Description |
|---|---|---|
| `name` | yes | Human-readable project name. |
| `roots` | yes | Source root directories (relative to the manifest). All `.obx` files under these roots are part of the project. |
| `entry` | no | Default entry module for `obx build`. Can be overridden with `--entry`. |
| `stdlib` | no | Override for the stdlib root directory. Falls back to `$OBX_STDLIB`, then to a `stdlib/` directory alongside the `obx` binary. |

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
| `--target` | `-T` | `rv64imafd` | Target architecture — used to inject the correct platform directives (`POSIX`, `LINUX`, `DARWIN`, `WINDOWS`) for stdlib selection. |
| `--define` | `-d` | | Compile-time directive: `NAME` (bool true) or `NAME=VALUE`. Repeatable. Overrides platform directives. |
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
| `--target` | `-T` | `rv64imafd` | Target architecture. Run `obx build --help` to see all registered targets. Injects platform directives automatically. |
| `--define` | `-d` | | Compile-time directive: `NAME` (bool true) or `NAME=VALUE`. Repeatable. Applied after platform directives, so can override them. |
| `--asm` | `-S` | `false` | Print generated assembly to stdout in addition to writing it to `out/`. |
| `--optlevel` | `-O` | `2` | Optimisation level `0`–`3` (see [Optimisation Passes](#optimisation-passes)). |
| `--passes` | `-P` | | Comma-separated passes to enable, overriding `-O`. |
| `--disable-passes` | `-D` | | Comma-separated passes to disable from the selected level. |
| `--verbose` | `-V` | `false` | Print per-pass IR diffs and optimisation details. |

Assembly is written to `out/<ModuleName>.s` relative to the project root.

---

### `obx web`

Starts an HTTP server with a browser-based Oberon+ editor and a JSON API for
interactive type-checking.  No project manifest or local files are required —
source code is submitted in the request body.

```
obx web [flags]
```

**Flags**

| Flag | Short | Default | Description |
|---|---|---|---|
| `--addr` | `-a` | `:8080` | `host:port` to listen on. |
| `--max-errors` | | `50` | Stop after this many errors per request. |

**Endpoints**

| Method | Path | Description |
|---|---|---|
| `GET` | `/` | Browser-based Oberon+ editor and diagnostic viewer. |
| `POST` | `/api/check` | JSON API: `{"source":"…","filename":"…"}` → diagnostics array. |
| `GET` | `/api/version` | Build and runtime information (version, Go version, OS/arch). |

CORS is enabled on all endpoints (`Access-Control-Allow-Origin: *`) so the API can be called
directly from external editors, scripts, or CI tooling.

**Example**

```shell
$ obx web
obx web  →  http://:8080
```

```shell
# Type-check a snippet via curl
$ curl -s -X POST http://localhost:8080/api/check \
    -H 'Content-Type: application/json' \
    -d '{"source":"module Hello\nbegin\nend Hello\n","filename":"Hello.obx"}' | jq .
{
  "ok": true,
  "error_count": 0,
  "diagnostics": []
}
```

---

## Standard Library

Obx ships a standard library under `stdlib/` that is **automatically
available** in every project — no `obx.mod` change required.  Eight
idiomatic modules cover I/O, file access, OS utilities, strings, math,
memory, time, and formatted output:

| Module | What it provides |
|---|---|
| `IO` | Console read/write — `Write`, `WriteLn`, `WriteInt`, `ReadLn`, `ReadInt` … |
| `Files` | File open/close/read/write/seek/delete/rename/exists |
| `OS` | `Exit`, `Env`, `GetCwd`, `SetCwd`, `Exec`, `GetPid`, `Sleep` |
| `Strings` | `Len`, `Copy`, `Append`, `Compare`, `IndexOf`, `Contains`, `ToUpper`, `Trim` … |
| `Math` | `Sin`, `Cos`, `Sqrt`, `Pow`, `Floor`, `Log`, `Pi`, `E` … |
| `Mem` | `Alloc`, `Free`, `Copy`, `Fill`, `Zero`, `Equal` over `CPOINTER TO VOID` |
| `Time` | `DateTime` record, `Now`, `Epoch`, `Clock`, `Elapsed`, `Format` |
| `Fmt` | `snprintf`-backed `Int`, `Real`, `Str`, `SprintI`, `ScanInt` … (no `IO` dependency) |

```oberon
module Hello
  import IO
begin
  IO.WriteLn("Hello, world!")
end Hello
```

### Platform selection

The stdlib contains two parallel FFI binding layers — `stdlib/posix/` and
`stdlib/win32/` — that are **both always discovered** by the module resolver
(dual-root strategy).  Wrapper modules choose between them at parse time via
`<* IF WINDOWS THEN *> … <* ELSE *> … <* END *>` compile-time directives.

`obx build` and `obx check` inject the correct boolean directives automatically
from `--target`:

| Target (`--target`) | Directives injected | C libraries linked |
|---|---|---|
| `rv64imafd` *(default)* | `POSIX=true`, `LINUX=true` | `libc`, `libm` |
| `arm64-apple-macos` / `aarch64-apple-darwin` | `POSIX=true`, `DARWIN=true` | `libc`, `libm` |
| `x86_64-pc-windows` | `WINDOWS=true` | `msvcrt`, `ucrtbase`, `kernel32` |

Platform directives are applied **before** user-supplied `--define` flags, so
they can always be overridden:

```shell
# Force the Windows stdlib layer regardless of --target
obx build --define WINDOWS=true --define POSIX=false
```

### Stdlib root resolution

The stdlib root is resolved in this order:

1. `"stdlib"` field in `obx.mod`
2. `$OBX_STDLIB` environment variable
3. A `stdlib/` directory adjacent to the `obx` binary

```shell
# Permanent override via environment variable
export OBX_STDLIB=/opt/obx/stdlib

# Per-project override in obx.mod
{ "stdlib": "/opt/obx/stdlib", … }
```

Linker flags for all required native libraries are written to `out/link.flags`
automatically.  Example link invocation for RISC-V:

```shell
riscv64-linux-gnu-gcc out/Main.s $(cat out/link.flags) -o myapp
```

See **[stdlib/README.md](stdlib/README.md)** for the full API reference.

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

### Stdlib Demo — `examples/stdlib/StdlibDemo.obx`

Exercises all eight stdlib modules in one program (I/O, strings, math, fmt,
time, files, OS, memory):

```shell
$ obx build -S -r examples/stdlib -e StdlibDemo
Building 9 module(s)  (entry: StdlibDemo)
  ...
```

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
│  5. ObxIR construction  (src/ir/obxir)                           │
│     IRBuilder lowers the desugared IR into SSA-form ObxIR:       │
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
│     ObxIR trees are matched against patterns described in the    │
│     bud DSL (src/codegen/bud) to produce target instructions.    │
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
│  9. Assembly emission  (src/codegen/target)                      │
│     The active target backend (rv64imafd or arm64-apple-macos)   │
│     formats instructions, computes the frame layout, and writes  │
│     .s files to out/.                                            │
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
stdlib/                 Standard library (auto-discovered on every build)
├── posix/              POSIX FFI bindings (libc / libm)
│   ├── Stdio.obx       stdio.h  — FILE*, printf, fopen, fread/fwrite …
│   ├── Stdlib.obx      stdlib.h — malloc, free, exit, getenv …
│   ├── StringH.obx     string.h — strlen, strcmp, memcpy …
│   ├── LibM.obx        math.h   — sin, cos, sqrt, pow …  [-lm]
│   ├── TimeH.obx       time.h   — time, clock, localtime, strftime, tm
│   └── Unistd.obx      unistd.h — getpid, getcwd, open, read, write …
├── win32/              Windows FFI bindings (msvcrt / ucrtbase / kernel32)
│   ├── Stdio.obx       msvcrt stdio
│   ├── Stdlib.obx      msvcrt stdlib
│   ├── StringH.obx     msvcrt string
│   ├── LibM.obx        ucrtbase math
│   ├── TimeH.obx       msvcrt time
│   └── WinAPI.obx      kernel32 — CreateFile, ExitProcess, GetEnvironmentVariable …
├── IO.obx              Console read / write
├── Files.obx           File open / read / write / seek
├── OS.obx              Exit, Env, GetCwd, Exec, GetPid, Sleep
├── Strings.obx         Len, Copy, Append, Compare, IndexOf, ToUpper, Trim …
├── Math.obx            Sin, Cos, Sqrt, Pow, Floor, Pi, E …
├── Mem.obx             Alloc, Free, Copy, Fill, Zero, Equal
├── Time.obx            DateTime, Now, Epoch, Clock, Elapsed, Format
├── Fmt.obx             Int, Real, Str, SprintI, ScanInt … (snprintf-backed)
└── README.md           Full stdlib API reference

cmd/
├── obx.go              Binary entry point
├── cli/                Cobra command definitions (new, check, build, web)
└── web/                HTTP server, route handlers, and embedded static UI

src/
├── support/            Infrastructure shared across all stages
│   ├── source/         Source-file and source-location tracking
│   ├── compiler/       Shared compiler context (diagnostics, directives)
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
│   └── obxir/          ObxIR — SSA three-address code with basic blocks,
│                       φ-nodes, CFG, and DOT CFG visualisation; includes
│                       the IRBuilder and a pretty-printer
│
├── opt/                Optimisation passes (all operate on ObxIR)
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
│       ├── riscv/      RISC-V rv64imafd implementation
│       └── arm64/      ARM64 (apple-macos) implementation
│
└── project/            Project manifest (obx.mod) — loading, writing,
                        project-root discovery, module-graph resolution
```
