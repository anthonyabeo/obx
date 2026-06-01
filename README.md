# Obx — Oberon+ Compiler

Obx is a compiler for the [Oberon+](https://oberon-lang.github.io/) programming language, targeting
**RISC-V (rv64imafd)** on Linux and **ARM64** on macOS.  It is written in Go and implements the
full compilation pipeline: module discovery → parsing → semantic analysis → desugar/minir lowering →
minir optimisation + verification → backend stages (call/switch lowering, isel, legalization,
regalloc, prologue/epilogue, phi-removal) → assembly + linking.

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

Runs the full compilation pipeline and produces a runnable executable under `build/`.

```
obx build [flags]
```

**Flags**

| Flag | Short | Default | Description |
|---|---|---|---|
| `--root` | `-r` | *(obx.mod)* | Source root directory. Repeatable. Falls back to `roots` in `obx.mod`. |
| `--entry` | `-e` | *(obx.mod)* | Entry module. Falls back to `entry` in `obx.mod`; omit to build all modules. |
| `--output` | `-o` | *(project name)* | Final executable path/name. Relative names are written under `build/`. |
| `--target` | `-T` | `rv64imafd` | Target architecture. Run `obx build --help` to see all registered targets. Injects platform directives automatically. |
| `--define` | `-d` | | Compile-time directive: `NAME` (bool true) or `NAME=VALUE`. Repeatable. Applied after platform directives, so can override them. |
| `--asm` | `-S` | `false` | Print generated assembly to stdout in addition to writing it to `build/`. |
| `--keep-asm` | | `false` | Keep generated `.s` files after linking. |
| `--keep-obj` | | `false` | Keep generated `.o` files after linking. |
| `--optlevel` | `-O` | `2` | Optimisation level `0`–`3` (see [Optimisation Passes](#optimisation-passes)). |
| `--passes` | `-P` | | Comma-separated passes to enable, overriding `-O`. |
| `--disable-passes` | `-D` | | Comma-separated passes to disable from the selected level. |
| `--verbose` | `-V` | `false` | Print per-pass IR diffs and optimisation details. |

Assembly is written to `build/<ModuleName>.s` and object files to `build/<ModuleName>.o`.
The executable defaults to the project name from `obx.mod` (sanitized per platform) when
`--output` is omitted.

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
| `GET` | `/` | Browser-based Oberon+ editor, diagnostic viewer, and CFG visualiser. |
| `POST` | `/api/check` | JSON API: `{"source":"…","filename":"…"}` → diagnostics array. |
| `POST` | `/api/cfg` | JSON API: `{"source":"…","filename":"…"}` → per-function Graphviz DOT strings. |
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

Linker flags for all required native libraries are written to `build/link.flags`
automatically.  `obx build` also invokes the linker directly, but if you want to relink
manually, a typical RISC-V invocation is:

```shell
riscv64-linux-gnu-gcc build/Main.o $(cat build/link.flags) -o build/myapp
```

See **[stdlib/README.md](stdlib/README.md)** for the full API reference.

---

### ARM64 macOS smoke test

To validate the build pipeline end to end on Apple Silicon, use the small
`examples/smoke/Main.obx` program and check that the produced executable is a
Mach-O arm64 binary.

```shell
# Run the helper script from the repo root
bash scripts/arm64_macos_smoke.sh

# Optional: also verify the aarch64-apple-darwin alias target
bash scripts/arm64_macos_smoke.sh --alias
```

The script performs:

1. `go run ./cmd/obx.go build --target arm64-apple-macos -r examples/smoke -e Main`
2. `file build/obx-arm64-smoke`
3. `otool -hv build/obx-arm64-smoke`
4. execution of the produced binary

If you want a slightly richer smoke test that also exercises stdlib linking,
point it at the loop example instead:

```shell
bash scripts/arm64_macos_smoke.sh --source-root examples/loop --entry LoopTest --output loop-smoke
```

## Examples

### Main Smoke — `examples/smoke/Main.obx`

This small program computes Fibonacci iteratively and prints the result through
the stdlib `IO` module.

```oberon
module Main

IMPORT IO
  var res: integer

  PROCEDURE fib(n: INTEGER): INTEGER;
  VAR
    a, b, temp, count: INTEGER;
  BEGIN
    IF n <= 0 THEN
      RETURN 0
    ELSIF n = 1 THEN
      RETURN 1
    ELSE
      a := 0;
      b := 1;
      count := 2;
      WHILE count <= n DO
        temp := a + b;
        a := b;
        b := temp;
        INC(count)
      END;
      RETURN b
    END
  END fib;

begin
  res := fib(15);
  IO.Write("fib(15) = ");
  IO.WriteInt(res);
  IO.WriteLn("")
end Main
```

```shell
$ obx build -r examples/smoke -e Main
Building 3 module(s)  (entry: Main)
  posix.Stdio                     stdlib/posix/Stdio.obx
  IO                              stdlib/IO.obx
  Main                            examples/smoke/Main.obx
```

```shell
$ ./build/obx
fib(15) = 610
```

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
│     Scanner tokenises each file; recursive-descent parser builds │
│     AST units with source positions.                             │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  3. Semantic analysis  (src/sema)                                │
│     Name resolution, type checking, and flow checks.             │
│     Diagnostics are emitted via src/support/diag.                │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  4. Desugar  (src/ir/desugar)                                    │
│     Lower high-level language sugar into a simpler typed IR.     │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  5. Lower to minir  (src/ir/minir)                               │
│     Build module/function IR with explicit blocks and SSA forms. │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  6. Merge precompiled stdlib + dedup externs  (cmd/cli helpers)  │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  7. minir optimisation passes  (src/ir/minir/opt)                │
│     PassManager runs configured passes from -O / --passes.       │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  8. minir verification  (src/ir/minir)                           │
└─────────────────────────┬────────────────────────────────────────┘
                          │
                          ▼
┌──────────────────────────────────────────────────────────────────┐
│  9. Backend pipeline  (src/backend)                              │
│     call-lowering → switch-lowering → isel → legalization →      │
│     scheduling → regalloc → prologue/epilogue → phi-removal      │
│     → assemble → link                                             │
└──────────────────────────────────────────────────────────────────┘
```

---

## Optimisation Passes

Passes are selected by `-O` level or overridden with `--passes` / `--disable-passes`.

| Level | Passes enabled |
|---|---|
| `-O0` | *(none)* |
| `-O1` | `mem2reg`, `loadfwd`, `constfold`, `cleancfg` |
| `-O2` *(default)* | `mem2reg`, `loadfwd`, `constfold`, `cleancfg`, `simplify` *(fixed-point)* |
| `-O3` | `mem2reg`, `loadfwd`, `constfold`, `cleancfg`, `simplify`, `strength` *(fixed-point)* |

| Pass | Description |
|---|---|
| `mem2reg` | **Promote allocas to SSA** — converts non-escaping stack allocas into SSA values/phis. |
| `loadfwd` | **Load forwarding** — forwards recently stored values through matching loads. |
| `constfold` | **Constant folding** — evaluates operations with compile-time constants. |
| `cleancfg` | **CFG cleanup** — removes/merges trivial or unreachable control-flow structure. |
| `simplify` | **Algebraic simplification** — identity/annihilator simplifications. |
| `strength` | **Strength reduction** — replaces expensive ops with cheaper equivalents when legal. |

**Custom pass selection examples**

```shell
# Only constant folding
$ obx build -O0 --passes constfold

# Full O3 but skip strength reduction
$ obx build -O3 --disable-passes strength

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
├── syntax/             Frontend lexer/parser/AST/directive handling
│   ├── token/
│   ├── scan/
│   ├── parser/
│   ├── ast/
│   └── directive/
│
├── sema/               Semantic analysis and type checking
│   └── types/
│
├── ir/
│   ├── desugar/        AST desugaring
│   └── minir/          Core IR, verifier, emitter, lowering, and minir passes
│       └── opt/        PassManager + passes (mem2reg/loadfwd/constfold/...)
│
├── backend/            Target-lowering and machine pipeline
│   ├── lower/          Backend MIR lowering from minir
│   ├── mir/            Backend MIR model
│   ├── select/         Instruction selection (.td descriptor-driven)
│   ├── legalize/       Target-specific legalization
│   ├── regalloc/       Register allocation and rewrite
│   ├── target/         Target descriptors and emitters (rv64/arm64)
│   ├── emit/           Assemble/link integration
│   └── stages/         Stage registration and pipeline orchestration
│
├── project/            Module graph + manifest loading/resolution
└── support/            Compiler context, diagnostics, source mapping, cache
```
