# Obx Standard Library

The Obx stdlib ships eight idiomatic Oberon+ modules backed by platform FFI
bindings for both **POSIX** (Linux, macOS) and **Windows**.

---

## Directory Layout

```
stdlib/
  posix/          Raw POSIX libc / libm DEFINITION bindings (auto-discovered)
    Stdio.obx       stdio.h  — FILE*, printf, fopen, fread …
    Stdlib.obx      stdlib.h — malloc, free, exit, getenv …
    StringH.obx     string.h — strlen, strcmp, memcpy …
    LibM.obx        math.h   — sin, cos, sqrt, pow … (links -lm)
    TimeH.obx       time.h   — time, clock, localtime, strftime, struct tm
    Unistd.obx      unistd.h — getpid, getcwd, open, read, write …
  win32/          Raw Win32 / msvcrt / ucrtbase DEFINITION bindings
    Stdio.obx       msvcrt stdio (fopen, fprintf, _snprintf …)
    Stdlib.obx      msvcrt stdlib (malloc, free, _exit …)
    StringH.obx     msvcrt string (strlen, strcmp, memcpy …)
    LibM.obx        ucrtbase math (sin, cos, sqrt …)
    TimeH.obx       msvcrt time (time, localtime, strftime …)
    WinAPI.obx      kernel32 (CreateFileA, ReadFile, WriteFile,
                              ExitProcess, GetEnvironmentVariableA …)
  IO.obx          Console / stream I/O
  Files.obx       File-system I/O
  OS.obx          Process control & environment
  Strings.obx     String utilities
  Math.obx        Mathematical functions & constants
  Mem.obx         Heap memory management
  Time.obx        Date / time utilities
  Fmt.obx         Formatted string building (snprintf-backed)
```

The `stdlib/` directory is **automatically prepended** to the source-root list
on every `obx build` / `obx check` invocation — no `obx.mod` change is needed.
Both `posix/` and `win32/` are always discovered; the correct layer is selected
at parse time via `<* IF WINDOWS THEN *>` / `<* ELSE *>` directives that the
compiler injects automatically from `--target`.

---

## Platform directive injection

| Target flag | Directives set |
|---|---|
| `rv64imafd` *(default)* | `POSIX=true`, `LINUX=true` |
| `arm64-apple-macos` / `aarch64-apple-darwin` | `POSIX=true`, `DARWIN=true` |
| `x86_64-pc-windows` | `WINDOWS=true` |

Override any directive manually with `--define`, e.g.:

```shell
obx build --define WINDOWS=true   # force Windows stdlib layer on any target
```

---

## Module Reference

### `IO` — Console I/O

```oberon
import IO

IO.Write("hello ")           // no newline
IO.WriteLn("world!")         // with newline
IO.WriteInt(42)
IO.WriteReal(3.14)
IO.WriteBool(true)

var line: array 256 of char
    n: integer
IO.ReadLn(line)
IO.ReadInt(n)
```

| Procedure | Signature | Description |
|---|---|---|
| `Write` | `(in s: ARRAY OF CHAR)` | Write string, no newline |
| `WriteLn` | `(in s: ARRAY OF CHAR)` | Write string + newline |
| `WriteChar` | `(c: CHAR)` | Write one character |
| `WriteInt` | `(n: INTEGER)` | Write decimal integer |
| `WriteLongInt` | `(n: LONGINT)` | Write decimal LONGINT |
| `WriteReal` | `(x: REAL)` | Write REAL (`%g`) |
| `WriteLongReal` | `(x: LONGREAL)` | Write LONGREAL (full precision) |
| `WriteBool` | `(b: BOOLEAN)` | Write `"true"` or `"false"` |
| `WriteToStream` | `(stream: FilePtr; in s: …)` | Write to arbitrary stream |
| `ReadLn` | `(VAR buf: ARRAY OF CHAR): BOOLEAN` | Read line from stdin |
| `ReadChar` | `(): CHAR` | Read one char from stdin |
| `ReadInt` | `(VAR n: INTEGER): BOOLEAN` | Parse integer from stdin |
| `ReadLongInt` | `(VAR n: LONGINT): BOOLEAN` | Parse LONGINT from stdin |
| `ReadReal` | `(VAR x: REAL): BOOLEAN` | Parse REAL from stdin |
| `StdOut` / `StdErr` / `StdIn` | `FilePtr` | Pre-opened stream handles |

---

### `Files` — File I/O

```oberon
import Files

var f: Files.File
    buf: array 1024 of byte
    n: integer
begin
  f := Files.Open("data.bin", Files.ModeRead)
  if Files.IsOpen(f) then
    n := Files.ReadBytes(f, buf)
    Files.Close(f)
  end
```

| Constant | Value | Meaning |
|---|---|---|
| `ModeRead` | 0 | `"r"` — existing file, read-only |
| `ModeWrite` | 1 | `"w"` — truncate or create, write-only |
| `ModeAppend` | 2 | `"a"` — append or create |
| `ModeReadWrite` | 3 | `"r+"` — existing file, read+write |
| `ModeNew` | 4 | `"w+"` — truncate or create, read+write |
| `SeekSet` / `SeekCur` / `SeekEnd` | 0/1/2 | `fseek` origin |

Key procedures: `Open`, `Close`, `IsOpen`, `Flush`, `ReadBytes`, `ReadLine`,
`ReadChar`, `Eof`, `WriteBytes`, `WriteStr`, `WriteStrLn`, `WriteChar`,
`Seek`, `Tell`, `Rewind`, `Delete`, `Rename`, `Exists`.

---

### `OS` — Operating-system utilities

```oberon
import OS

var home: array 256 of char
begin
  if OS.Env("HOME", home) then IO.WriteLn(home) end
  OS.Exit(0)
```

| Procedure | Signature | Description |
|---|---|---|
| `Exit` | `(code: INTEGER)` | Terminate process |
| `Abort` | `()` | Abnormal termination |
| `Env` | `(in key: …; VAR buf: …): BOOLEAN` | Read environment variable |
| `GetCwd` | `(VAR buf: …): BOOLEAN` | Current working directory |
| `SetCwd` | `(in path: …): BOOLEAN` | Change working directory |
| `Exec` | `(in cmd: …): INTEGER` | Run shell command |
| `GetPid` | `(): INTEGER` | Current process ID |
| `Sleep` | `(seconds: INTEGER)` | Sleep N seconds |

---

### `Strings` — String utilities

```oberon
import Strings

var s: array 64 of char
begin
  s := "  Hello, World!  "
  Strings.Trim(s)
  assert(Strings.Equal(s, "Hello, World!"))
  assert(Strings.IndexOf(s, "World") = 7)
```

Key procedures: `Len`, `Copy`, `Append`, `Compare`, `Equal`, `IndexOf`,
`Contains`, `StartsWith`, `EndsWith`, `IndexOfChar`,
`ToUpper`, `ToLower`, `Trim`, `TrimLeft`, `TrimRight`.

---

### `Math` — Mathematical functions

```oberon
import Math

var x: longreal
begin
  x := Math.Sqrt(2.0D0)
  assert(Math.Abs(x * x - 2.0D0) < 1.0D-14)
  x := Math.Sin(Math.Pi / 2.0D0)   // 1.0
```

Constants: `Pi`, `E`, `Sqrt2`, `Ln2`, `Ln10`.

Functions (LONGREAL): `Sin`, `Cos`, `Tan`, `Asin`, `Acos`, `Atan`, `Atan2`,
`Sinh`, `Cosh`, `Tanh`, `Exp`, `Exp2`, `Log`, `Log2`, `Log10`,
`Pow`, `Sqrt`, `Cbrt`, `Hypot`, `Floor`, `Ceil`, `Round`, `Trunc`,
`Abs`, `Mod`, `IsNaN`, `IsInf`, `IsFinite`.

REAL variants (suffix `F`): `SinF`, `CosF`, `SqrtF`, `PowF`, `AbsF`,
`FloorF`, `CeilF`.

Integer helpers: `Min`, `Max`, `Clamp`.

---

### `Mem` — Heap memory management

```oberon
import Mem

var p: cpointer to void
begin
  p := Mem.Alloc(4096)
  Mem.Zero(p, 4096)
  Mem.Free(p)
```

| Procedure | Description |
|---|---|
| `Alloc(size)` | `malloc(size)` — uninitialized |
| `AllocZero(size)` | `calloc(1, size)` — zeroed |
| `Realloc(ptr, size)` | `realloc` |
| `Free(ptr)` | `free` |
| `Copy(dst, src, n)` | `memcpy` |
| `Move(dst, src, n)` | `memmove` (overlapping-safe) |
| `Fill(ptr, val, n)` | `memset` |
| `Zero(ptr, n)` | `memset(ptr, 0, n)` |
| `Compare(a, b, n)` | `memcmp` |
| `Equal(a, b, n)` | `memcmp = 0` |

---

### `Time` — Date and time

```oberon
import Time

var dt: Time.DateTime
    buf: array 32 of char
    t0, t1: longint
begin
  Time.Now(dt)
  Time.Format(buf, dt, "%Y-%m-%d %H:%M:%S")
  IO.WriteLn(buf)

  t0 := Time.Clock()
  // ... work ...
  t1 := Time.Clock()
  // elapsed seconds:
  x := Time.Elapsed(t0, t1)
```

| Item | Description |
|---|---|
| `DateTime` record | `year`, `month`, `day`, `hour`, `min`, `sec`, `wday`, `yday` |
| `Now(VAR dt)` | Fill dt with current local time |
| `Epoch(): LONGINT` | Seconds since Unix epoch |
| `Clock(): LONGINT` | Raw CPU clock ticks |
| `ClocksPerSec` | `1 000 000` (POSIX) / `1 000` (Windows) |
| `Elapsed(start, stop)` | `(stop-start) / ClocksPerSec` in seconds |
| `Format(VAR buf; dt; fmt)` | `strftime`-compatible formatting |

---

### `Fmt` — Formatted string building

`Fmt` calls `snprintf` directly and has **no dependency on `IO`**.

```oberon
import Fmt, IO

var buf: array 128 of char
begin
  Fmt.Int(buf, 42)                // "42"
  Fmt.LongReal(buf, 3.14159, 2)  // "3.14"
  Fmt.SprintI(buf, "%05d", 7)    // "00007"
  IO.WriteLn(buf)

  var n: integer
  Fmt.ScanInt("   -99", n)       // n = -99
```

Formatters: `Bool`, `Int`, `LongInt`, `IntHex`, `IntHexUpper`, `IntPadded`,
`Real`, `LongReal`, `RealSci`, `Str`, `Char`.

Composite: `SprintI`, `SprintS`, `SprintR`, `SprintII`, `SprintSI`.

Scanners: `ScanInt`, `ScanLongInt`, `ScanReal`, `ScanStr`.

---

## Adding stdlib to an existing project

No `obx.mod` change is required — the compiler auto-discovers the stdlib.
If you need to override the location:

```json
{
  "name":   "myproject",
  "roots":  ["src"],
  "entry":  "Main",
  "stdlib": "/path/to/custom/stdlib"
}
```

Or set the environment variable:

```shell
export OBX_STDLIB=/path/to/custom/stdlib
obx build
```

---

## Linking

The linker flags for stdlib dependencies are written to `out/link.flags`
automatically by `obx build`.  A typical POSIX link invocation:

```shell
riscv64-linux-gnu-gcc out/Main.s $(cat out/link.flags) -o myapp
```

Expected flags for each module:

| Module | Libs linked |
|---|---|
| `IO`, `Files`, `OS`, `Strings`, `Mem`, `Time`, `Fmt` | *(libc, linked by default)* |
| `Math` | `-lm` |
| Win32 modules | `-lmsvcrt`, `-lucrtbase`, `-lkernel32` |

