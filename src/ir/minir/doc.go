package minir

// Package minir implements a compact minimal SSA-like three-address
// (3AC) intermediate representation used by the obx compiler for
// testing and code-generation experiments. The IR is intentionally small
// and self-contained so it is easy to construct programmatically in
// tests and exercises while still being expressive enough to represent
// common control-flow and data-flow patterns (phis, branches, calls,
// memory operations, global variables, etc.).
//
// # Symbol table management (LLVM-style)
//
// The IR has two scoped symbol tables mirroring LLVM's design:
//
//   - Module.SymTab (SymbolTable) — module-scope.  Maps global names to their
//     address Values.  GlobalVar entries are stored as *GlobalRef; GlobalConst
//     entries are stored as the immediate *Constant value (so ConstantRef
//     resolution inside functions returns the value without a load).
//
//   - Function.SymTab (SymbolTable) — function-scope.  Reserved for future use
//     (e.g. SSA rename passes); the Lowerer currently uses the private varEnv /
//     constEnv maps for local resolution.
//
// SymbolTable provides Define(name, v), Lookup(name) and Names() (sorted).
//
// # Global symbols
//
// Module-scope declarations from the source language are considered global.
// Three kinds are represented:
//
//   - GlobalVar — a mutable, addressable module-scope variable.
//     "@name = [linkage] global <type> [init]".
//     Its address is represented by a *GlobalRef value returned by Ref().
//
//   - GlobalConst — a read-only module-scope constant.
//     "@name = [linkage] constant <type> <init>".
//     Its *Constant initializer is stored directly in Module.SymTab so that
//     ConstantRef expressions resolve to an immediate without emitting a load.
//
//   - ExternalFunc — a declaration-only (no body) function imported from
//     another module or via FFI.  "declare @name(...) -> T".
//
// # Linkage
//
// Three linkage levels (Linkage) mirror LLVM:
//
//   - ExternalLinkage — symbol exported from the module (IsExport=true in HIR).
//   - InternalLinkage — symbol local to the module but named in the object file.
//   - PrivateLinkage  — compiler-synthesised symbol; linker strips the name.
//
// # GlobalRef
//
// GlobalRef is a Value representing a module-scope address (equivalent to
// "@name" in LLVM textual IR).  It satisfies:
//   - IsMem() == true, so it can be used directly as LoadInst.Addr /
//     StoreInst.Addr without a materialisation instruction.
//   - Type() returns Ptr(elemType) — the pointer type to the global's element.
//
// # Type system
//
// Type: the type system includes primitive types (i1, i32, i64, f32, f64),
// pointer types, function types, aggregate record types (RecordType with
// RecordField slices) and fixed-length array types (ArrayType).  Primitive
// types are canonicalised singletons (e.g. I32()).  Use
// NewRecordType/NewArrayType/NormalizeType/Ptr to obtain canonical instances.
// RecordType.FieldIndex(name) looks up a field by name; GEPInst.ElemType
// carries a RecordType or ArrayType to identify the aggregate being indexed.
//
// # Values
//
// Value: either a *Temp (SSA local) or a *Constant.
//   - Temp: defined by an instruction; has a numeric ID and optional NameStr.
//     IsAddr=true marks address-like temps (alloca results).
//   - Constant: immediate, immutable value with an optional NameStr.
//   - GlobalRef: module-scope address value; implements Value with IsMem()==true.
//
// # Instructions
//
// Instr implementations include BinaryInst, ICmpInst/FCmpInst, PhiInst,
// LoadInst, StoreInst, AllocaInst, GEPInst, CallInst, ReturnInst and
// terminators (JumpInst, CondBrInst, SwitchInst, HaltInst).
//
// LoadInst.Addr and StoreInst.Addr are typed as Value (not *Temp) so that a
// *GlobalRef can be used directly as an address operand (LLVM-style), avoiding
// a materialization instruction for each global access.
//
// # Block and Function
//
// Block: basic block with ordered Instrs, a Terminator in Term, and
// predecessor/successor maps with deterministic ordering helpers
// (PredOrder/SuccOrder) for stable iteration.
//
// Function: collection of blocks, entry/exit markers, signature, and a
// function-local SymbolTable.
//
// # Module and Program (top-level containers)
//
// Module is the IR container for a single translation unit (analogous to
// LLVM's llvm::Module):
//   - Globals   — mutable module-scope variables (*GlobalVar)
//   - Constants — read-only module-scope constants (*GlobalConst)
//   - Externals — declaration-only imported functions (*ExternalFunc)
//   - Functions — function definitions with bodies (*Function)
//   - SymTab    — module-scope symbol table
//
// Program is a collection of Modules, one per source module, mirroring the
// desugar.Program → []*desugar.Module relationship at the minir level.
//
// Lower(prog *desugar.Program) *Program lowers a full HIR program into a
// *Program.  Each desugar.Module produces exactly one minir.Module.  Within
// each module, lowering runs in two passes: (1) module-scope declarations,
// (2) function bodies.
//
// # Construction helpers
//
// NewTemp(name, ty) and NewAnonTemp(ty) create Temps with unique IDs and
// canonicalised types (via NormalizeType).  Tests may reset the package-level
// tempIDCounter to make generated IDs deterministic.
// NewConst(name, val, ty) constructs Constant values.
//
// # Verification
//
// VerifyIR(fn) checks structural invariants of a single function.
// VerifyModule(m) checks a whole module: global types/inits, ExternalFunc
// signatures, per-function VerifyIR, and GlobalRef resolution in SymTab.
// VerifyProgram(prog) calls VerifyModule on every module and aggregates results.
//
// # Formatting
//
// FormatFunction(fn) — human-readable function text.
// FormatModule(m)    — full module text (globals, externals, then functions).
// FormatProgram(p)   — all modules separated by a divider line,
//
//	analogous to a multi-module LLVM .ll dump.
//
// PrintFunction / PrintModule / PrintProgram write to an io.Writer.
//
// # Determinism note
//
// For tests that assert exact textual output, block successor/predecessor
// order is preserved when callers use AddSucc/AddPred.  Temp IDs are
// assigned from a package-level counter; tests reset `tempIDCounter = 0`
// at the start for stable numbering.

// End of package documentation.

// Package minir implements a compact minimal SSA-like three-address
// (3AC) intermediate representation used by the obx compiler for
// testing and code-generation experiments. The IR is intentionally small
// and self-contained so it is easy to construct programmatically in
// tests and exercises while still being expressive enough to represent
// common control-flow and data-flow patterns (phis, branches, calls,
// memory operations, etc.).
//
// Main concepts
// - Type: the type system includes primitive types (i1, i32, i64, f32,
//   f64), pointer types, function types, aggregate record types
//   (RecordType with RecordField slices) and fixed-length array types
//   (ArrayType). Primitive types are canonicalized singletons (e.g.
//   I32()). Use NewRecordType/NewArrayType/NormalizeType/Ptr to obtain
//   canonical instances. RecordType.FieldIndex(name) looks up a field
//   by name; GEPInst.ElemType carries a RecordType or ArrayType to
//   identify the aggregate being indexed.
// - Value: a Value is either a *Temp (an SSA local) or a *Constant.
// - Temp: represents a value defined by an instruction. Temps have a
//   numeric ID and an optional NameStr for readable printing. Temps can
//   also be address-like (IsAddr=true) for memory ops.
// - Constant: immediate, immutable values with an optional NameStr for
//   printing (e.g. numeric literal text).
// - Instr: instructions implement the Instr interface. Common
//   instruction kinds include BinaryInst, ICmpInst/FCmpInst, PhiInst,
//   LoadInst, StoreInst, AllocaInst, GEPInst, CallInst, ReturnInst and
//   terminators (JumpInst, CondBrInst, SwitchInst). Terminators must
//   appear as the last instruction of a block and are recorded also in
//   the Block.Term field.
// - Block: basic block with ordered Instrs, a Terminator in Term, and
//   predecessor/successor maps with deterministic ordering helpers
//   (PredOrder/SuccOrder) for stable iteration.
// - Function: collection of blocks, entry/exit markers and signature.
//
// Construction helpers
// - NewTemp(name, ty) and NewAnonTemp(ty) create Temps with unique
//   IDs and canonicalized types (via NormalizeType). Tests may reset
//   the package-level tempIDCounter to make generated IDs deterministic.
// - NewConst(name, val, ty) constructs Constant values and canonicalizes
//   their types.
//
// Verification and formatting
// - VerifyIR(fn) performs structural checks on functions (terminators
//   presence and placement, phi ordering, CFG consistency, type
//   checking of operands/defs, etc.) and returns a slice of
//   VerifyError describing any problems. The verifier is used by unit
//   tests to assert IR correctness before code generation.
// - FormatFunction(fn) provides a human-readable textual representation
//   of a function (used in the pretty tests).
//
// Intended use
// The minir package is primarily targeted at unit tests and small
// transformations inside the compiler. Tests in this package construct
// IR by hand (or via helpers), run VerifyIR to sanity-check invariants,
// and then exercise formatters or optimizations.
//
// Note about determinism
// For tests that assert exact textual output, deterministic iteration
// and numbering are important. Block successor/predecessor order is
// preserved when callers use AddSucc/AddPred (these record ordering in
// SuccOrder/PredOrder). Temp IDs are assigned from a package-level
// counter used by NewTemp; tests currently reset `tempIDCounter = 0`
// at the start to ensure stable numbering across runs. If desired, a
// small exported helper ResetTempIDs() can be added to encapsulate this
// behavior.

// End of package documentation.
