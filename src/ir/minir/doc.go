package minir

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

