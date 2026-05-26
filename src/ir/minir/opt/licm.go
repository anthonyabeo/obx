package miniropt

// LICM (Loop-Invariant Code Motion) Pass Overview
//
// This file documents the LICM (Loop-Invariant Code Motion) pass for the Obx compiler.
// LICM is an important optimization that moves invariant computations outside loops.
//
// ## When to Apply LICM
//
// GEP instructions inside loops often compute addresses from compile-time constants:
//
//   for i in 0..n {
//     addr = base + i * stride    // stride is compile-time known
//     store(value, addr)          // store to array element
//   }
//
// LICM can split this into:
//   stride_computed = base + constant_offset  // moved outside loop
//   for i in 0..n {
//     addr = stride_computed + i * stride_per_index
//     store(value, addr)
//   }
//
// ## Implementation Strategy
//
// A production LICM pass requires:
//
// 1. **Loop Detection**
//    - Identify natural loops via back edges in CFG
//    - Build loop hierarchy (nested loops)
//    - Compute loop-entry blocks and exit blocks
//
// 2. **Invariant Analysis**
//    - Determine which instructions depend only on loop-invariant values
//    - Mark invariant instructions recursively:
//      - Phi-nodes at loop entry: invariant if all predecessors from outside loop
//      - Regular instructions: invariant if all operands are invariant
//
// 3. **Safety Conditions**
//    - Instruction has no side effects (load/store/call not safe)
//    - No path from instruction to loop exit goes through exception handler
//    - Safe to speculate (won't execute more times than original)
//
// 4. **Motion**
//    - Move invariant instructions to pre-header (block immediately before loop entry)
//    - Update phi-nodes to reflect new sources
//    - Maintain SSA form
//
// ## GEP-Specific Optimizations
//
// For GEP instructions, LICM combined with constant folding gives:
//
//   const i64 stride = 8;
//   const *base;
//   for _ in 0..1000 {
//     addr = gep(base, idx, stride)   // stride is constant
//     load(addr)
//   }
//
// LICM moves the constant stride computation outside, enabling:
//  - Better instruction selection (stride folded as immediate)
//  - Reduced register pressure in loop body
//  - Fewer memory access operations
//
// ## Future Implementation Checklist
//
// - [ ] Implement LoopDetector to find natural loops
// - [ ] Implement LoopNormalizer (create pre-headers)
// - [ ] Implement DominatorAnalysis (prerequisite)
// - [ ] Implement LoopInvariantAnalyzer
// - [ ] Implement InvariancyChecker (safety conditions)
// - [ ] Implement InstructionMotion (actually move code)
// - [ ] Integrate with PassManager
// - [ ] Add comprehensive test suite
//
// ## References
//
// Classic algorithm: Muchnick "Advanced Compiler Design and Implementation", Chapter 13
// LLVM implementation: llvm/lib/Transforms/Scalar/LoopIdiomRecognize.cpp
//
