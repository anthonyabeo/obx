package target

import (
	"fmt"
	"sort"
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
)

// Copy is one move in a canonicalized parallel-copy set.
type Copy struct {
	Dst *mir.Register
	Src mir.Operand
}

// ParallelCopy captures the moves that must happen simultaneously on one edge.
type ParallelCopy struct {
	Copies []Copy
}

// PhiEdgePlan groups the parallel copies that must execute on one incoming edge.
type PhiEdgePlan struct {
	PredLabel string
	Copies    ParallelCopy
}

// PhiPlan is the canonical lowering shape for a block's phi nodes.
type PhiPlan struct {
	JoinLabel string
	Edges     []PhiEdgePlan
}

// SwitchCase is one integer switch arm.
type SwitchCase struct {
	Value int64
	Label string
}

// JumpTable is the canonical jump-table form chosen for dense integer switches.
type JumpTable struct {
	Low, High int64
	Default   string
	Entries   []string
}

// SwitchPlan is the canonical lowering shape for a switch terminator.
type SwitchPlan struct {
	Key          mir.Operand
	Default      string
	Cases        []SwitchCase
	JumpTable    *JumpTable
	CompareChain []SwitchCase
}

// ArgLocation describes where one argument is placed by the calling convention.
type ArgLocation struct {
	Index       int
	Value       mir.Operand
	InRegister  bool
	Register    string
	StackOffset int
}

// ResultLocation describes where a return value is placed.
type ResultLocation struct {
	InRegister  bool
	Register    string
	StackOffset int
}

// CallPlan is the canonical lowering shape for a call terminator.
type CallPlan struct {
	Callee     mir.Operand
	Args       []ArgLocation
	Result     *ResultLocation
	StackBytes int
}

// BuildPhiPlan groups all phi instructions by predecessor and returns the
// predecessor-edge parallel-copy plan.
func BuildPhiPlan(joinLabel string, phis []*mir.PhiInstr) (*PhiPlan, error) {
	if joinLabel == "" {
		return nil, fmt.Errorf("phi lowering requires a join label")
	}
	plan := &PhiPlan{JoinLabel: joinLabel}
	if len(phis) == 0 {
		return plan, nil
	}

	edgeCopies := make(map[string][]Copy)
	for _, phi := range phis {
		if phi == nil {
			return nil, fmt.Errorf("phi lowering encountered a nil phi")
		}
		if phi.Dst == nil {
			return nil, fmt.Errorf("phi lowering requires a destination register")
		}
		if !SupportsIntegerScalar(phi.Dst.Type()) {
			return nil, fmt.Errorf("phi %s has unsupported destination type", phi.Dst)
		}
		for _, arm := range phi.Arms {
			if arm.BlockLabel == "" {
				return nil, fmt.Errorf("phi %s is missing a predecessor label", phi.Dst)
			}
			if arm.Value == nil {
				return nil, fmt.Errorf("phi %s has a nil incoming value", phi.Dst)
			}
			if !supportsIntegerOperand(arm.Value) {
				return nil, fmt.Errorf("phi %s has unsupported incoming value %s", phi.Dst, arm.Value)
			}
			edgeCopies[arm.BlockLabel] = append(edgeCopies[arm.BlockLabel], Copy{Dst: phi.Dst, Src: arm.Value})
		}
	}

	labels := make([]string, 0, len(edgeCopies))
	for label := range edgeCopies {
		labels = append(labels, label)
	}
	sort.Strings(labels)
	for _, label := range labels {
		plan.Edges = append(plan.Edges, PhiEdgePlan{PredLabel: label, Copies: ParallelCopy{Copies: edgeCopies[label]}})
	}
	return plan, nil
}

// LinearizeParallelCopy resolves a simultaneous copy set into an ordered move
// list, inserting temporary registers when cycles are present.
//
// Safety criterion: copy C is safe to emit when C.Dst is NOT a pending source,
// i.e., no other pending copy still needs to READ C.Dst.  Emitting C first would
// overwrite the value before the other copy reads it.
func LinearizeParallelCopy(pc ParallelCopy) []Copy {
	pending := append([]Copy(nil), pc.Copies...)
	out := make([]Copy, 0, len(pending))
	tmpSeq := 0

	for len(pending) > 0 {
		progress := false
		srcNames := pendingSrcNames(pending)

		for i := 0; i < len(pending); i++ {
			c := pending[i]
			if isNoOpCopy(c) {
				pending = append(pending[:i], pending[i+1:]...)
				progress = true
				break
			}
			if !dstIsPendingSource(c.Dst, srcNames) {
				out = append(out, c)
				pending = append(pending[:i], pending[i+1:]...)
				progress = true
				break
			}
		}
		if progress {
			continue
		}

		// All remaining copies form cycles; break the first one with a temp.
		cycle := pending[0]
		tmp := newParallelCopyTemp(tmpSeq, cycle)
		tmpSeq++
		out = append(out, Copy{Dst: tmp, Src: cycle.Src})
		cycleSrcReg, _ := cycle.Src.(*mir.Register)
		for i := range pending {
			if registerOperand, ok := pending[i].Src.(*mir.Register); ok && sameRegister(registerOperand, cycleSrcReg) {
				pending[i].Src = tmp
			}
		}
	}

	return out
}

func newParallelCopyTemp(seq int, cycle Copy) *mir.Register {
	name := fmt.Sprintf("__pc_tmp%d", seq)
	kind := mir.VirtualReg

	seed := cycle.Dst
	if seed == nil {
		if srcReg, ok := cycle.Src.(*mir.Register); ok {
			seed = srcReg
		}
	}

	if seed != nil && seed.Kind == mir.PhysicalReg {
		kind = mir.PhysicalReg
		name = "x15"
		if isFloatRegName(seed.Name) {
			name = "d15"
		}
	}

	return &mir.Register{Name: name, Kind: kind, Ty: copyType(cycle)}
}

func isFloatRegName(name string) bool {
	if name == "" {
		return false
	}
	return strings.HasPrefix(name, "d") || strings.HasPrefix(name, "s")
}

// BuildSwitchPlan validates the switch and chooses jump-table lowering when the
// integer range is dense enough.
func BuildSwitchPlan(sw *mir.SwitchInstr, abi ABI) (*SwitchPlan, error) {
	if sw == nil {
		return nil, fmt.Errorf("switch lowering requires a switch instruction")
	}
	if sw.Value == nil {
		return nil, fmt.Errorf("switch lowering requires a key register")
	}
	if !supportsIntegerOperand(sw.Value) {
		return nil, fmt.Errorf("switch key %s has unsupported type", sw.Value)
	}

	plan := &SwitchPlan{Key: sw.Value, Default: sw.Default}
	for _, arm := range sw.Arms {
		value, ok, err := operandToInt64(arm.Value)
		if err != nil {
			return nil, err
		}
		if !ok {
			return nil, fmt.Errorf("switch arm %q must be an integer immediate", arm.Label)
		}
		plan.Cases = append(plan.Cases, SwitchCase{Value: value, Label: arm.Label})
	}

	sort.Slice(plan.Cases, func(i, j int) bool { return plan.Cases[i].Value < plan.Cases[j].Value })
	for i := 1; i < len(plan.Cases); i++ {
		if plan.Cases[i].Value == plan.Cases[i-1].Value {
			return nil, fmt.Errorf("switch has duplicate case value %d", plan.Cases[i].Value)
		}
	}
	if len(plan.Cases) == 0 {
		return plan, nil
	}

	low := plan.Cases[0].Value
	high := plan.Cases[len(plan.Cases)-1].Value
	span := high - low + 1
	if span <= 0 {
		return nil, fmt.Errorf("invalid switch span")
	}
	density := float64(len(plan.Cases)) / float64(span)
	if density >= abi.JumpTableDensity() && len(plan.Cases) >= 2 {
		entries := make([]string, int(span))
		for i := range entries {
			entries[i] = sw.Default
		}
		for _, c := range plan.Cases {
			entries[int(c.Value-low)] = c.Label
		}
		plan.JumpTable = &JumpTable{Low: low, High: high, Default: sw.Default, Entries: entries}
		return plan, nil
	}

	plan.CompareChain = append(plan.CompareChain, plan.Cases...)
	return plan, nil
}

// BuildCallPlan assigns integer arguments/results to the target ABI.
func BuildCallPlan(call *mir.CallInstr, abi ABI) (*CallPlan, error) {
	if call == nil {
		return nil, fmt.Errorf("call lowering requires a call instruction")
	}
	plan := &CallPlan{Callee: call.Callee}

	intArgIdx, floatArgIdx, stackArgIdx := 0, 0, 0
	for i, arg := range call.Args {
		if !supportsCallOperand(arg) {
			tyStr := "<nil>"
			if r, ok := arg.(*mir.Register); ok && r.Type() != nil {
				tyStr = r.Type().String()
			} else if imm, ok := arg.(*mir.Immediate); ok && imm.Type() != nil {
				tyStr = imm.Type().String()
			} else if sym, ok := arg.(*mir.Symbol); ok && sym.Type() != nil {
				tyStr = sym.Type().String()
			}
			return nil, fmt.Errorf("call argument %d (%s) has unsupported type %s", i, arg, tyStr)
		}
		loc := ArgLocation{Index: i, Value: arg}
		argIsFloat := isFloatOperand(arg)
		if argIsFloat {
			if reg, ok := abi.FloatArgReg(floatArgIdx); ok {
				loc.InRegister = true
				loc.Register = reg
				floatArgIdx++
			} else {
				loc.StackOffset = stackArgIdx * abi.WordSize
				stackArgIdx++
				plan.StackBytes = loc.StackOffset + abi.WordSize
			}
		} else {
			if reg, ok := abi.ArgReg(intArgIdx); ok {
				loc.InRegister = true
				loc.Register = reg
				intArgIdx++
			} else {
				loc.StackOffset = stackArgIdx * abi.WordSize
				stackArgIdx++
				plan.StackBytes = loc.StackOffset + abi.WordSize
			}
		}
		plan.Args = append(plan.Args, loc)
	}

	if call.Dst != nil {
		dstTy := call.Dst.Type()
		if !SupportsIntegerScalar(dstTy) && !IsFloatType(dstTy) {
			return nil, fmt.Errorf("call result %s has unsupported type", call.Dst)
		}
		res := &ResultLocation{}
		if IsFloatType(dstTy) {
			if reg, ok := abi.FloatRetReg(0); ok {
				res.InRegister = true
				res.Register = reg
			} else {
				res.StackOffset = 0
			}
		} else {
			if reg, ok := abi.RetReg(0); ok {
				res.InRegister = true
				res.Register = reg
			} else {
				res.StackOffset = 0
			}
		}
		plan.Result = res
	}

	if plan.StackBytes > 0 && abi.Align > 0 {
		plan.StackBytes = alignUp(plan.StackBytes, abi.Align)
	}
	return plan, nil
}

func isNoOpCopy(c Copy) bool {
	if c.Dst == nil {
		return true
	}
	src, ok := c.Src.(*mir.Register)
	return ok && sameRegister(c.Dst, src)
}

func copyType(c Copy) *mir.Type {
	if c.Dst != nil && c.Dst.Type() != nil {
		return c.Dst.Type()
	}
	if src, ok := c.Src.(*mir.Register); ok {
		return src.Type()
	}
	if imm, ok := c.Src.(*mir.Immediate); ok {
		return imm.Type()
	}
	return nil
}

func pendingSrcNames(pending []Copy) map[string]struct{} {
	set := make(map[string]struct{}, len(pending))
	for _, c := range pending {
		if reg, ok := c.Src.(*mir.Register); ok && reg != nil {
			set[reg.Name] = struct{}{}
		}
	}
	return set
}

func dstIsPendingSource(dst *mir.Register, srcNames map[string]struct{}) bool {
	if dst == nil {
		return false
	}
	_, ok := srcNames[dst.Name]
	return ok
}

func sameRegister(a, b *mir.Register) bool {
	if a == nil || b == nil {
		return false
	}
	return a.Name == b.Name && a.Kind == b.Kind
}

func operandToInt64(op mir.Operand) (int64, bool, error) {
	imm, ok := op.(*mir.Immediate)
	if !ok {
		return 0, false, nil
	}
	if imm == nil {
		return 0, false, fmt.Errorf("nil immediate")
	}
	switch v := imm.Value.(type) {
	case int:
		return int64(v), true, nil
	case int8:
		return int64(v), true, nil
	case int16:
		return int64(v), true, nil
	case int32:
		return int64(v), true, nil
	case int64:
		return v, true, nil
	case uint:
		return int64(v), true, nil
	case uint8:
		return int64(v), true, nil
	case uint16:
		return int64(v), true, nil
	case uint32:
		return int64(v), true, nil
	case uint64:
		if v > ^uint64(0)>>1 {
			return 0, false, fmt.Errorf("immediate %v overflows int64", v)
		}
		return int64(v), true, nil
	case string:
		parsed, err := strconv.ParseInt(v, 10, 64)
		if err != nil {
			return 0, false, fmt.Errorf("cannot parse immediate %q as integer: %w", v, err)
		}
		return parsed, true, nil
	default:
		return 0, false, nil
	}
}

func alignUp(n, align int) int {
	if align <= 0 {
		return n
	}
	r := n % align
	if r == 0 {
		return n
	}
	return n + (align - r)
}

func supportsIntegerOperand(op mir.Operand) bool {
	switch v := op.(type) {
	case *mir.Register:
		return SupportsIntegerScalar(v.Type())
	case *mir.Immediate:
		return SupportsIntegerScalar(v.Type())
	case *mir.Symbol:
		return SupportsIntegerScalar(v.Type())
	default:
		return false
	}
}

func supportsCallOperand(op mir.Operand) bool {
	if supportsIntegerOperand(op) {
		return true
	}
	switch v := op.(type) {
	case *mir.Register:
		return IsFloatType(v.Type())
	case *mir.Immediate:
		return IsFloatType(v.Type())
	case *mir.Symbol:
		return IsFloatType(v.Type())
	default:
		return false
	}
}

func isFloatOperand(op mir.Operand) bool {
	switch v := op.(type) {
	case *mir.Register:
		return IsFloatType(v.Type())
	case *mir.Immediate:
		return IsFloatType(v.Type())
	case *mir.Symbol:
		return IsFloatType(v.Type())
	default:
		return false
	}
}
