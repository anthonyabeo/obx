package selector

import (
	"fmt"
	"math/bits"
	"strconv"
	"strings"
	"sync"

	"github.com/anthonyabeo/obx/src/backend/legalize"
	"github.com/anthonyabeo/obx/src/backend/mir"
)

// PredicateFunc evaluates a predicate against the current match environment.
type PredicateFunc func(env map[string]any, args []*Value) bool

// Selector is a compiled instruction selector built from a descriptor file.
// It rewrites backend MIR into machine instructions/terminators when rules
// match; otherwise it passes instructions through unchanged.
type Selector struct {
	file       *File
	abi        string
	rules      []*compiledRule
	byOp       map[string][]*compiledRule
	predicates map[string]PredicateFunc
	termPrefix []mir.Instr
	tempMu     sync.Mutex
	tempSeq    int
}

type compiledRule struct {
	name    string
	pattern *PatternExpr

	out   *Binding
	in    []*Binding
	temps []*Binding

	commutative bool
	cost        int
	conds       []*Predicate
	legalize    *LegalizeSection
	emit        *EmitSection
}

type matchNode struct {
	op   string
	dst  mir.Operand
	args []mir.Operand
}

// New compiles a parsed descriptor file into a selector.
func New(file *File) (*Selector, error) {
	s := &Selector{
		file: file,
		byOp: make(map[string][]*compiledRule),
	}
	if file == nil {
		s.predicates = defaultPredicates("")
		return s, nil
	}
	s.abi = headerString(file.Header, "ABI")
	s.predicates = defaultPredicates(s.abi)
	if err := s.compile(); err != nil {
		return nil, err
	}
	return s, nil
}

// ParseSelectorString parses and compiles a selector from source text.
func ParseSelectorString(src string) (*Selector, error) {
	file, err := ParseFile(src)
	if err != nil {
		return nil, err
	}
	return New(file)
}

// ParseSelectorFile parses and compiles a selector from a file path.
func ParseSelectorFile(path string) (*Selector, error) {
	file, err := ParseFilePath(path)
	if err != nil {
		return nil, err
	}
	return New(file)
}

func (s *Selector) compile() error {
	for _, tgt := range s.file.Targets {
		if tgt == nil {
			continue
		}

		if err := s.compileItems(tgt.Items); err != nil {
			return fmt.Errorf("target %s: %w", tgt.Name, err)
		}
	}
	return nil
}

func headerString(hdr *Header, key string) string {
	if hdr == nil {
		return ""
	}
	for _, f := range hdr.Fields {
		if f == nil || !strings.EqualFold(f.Key, key) || f.Val == nil {
			continue
		}
		return valueString(f.Val)
	}
	return ""
}

func (s *Selector) compileItems(items []BlockItem) error {
	for _, item := range items {
		switch n := item.(type) {
		case *Block:
			if err := s.compileItems(n.Items); err != nil {
				return err
			}
		case *Rule:
			rule, err := compileRule(n)
			if err != nil {
				return err
			}

			s.rules = append(s.rules, rule)
			if rule.pattern != nil {
				key := strings.ToLower(rule.pattern.Name)
				s.byOp[key] = append(s.byOp[key], rule)
			}
		}
	}
	return nil
}

func compileRule(r *Rule) (*compiledRule, error) {
	if r == nil {
		return nil, fmt.Errorf("nil rule")
	}
	cr := &compiledRule{name: r.Name, cost: 1}
	for _, sec := range r.Sections {
		switch x := sec.(type) {
		case *MatchSection:
			for _, item := range x.Items {
				switch m := item.(type) {
				case *Binding:
					switch strings.ToLower(m.Dir) {
					case "in":
						cr.in = append(cr.in, m)
					case "out":
						cr.out = m
					case "temp", "temps":
						cr.temps = append(cr.temps, m)
					}
				case *PatternDecl:
					cr.pattern = m.Pattern
				case *MatchAttr:
					if strings.EqualFold(m.Name, "commutative") {
						cr.commutative = true
					}
				}
			}
		case *PatternSection:
			cr.pattern = x.Pattern
		case *CostSection:
			if strings.TrimSpace(x.Cost) != "" {
				cost, err := strconv.Atoi(strings.TrimSpace(x.Cost))
				if err != nil {
					return nil, fmt.Errorf("rule %s: invalid cost %q: %w", r.Name, x.Cost, err)
				}
				cr.cost = cost
			}
		case *CondSection:
			cr.conds = append(cr.conds, x.Predicates...)
		case *LegalizeSection:
			cr.legalize = x
		case *EmitSection:
			cr.emit = x
		}
	}
	if cr.pattern == nil {
		return nil, fmt.Errorf("rule %s: missing pattern", r.Name)
	}
	return cr, nil
}

// SelectProgram rewrites every function in a MIR program.
func (s *Selector) SelectProgram(prog *mir.Program) (*mir.Program, error) {
	if prog == nil {
		return nil, nil
	}

	out := mir.NewProgram()
	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		nmod, err := s.SelectModule(mod)
		if err != nil {
			return nil, fmt.Errorf("module %s: %w", mod.Name, err)
		}
		out.AddModule(nmod)
	}
	return out, nil
}

// SelectModule rewrites a single MIR module.
func (s *Selector) SelectModule(mod *mir.Module) (*mir.Module, error) {
	if mod == nil {
		return nil, nil
	}
	out := mir.NewModule(mod.Name)
	out.IsEntry = mod.IsEntry
	out.DLLName = mod.DLLName
	for _, g := range mod.Globals {
		out.AddGlobal(g)
	}
	for _, e := range mod.Externals {
		out.AddExtern(e)
	}
	for _, c := range mod.Constants {
		out.AddConst(c)
	}
	for _, fn := range mod.Functions {
		nfn, err := s.SelectFunction(fn)
		if err != nil {
			return nil, fmt.Errorf("function %s: %w", fn.Name, err)
		}
		out.AddFunction(nfn)
	}
	return out, nil
}

// SelectFunction rewrites a single MIR function block-by-block.
func (s *Selector) SelectFunction(fn *mir.Function) (*mir.Function, error) {
	if fn == nil {
		return nil, nil
	}

	out := mir.NewFunction(fn.Name, fn.Result)
	out.Params = append(out.Params, fn.Params...)
	out.Locals = append(out.Locals, fn.Locals...)
	out.IsLeaf = fn.IsLeaf
	out.HasCalls = fn.HasCalls
	out.Frame = fn.Frame

	for _, b := range fn.Blocks {
		nb, err := s.SelectBlock(b)
		if err != nil {
			return nil, fmt.Errorf("block %s: %w", b.Label, err)
		}

		out.AddBlock(nb)
		if fn.Entry != nil && b.ID == fn.Entry.ID {
			out.SetEntry(nb)
		}

		if fn.Exit != nil && b.ID == fn.Exit.ID {
			out.SetExit(nb)
		}
	}

	// Rebuild CFG edges from the original block graph.
	for i, b := range fn.Blocks {
		nb := out.Blocks[i]
		if b == nil || nb == nil {
			continue
		}
		for _, succ := range b.Succs {
			if succ == nil {
				continue
			}
			msucc := out.BlockByLabel(succ.Label)
			if msucc == nil {
				continue
			}
			nb.AddSucc(msucc)
			msucc.AddPred(nb)
		}
	}

	return out, nil
}

// SelectBlock rewrites one MIR block.
func (s *Selector) SelectBlock(b *mir.Block) (*mir.Block, error) {
	if b == nil {
		return nil, nil
	}

	out := mir.NewBlock(b.ID, b.Label)
	for _, inst := range b.Instrs {
		sel, err := s.SelectInstr(inst)
		if err != nil {
			return nil, fmt.Errorf("instr %T: %w", inst, err)
		}
		out.Instrs = append(out.Instrs, sel...)
	}

	if b.Term != nil {
		term, err := s.SelectTerminator(b.Term)
		if err != nil {
			return nil, fmt.Errorf("terminator %T: %w", b.Term, err)
		}
		if len(s.termPrefix) > 0 {
			out.Instrs = append(out.Instrs, s.termPrefix...)
			s.termPrefix = nil
		}
		out.Term = term
	}

	return out, nil
}

// SelectInstr rewrites one MIR instruction. If nothing matches, the instruction
// is kept unchanged.
func (s *Selector) SelectInstr(inst mir.Instr) ([]mir.Instr, error) {
	if inst == nil {
		return nil, nil
	}

	node := classifyInstr(inst)
	if node.op == "" {
		return []mir.Instr{inst}, nil
	}

	rule, env, ok := s.bestRule(node)
	if !ok || rule == nil || rule.emit == nil || len(rule.emit.Stmts) == 0 {
		return []mir.Instr{inst}, nil
	}

	sel, _, err := s.emitSelection(rule, env, false)
	if err != nil {
		return nil, err
	}

	if len(sel) == 0 {
		return []mir.Instr{inst}, nil
	}

	return sel, nil
}

// SelectTerminator rewrites one MIR terminator. If nothing matches, the
// original terminator is kept unchanged.
func (s *Selector) SelectTerminator(term mir.Terminator) (mir.Terminator, error) {
	if term == nil {
		return nil, nil
	}
	s.termPrefix = nil

	node := classifyTerm(term)
	if node.op == "" {
		return term, nil
	}

	rule, env, ok := s.bestRule(node)
	if !ok || rule == nil || rule.emit == nil || len(rule.emit.Stmts) == 0 {
		return term, nil
	}

	sel, selectedTerm, err := s.emitSelection(rule, env, true)
	if err != nil {
		return nil, err
	}

	if selectedTerm != nil {
		// Preserve any prefix instructions emitted by the descriptor so the
		// caller can splice them into the block before the final terminator.
		// This is how ABI return-register moves are carried through.
		// (SelectBlock consumes termPrefix immediately after selection.)
		if len(sel) > 0 {
			s.termPrefix = append([]mir.Instr(nil), sel...)
		}
		return selectedTerm, nil
	}

	return term, nil
}

func classifyInstr(inst mir.Instr) matchNode {
	switch i := inst.(type) {
	case *mir.BinaryInstr:
		return matchNode{op: strings.ToLower(i.Op), dst: i.Dst, args: []mir.Operand{i.Left, i.Right}}
	case *mir.UnaryInstr:
		return matchNode{op: strings.ToLower(i.Op), dst: i.Dst, args: []mir.Operand{i.X}}
	case *mir.CompareInstr:
		return matchNode{op: strings.ToLower(i.Pred), dst: i.Dst, args: []mir.Operand{i.Left, i.Right}}
	case *mir.LoadInstr:
		return matchNode{op: "load", dst: i.Dst, args: []mir.Operand{i.Addr}}
	case *mir.StoreInstr:
		return matchNode{op: "store", args: []mir.Operand{i.Value, i.Addr}}
	case *mir.CallInstr:
		args := make([]mir.Operand, 0, 1+len(i.Args))
		args = append(args, i.Callee)
		args = append(args, i.Args...)
		return matchNode{op: "call", dst: i.Dst, args: args}
	case *mir.PhiInstr:
		args := make([]mir.Operand, 0, len(i.Arms))
		for _, arm := range i.Arms {
			args = append(args, arm.Value)
		}
		return matchNode{op: "phi", dst: i.Dst, args: args}
	case *mir.MachineInstr:
		return matchNode{op: strings.ToLower(i.Op), dst: firstRegister(i.Dsts), args: i.Srcs}
	case *mir.MoveInstr:
		return matchNode{op: "mov", dst: i.Dst, args: []mir.Operand{i.Src}}
	case *mir.GEPInstr:
		// Build args: base + all indices (strides/offsets are metadata for predicates)
		args := make([]mir.Operand, 0, 1+len(i.Indices))
		args = append(args, i.Base)
		// Encode strides and offsets as fake "operands" for pattern matching
		// First append stride operands: one per dimension
		for _, stride := range i.Strides {
			args = append(args, mir.NewImmediate(int64(stride), mir.NewScalarType("i64", 8)))
		}
		// Then append offset values: one per dimension
		for _, offset := range i.Offsets {
			args = append(args, mir.NewImmediate(int64(offset), mir.NewScalarType("i64", 8)))
		}
		// Finally, append the runtime indices
		args = append(args, i.Indices...)
		return matchNode{op: "gep", dst: i.Dst, args: args}
	default:
		return matchNode{}
	}
}

func classifyTerm(term mir.Terminator) matchNode {
	switch t := term.(type) {
	case *mir.JumpInstr:
		return matchNode{op: "jmp", args: []mir.Operand{&mir.Label{Name: t.Target}}}
	case *mir.CondBrInstr:
		return matchNode{op: "br", args: []mir.Operand{t.Cond, &mir.Label{Name: t.TrueLabel}, &mir.Label{Name: t.FalseLabel}}}
	case *mir.ReturnInstr:
		if t.Value == nil {
			return matchNode{op: "ret"}
		}
		return matchNode{op: "ret", args: []mir.Operand{t.Value}}
	case *mir.HaltInstr:
		if t.Code == nil {
			return matchNode{op: "halt"}
		}
		return matchNode{op: "halt", args: []mir.Operand{t.Code}}
	case *mir.SwitchInstr:
		args := make([]mir.Operand, 0, 1+len(t.Arms))
		args = append(args, t.Value)
		for _, arm := range t.Arms {
			args = append(args, &mir.Label{Name: arm.Label})
		}
		return matchNode{op: "switch", args: args}
	case *mir.MachineTerm:
		return matchNode{op: strings.ToLower(t.Op), args: t.Srcs}
	default:
		return matchNode{}
	}
}

func (s *Selector) bestRule(node matchNode) (*compiledRule, map[string]any, bool) {
	cands := s.byOp[node.op]
	if len(cands) == 0 {
		cands = s.rules
	}

	bestCost := int(^uint(0) >> 1)
	var best *compiledRule
	var bestEnv map[string]any
	for _, rule := range cands {
		env := map[string]any{}
		if !s.tryMatch(rule, node, env) {
			continue
		}

		if !s.checkPredicates(rule, env) {
			continue
		}

		if err := s.applyBindings(rule, node, env); err != nil {
			continue
		}
		if rule.cost < bestCost {
			bestCost = rule.cost
			best = rule
			bestEnv = env
		}
	}

	return best, bestEnv, best != nil
}

func (s *Selector) tryMatch(rule *compiledRule, node matchNode, env map[string]any) bool {
	if rule == nil || rule.pattern == nil {
		return false
	}
	if strings.ToLower(rule.pattern.Name) != node.op {
		return false
	}
	if len(rule.pattern.Args) != len(node.args) {
		if !(rule.commutative && len(rule.pattern.Args) == 2 && len(node.args) == 2) {
			return false
		}
	}

	if s.matchArgs(rule.pattern.Args, node.args, env) {
		return true
	}
	if rule.commutative && len(rule.pattern.Args) == 2 && len(node.args) == 2 {
		return s.matchArgs(rule.pattern.Args, []mir.Operand{node.args[1], node.args[0]}, env)
	}
	return false
}

func (s *Selector) matchArgs(patternArgs []PatternArg, actual []mir.Operand, env map[string]any) bool {
	if len(patternArgs) != len(actual) {
		return false
	}
	for i := range patternArgs {
		if !s.matchPatternArg(patternArgs[i], actual[i], env) {
			return false
		}
	}
	return true
}

func (s *Selector) matchPatternArg(p PatternArg, actual mir.Operand, env map[string]any) bool {
	switch x := p.(type) {
	case *Value:
		return s.matchValue(x, actual, env)
	case *PatternExpr:
		return s.matchPatternExpr(x, actual, env)
	default:
		return false
	}
}

func (s *Selector) matchPatternExpr(pe *PatternExpr, actual mir.Operand, env map[string]any) bool {
	if pe == nil {
		return false
	}
	if mem, ok := actual.(*mir.Memory); ok {
		switch strings.ToLower(pe.Name) {
		case "add":
			return s.matchArgs(pe.Args, []mir.Operand{mem.Base, mem.Offset}, env)
		case "mem":
			if len(pe.Args) == 1 {
				if rec, ok := pe.Args[0].(*Value); ok {
					return s.matchMemoryRecord(rec, mem, env)
				}
			}
			return s.matchArgs(pe.Args, []mir.Operand{mem.Base, mem.Offset}, env)
		default:
			return false
		}
	}
	return strings.EqualFold(pe.Name, operandName(actual))
}

func (s *Selector) matchValue(v *Value, actual mir.Operand, env map[string]any) bool {
	if v == nil {
		return false
	}
	if strings.HasPrefix(v.Lit, "$") {
		name := trimDollar(v.Lit)
		if prev, ok := env[name]; ok {
			return sameCapturedOperand(prev, actual)
		}
		env[name] = actual
		return true
	}
	switch v.Kind {
	case ValueString:
		return fmt.Sprint(actual) == v.Lit
	case ValueNumber:
		return operandString(actual) == v.Lit
	case ValueIdent:
		if parsed, ok := decodeTypedLiteral(v.Lit, env); ok {
			if op, ok := asOperand(parsed); ok {
				return sameCapturedOperand(op, actual)
			}
			return operandStringFromAny(parsed) == operandString(actual)
		}
		return strings.EqualFold(operandString(actual), v.Lit)
	case ValueRef:
		name := trimDollar(v.Ref)
		if prev, ok := env[name]; ok {
			return sameCapturedOperand(prev, actual)
		}
		env[name] = actual
		return true
	case ValuePattern:
		return s.matchPatternExpr(v.Pattern, actual, env)
	case ValueRecord:
		if mem, ok := actual.(*mir.Memory); ok {
			return s.matchMemoryRecord(v, mem, env)
		}
		return false
	case ValueList:
		return false
	default:
		return false
	}
}

func (s *Selector) matchMemoryRecord(v *Value, mem *mir.Memory, env map[string]any) bool {
	if v == nil || mem == nil {
		return false
	}

	fields := map[string]*Value{}
	for _, f := range v.Fields {
		fields[strings.ToLower(f.Key)] = f.Val
	}

	if base, ok := fields["base"]; ok {
		if !s.matchValue(base, mem.Base, env) {
			return false
		}
	}

	if off, ok := fields["offset"]; ok {
		if !s.matchValue(off, mem.Offset, env) {
			return false
		}
	}

	return true
}

func (s *Selector) applyBindings(rule *compiledRule, node matchNode, env map[string]any) error {
	if rule.out != nil {
		if node.dst == nil {
			return fmt.Errorf("missing destination for out binding %s", rule.out.Name)
		}
		if !bindValue(rule.out, node.dst, env) {
			return fmt.Errorf("out binding %s type mismatch", rule.out.Name)
		}
	}

	if len(rule.in) > len(node.args) {
		return fmt.Errorf("rule %s expects %d inputs, got %d", rule.name, len(rule.in), len(node.args))
	}

	for i, b := range rule.in {
		if !bindValue(b, node.args[i], env) {
			return fmt.Errorf("in binding %s type mismatch", b.Name)
		}
	}

	for _, t := range rule.temps {
		name := trimDollar(t.Name)
		if name == "" {
			continue
		}
		if _, ok := env[name]; !ok {
			if reg, ok := fixedRegisterForBinding(t); ok {
				env[name] = reg
			} else {
				env[name] = s.nextTempRegister()
			}
		}
	}

	return nil
}

func bindValue(b *Binding, actual mir.Operand, env map[string]any) bool {
	if b == nil {
		return true
	}

	if !bindingMatches(b.Type, actual) {
		return false
	}

	name := trimDollar(b.Name)
	if name == "" {
		return true
	}

	if prev, ok := env[name]; ok {
		return sameCapturedOperand(prev, actual)
	}

	env[name] = actual

	return true
}

func bindingMatches(ts TypeSpec, actual mir.Operand) bool {
	if ts.Kind == "" {
		return true
	}
	switch strings.ToLower(ts.Kind) {
	case "gpr", "spr":
		reg, ok := actual.(*mir.Register)
		if !ok {
			return false
		}
		if ts.Sub != "" {
			switch strings.ToLower(ts.Sub) {
			case "virt":
				if reg.Kind != mir.VirtualReg {
					return false
				}
			case "phys":
				if reg.Kind != mir.PhysicalReg {
					return false
				}
			}
		}
		return true
	case "fpr", "fprstrict", "gprstrict":
		reg, ok := actual.(*mir.Register)
		if !ok {
			return false
		}
		if !registerMatchesClass(strings.ToLower(ts.Kind), reg) {
			return false
		}
		if ts.Sub != "" {
			switch strings.ToLower(ts.Sub) {
			case "virt":
				if reg.Kind != mir.VirtualReg {
					return false
				}
			case "phys":
				if reg.Kind != mir.PhysicalReg {
					return false
				}
			}
		}
		return true
	case "imm":
		_, ok := actual.(*mir.Immediate)
		return ok
	case "label":
		_, ok := actual.(*mir.Label)
		return ok
	case "symbol":
		_, ok := actual.(*mir.Symbol)
		return ok
	case "mem":
		_, ok := actual.(*mir.Memory)
		return ok
	default:
		return true
	}
}

func registerMatchesClass(class string, reg *mir.Register) bool {
	if reg == nil {
		return false
	}
	switch class {
	case "fpr", "fprstrict":
		return isFloatRegister(reg)
	case "gprstrict":
		return !isFloatRegister(reg)
	default:
		return true
	}
}

func isFloatRegister(reg *mir.Register) bool {
	if reg == nil {
		return false
	}
	if ty := reg.Type(); ty != nil {
		if isFloatType(ty) {
			return true
		}
		return false
	}
	return looksFloatRegisterName(reg.Name)
}

func isFloatType(ty *mir.Type) bool {
	if ty == nil {
		return false
	}
	name := strings.ToLower(ty.Name)
	return strings.HasPrefix(name, "f")
}

func looksFloatRegisterName(name string) bool {
	n := strings.ToLower(strings.TrimSpace(name))
	if n == "" || n == "fp" {
		return false
	}
	if strings.HasPrefix(n, "f") {
		return true // riscv: f0/ft0/fa0
	}
	if len(n) >= 2 {
		head := n[:1]
		if head == "d" || head == "s" || head == "h" || head == "q" || head == "v" {
			for _, r := range n[1:] {
				if r < '0' || r > '9' {
					return false
				}
			}
			return true
		}
	}
	return false
}

func (s *Selector) checkPredicates(rule *compiledRule, env map[string]any) bool {
	for _, pred := range rule.conds {
		fn, ok := s.predicates[pred.Name]
		if !ok {
			return false
		}

		ok = fn(env, pred.Args)
		if pred.Negated {
			ok = !ok
		}

		if !ok {
			return false
		}
	}
	return true
}

func (s *Selector) emitSelection(rule *compiledRule, env map[string]any, termContext bool) ([]mir.Instr, mir.Terminator, error) {
	if rule == nil || rule.emit == nil {
		return nil, nil, nil
	}

	var instrs []mir.Instr
	var term mir.Terminator
	var legalizeRet bool
	if legalizeInstrs, hasReturnMove, err := s.emitLegalize(rule, env); err != nil {
		return nil, nil, err
	} else if len(legalizeInstrs) > 0 {
		instrs = append(instrs, legalizeInstrs...)
		legalizeRet = hasReturnMove
	}

	for _, stmt := range rule.emit.Stmts {
		i, t, err := s.emitStmt(stmt, env, termContext)
		if err != nil {
			return nil, nil, err
		}
		instrs = append(instrs, i...)
		if t != nil {
			term = t
		}
	}

	if termContext && legalizeRet {
		legalize.NormalizeReturnTerminator(term)
	}

	return instrs, term, nil
}

func (s *Selector) emitLegalize(rule *compiledRule, env map[string]any) ([]mir.Instr, bool, error) {
	if rule == nil || rule.legalize == nil {
		return nil, false, nil
	}

	var instrs []mir.Instr
	var normalizeRet bool
	for _, item := range rule.legalize.Items {
		i, ok, err := s.emitLegalizeItem(item, env)
		if err != nil {
			return nil, false, err
		}
		if !ok {
			return nil, false, nil
		}
		instrs = append(instrs, i...)
		if legalize.EmitsReturnMove(i, s.abi) {
			normalizeRet = true
		}
	}

	return instrs, normalizeRet, nil
}

func (s *Selector) emitLegalizeItem(item LegalizeItem, env map[string]any) ([]mir.Instr, bool, error) {
	switch x := item.(type) {
	case *RequireItem:
		if !s.evalPredicate(x.Pred, env) {
			return nil, false, nil
		}
		return nil, true, nil
	case *RewriteItem:
		args := make([]mir.Operand, 0, len(x.Expr.Args))
		for _, arg := range x.Expr.Args {
			op, ok := resolveEmitValue(asValueArg(arg), env)
			if !ok {
				return nil, false, nil
			}
			args = append(args, op)
		}
		return legalize.BuildRewrite(x.Expr.Name, args)
	case *MoveItem:
		src, ok := resolveEmitValue(x.From, env)
		if !ok {
			return nil, false, nil
		}
		dst, ok := resolveEmitValue(x.To, env)
		if !ok {
			return nil, false, nil
		}
		return legalize.BuildMove(src, dst)
	case *SpillItem:
		op, ok := legalize.ResolveOperand(x.Name, env)
		if !ok {
			return nil, false, nil
		}
		return legalize.BuildSpill(op)
	case *ReloadItem:
		op, ok := legalize.ResolveOperand(x.Name, env)
		if !ok {
			return nil, false, nil
		}
		return legalize.BuildReload(op)
	case *IfItem:
		if !s.evalPredicate(x.Pred, env) {
			return nil, true, nil
		}
		switch a := x.Action.(type) {
		case *RewriteExpr:
			args := make([]mir.Operand, 0, len(a.Args))
			for _, arg := range a.Args {
				op, ok := resolveEmitValue(asValueArg(arg), env)
				if !ok {
					return nil, false, nil
				}
				args = append(args, op)
			}
			return legalize.BuildRewrite(a.Name, args)
		case *SpillAction:
			op, ok := legalize.ResolveOperand(a.Name, env)
			if !ok {
				return nil, false, nil
			}
			return legalize.BuildSpill(op)
		case *ReloadAction:
			op, ok := legalize.ResolveOperand(a.Name, env)
			if !ok {
				return nil, false, nil
			}
			return legalize.BuildReload(op)
		case *MoveAction:
			src, ok := resolveEmitValue(a.From, env)
			if !ok {
				return nil, false, nil
			}
			dst, ok := resolveEmitValue(a.To, env)
			if !ok {
				return nil, false, nil
			}
			return legalize.BuildMove(src, dst)
		default:
			return nil, true, nil
		}
	default:
		return nil, true, nil
	}
}

func (s *Selector) evalPredicate(pred *Predicate, env map[string]any) bool {
	if pred == nil {
		return true
	}
	fn, ok := s.predicates[pred.Name]
	if !ok {
		return false
	}
	matched := fn(env, pred.Args)
	if pred.Negated {
		matched = !matched
	}
	return matched
}

func (s *Selector) emitStmt(stmt EmitStmt, env map[string]any, termContext bool) ([]mir.Instr, mir.Terminator, error) {
	switch x := stmt.(type) {
	case *InstrStmt:
		return s.emitInstrStmt(x, env, termContext)
	case *TemplateStmt, *CommentStmt:
		return nil, nil, nil
	default:
		return nil, nil, nil
	}
}

func (s *Selector) emitInstrStmt(stmt *InstrStmt, env map[string]any, termContext bool) ([]mir.Instr, mir.Terminator, error) {
	if stmt == nil {
		return nil, nil, nil
	}
	var opcode string
	var dsts []*mir.Register
	var srcs []mir.Operand

	for _, f := range stmt.Fields {
		switch strings.ToLower(f.Name) {
		case "opcode":
			opcode = emitString(f.Val, env)
		case "dst":
			if op, ok := resolveEmitValue(f.Val, env); ok {
				if reg, ok := op.(*mir.Register); ok {
					dsts = append(dsts, reg)
				} else {
					srcs = append(srcs, op)
				}
			}
		case "src", "uses":
			vals, err := resolveEmitList(f.Val, env)
			if err != nil {
				return nil, nil, err
			}
			srcs = append(srcs, vals...)
		case "def":
			if op, ok := resolveEmitValue(f.Val, env); ok {
				if reg, ok := op.(*mir.Register); ok && len(dsts) == 0 {
					dsts = append(dsts, reg)
				}
			}
		case "imm":
			if op, ok := resolveEmitValue(f.Val, env); ok {
				srcs = append(srcs, op)
			}
		}
	}

	if opcode == "" {
		return nil, nil, fmt.Errorf("missing opcode in emit instruction")
	}
	if termContext && isTermOpcode(opcode) {
		return nil, mir.NewMachineTerm(opcode, srcs, terminalLabels(srcs)), nil
	}

	return []mir.Instr{mir.NewMachineInstr(opcode, dsts, srcs)}, nil, nil
}

func resolveEmitList(v *Value, env map[string]any) ([]mir.Operand, error) {
	if v == nil {
		return nil, nil
	}

	if v.Kind == ValueList {
		out := make([]mir.Operand, 0, len(v.Elems))
		for _, elem := range v.Elems {
			op, ok := resolveEmitValue(elem, env)
			if !ok {
				continue
			}
			out = append(out, op)
		}
		return out, nil
	}

	op, ok := resolveEmitValue(v, env)
	if !ok {
		return nil, nil
	}

	return []mir.Operand{op}, nil
}

func resolveEmitValue(v *Value, env map[string]any) (mir.Operand, bool) {
	if v == nil {
		return nil, false
	}

	if strings.HasPrefix(v.Lit, "$") {
		if resolved, ok := env[trimDollar(v.Lit)]; ok {
			if op, ok := asOperand(resolved); ok {
				return op, true
			}
			return nil, false
		}
	}

	switch v.Kind {
	case ValueRef:
		if resolved, ok := env[trimDollar(v.Ref)]; ok {
			if op, ok := asOperand(resolved); ok {
				return op, true
			}
		}
		return nil, false
	case ValueIdent:
		if parsed, ok := decodeTypedLiteral(v.Lit, env); ok {
			if op, ok := asOperand(parsed); ok {
				return op, true
			}
		}
		return &mir.Label{Name: v.Lit}, true
	case ValueString:
		return &mir.Symbol{Name: v.Lit}, true
	case ValueNumber:
		if n, err := strconv.ParseInt(v.Lit, 10, 64); err == nil {
			return &mir.Immediate{Value: n}, true
		}
		return &mir.Immediate{Value: v.Lit}, true
	case ValuePattern:
		return resolvePatternValue(v.Pattern, env)
	case ValueRecord:
		return resolveRecordValue(v, env)
	case ValueList:
		return nil, false
	default:
		return nil, false
	}
}

func resolvePatternValue(p *PatternExpr, env map[string]any) (mir.Operand, bool) {
	if p == nil {
		return nil, false
	}
	if len(p.Args) == 1 {
		arg := asValueArg(p.Args[0])
		op, ok := resolveEmitValue(arg, env)
		if !ok {
			return nil, false
		}
		switch {
		case strings.EqualFold(p.Name, "hi20"):
			return legalize.BuildHi20(op)
		case strings.EqualFold(p.Name, "lo12"):
			return legalize.BuildLo12(op)
		case strings.EqualFold(p.Name, "imm16_0"):
			return legalize.BuildImm16Part(op, 0)
		case strings.EqualFold(p.Name, "imm16_1"):
			return legalize.BuildImm16Part(op, 1)
		case strings.EqualFold(p.Name, "imm16_2"):
			return legalize.BuildImm16Part(op, 2)
		case strings.EqualFold(p.Name, "imm16_3"):
			return legalize.BuildImm16Part(op, 3)
		case strings.EqualFold(p.Name, "pagehi"):
			return legalize.BuildPageHi(op)
		case strings.EqualFold(p.Name, "pageoff"):
			return legalize.BuildPageOff(op)
		}
	}

	if strings.EqualFold(p.Name, "mem") {
		switch len(p.Args) {
		case 1:
			if rec, ok := p.Args[0].(*Value); ok {
				return resolveRecordValue(rec, env)
			}
		case 2:
			base, ok := resolveEmitValue(asValueArg(p.Args[0]), env)
			if !ok {
				return nil, false
			}
			off, ok := resolveEmitValue(asValueArg(p.Args[1]), env)
			if !ok {
				return nil, false
			}
			return &mir.Memory{Base: base, Offset: off}, true
		}
	}

	return &mir.Symbol{Name: patternString(p)}, true
}

func asValueArg(arg PatternArg) *Value {
	if v, ok := arg.(*Value); ok {
		return v
	}
	return nil
}

func resolveRecordValue(v *Value, env map[string]any) (mir.Operand, bool) {
	if v == nil {
		return nil, false
	}

	var base mir.Operand
	var offset mir.Operand
	for _, f := range v.Fields {
		switch strings.ToLower(f.Key) {
		case "base":
			op, ok := resolveEmitValue(f.Val, env)
			if !ok {
				return nil, false
			}
			base = op
		case "offset", "offs":
			op, ok := resolveEmitValue(f.Val, env)
			if !ok {
				return nil, false
			}
			offset = op
		}
	}

	if base == nil {
		return nil, false
	}

	return &mir.Memory{Base: base, Offset: offset}, true
}

func emitString(v *Value, env map[string]any) string {
	if v == nil {
		return ""
	}

	if strings.HasPrefix(v.Lit, "$") {
		if resolved, ok := env[trimDollar(v.Lit)]; ok {
			return operandStringFromAny(resolved)
		}
	}

	switch v.Kind {
	case ValueString, ValueIdent, ValueNumber, ValueList, ValueRecord, ValuePattern:
		return v.Lit
	case ValueRef:
		if resolved, ok := env[trimDollar(v.Ref)]; ok {
			return operandStringFromAny(resolved)
		}
	default:
		return v.Lit
	}

	if v.Pattern != nil {
		return patternString(v.Pattern)
	}

	return v.Lit
}

func operandString(op mir.Operand) string {
	if op == nil {
		return ""
	}
	return op.String()
}

func operandStringFromAny(v any) string {
	switch x := v.(type) {
	case mir.Operand:
		return x.String()
	case string:
		return x
	default:
		return fmt.Sprint(x)
	}
}

func asOperand(v any) (mir.Operand, bool) {
	op, ok := v.(mir.Operand)
	return op, ok
}

func sameCapturedOperand(a, b any) bool {
	return operandStringFromAny(a) == operandStringFromAny(b)
}

func fixedRegisterForBinding(b *Binding) (*mir.Register, bool) {
	if b == nil {
		return nil, false
	}
	if !strings.EqualFold(b.Type.Sub, "phys") {
		return nil, false
	}
	name := trimDollar(b.Name)
	if name == "" {
		return nil, false
	}
	return &mir.Register{Name: name, Kind: mir.PhysicalReg}, true
}

func operandName(op mir.Operand) string {
	switch x := op.(type) {
	case *mir.Register:
		return x.Name
	case *mir.Label:
		return x.Name
	case *mir.Symbol:
		return x.Name
	case *mir.Immediate:
		return fmt.Sprint(x.Value)
	case *mir.Memory:
		return "mem"
	default:
		return fmt.Sprint(op)
	}
}

func terminalLabels(srcs []mir.Operand) []string {
	labels := make([]string, 0)
	for _, src := range srcs {
		if l, ok := src.(*mir.Label); ok {
			labels = append(labels, l.Name)
		}
	}
	return labels
}

func isTermOpcode(op string) bool {
	switch strings.ToLower(op) {
	case "j", "beq", "bne", "blt", "ble", "bgt", "bge", "br", "ret", "switch", "halt":
		return true
	default:
		return false
	}
}

func trimDollar(s string) string { return strings.TrimPrefix(s, "$") }

func firstRegister(rs []*mir.Register) *mir.Register {
	if len(rs) == 0 {
		return nil
	}
	return rs[0]
}

func patternString(p *PatternExpr) string {
	if p == nil {
		return ""
	}

	if len(p.Args) == 0 {
		return p.Name
	}

	parts := make([]string, 0, len(p.Args))
	for _, arg := range p.Args {
		switch x := arg.(type) {
		case *Value:
			parts = append(parts, valueString(x))
		case *PatternExpr:
			parts = append(parts, patternString(x))
		default:
			parts = append(parts, fmt.Sprint(arg))
		}
	}

	return fmt.Sprintf("%s(%s)", p.Name, strings.Join(parts, ", "))
}

func valueString(v *Value) string {
	if v == nil {
		return ""
	}
	if v.Pattern != nil {
		return patternString(v.Pattern)
	}
	if v.Ref != "" {
		return "$" + v.Ref
	}
	if v.Kind == ValueList {
		parts := make([]string, 0, len(v.Elems))
		for _, e := range v.Elems {
			parts = append(parts, valueString(e))
		}
		return "[" + strings.Join(parts, ", ") + "]"
	}
	if v.Kind == ValueRecord {
		parts := make([]string, 0, len(v.Fields))
		for _, f := range v.Fields {
			parts = append(parts, f.Key+"="+valueString(f.Val))
		}
		return "{" + strings.Join(parts, ", ") + "}"
	}
	return v.Lit
}

func decodeTypedLiteral(lit string, env map[string]any) (any, bool) {
	parts := strings.Split(lit, ":")
	if len(parts) < 2 {
		return nil, false
	}

	kind := strings.ToLower(strings.TrimSpace(parts[0]))
	last := strings.TrimSpace(parts[len(parts)-1])
	if strings.HasPrefix(last, "$") {
		if resolved, ok := env[trimDollar(last)]; ok {
			if op, ok := asOperand(resolved); ok {
				return op, true
			}
			return nil, false
		}
	}

	if len(parts) == 3 && (kind == "gpr" || kind == "fpr" || kind == "spr") {
		mode := strings.ToLower(strings.TrimSpace(parts[1]))
		name := strings.TrimPrefix(last, "$")
		regKind := mir.VirtualReg

		if mode == "phys" {
			regKind = mir.PhysicalReg
		}

		return &mir.Register{Name: name, Kind: regKind}, true
	}

	if len(parts) == 2 {
		switch kind {
		case "label":
			return &mir.Label{Name: strings.TrimPrefix(last, "$")}, true
		case "symbol":
			return &mir.Symbol{Name: strings.TrimPrefix(last, "$")}, true
		case "imm":
			if n, err := strconv.ParseInt(strings.TrimPrefix(last, "$"), 10, 64); err == nil {
				return &mir.Immediate{Value: n}, true
			}
			return &mir.Immediate{Value: strings.TrimPrefix(last, "$")}, true
		}
	}

	return nil, false
}

func (s *Selector) nextTempRegister() *mir.Register {
	s.tempMu.Lock()
	defer s.tempMu.Unlock()

	name := fmt.Sprintf("__sel_tmp%d", s.tempSeq)
	s.tempSeq++

	return &mir.Register{Name: name, Kind: mir.VirtualReg}
}

func defaultPredicates(abi string) map[string]PredicateFunc {
	return map[string]PredicateFunc{
		"UImmFits16": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			return ok && v >= 0 && v <= 65535
		},
		"FitsLogicalImm": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}

			isShiftedMask := func(x uint64, size int) bool {
				mask := uint64(1<<size) - 1
				x &= mask

				if x == 0 || x == mask {
					return false
				}

				for rot := 0; rot < size; rot++ {
					r := bits.RotateLeft64(x, -rot) & mask

					// Check for form:
					// 000..00111..11
					if (r & (r + 1)) == 0 {
						return true
					}
				}

				return false
			}

			if v, ok := resolvePredicateInt(args[0], env); ok {
				vu := uint64(v)

				// All-zero and all-ones are NOT encodable
				if vu == 0 || vu == ^uint64(0) {
					return false
				}

				// Try every element size
				for size := 2; size <= 64; size <<= 1 {

					mask := uint64(1<<size) - 1
					pattern := vu & mask

					// Check if pattern repeats across 64 bits
					repeated := uint64(0)
					for i := 0; i < 64; i += size {
						repeated |= pattern << i
					}

					if repeated != vu {
						continue
					}

					// Pattern must contain:
					// one contiguous run of 1 bits,
					// possibly rotated.

					if isShiftedMask(pattern, size) {
						return true
					}
				}

				return false
			}
			return false
		},
		"FitsAddSubImm": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}

			if v, ok := resolvePredicateInt(args[0], env); ok {
				if v >= 0 && v <= 4095 {
					return true
				}
				if (v & 0xfff) == 0 {
					return (v >> 12) <= 4095
				}
				return false
			}

			return false
		},
		"UImmFits12": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			return ok && v >= 0 && v <= 4095
		},
		"SImmFits12": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			return ok && v >= -2048 && v <= 2047
		},
		"ShamtFits6": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			return ok && v >= 0 && v <= 63
		},
		"ABIIs": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			want := emitString(args[0], env)
			return want != "" && strings.EqualFold(want, abi)
		},
		// GEP optimization predicates

		// IsPowerOf2 checks if a value is a power of 2
		"IsPowerOf2": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			if !ok || v <= 0 {
				return false
			}
			// v is power of 2 iff v & (v-1) == 0
			return v&(v-1) == 0
		},

		// IsScaleFactor checks if value is 1, 2, 4, or 8 (ARM64 addressing mode scales)
		"IsScaleFactor": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			return ok && (v == 1 || v == 2 || v == 4 || v == 8)
		},

		// Log2 computes log2 of a power-of-2 value (for shift amount)
		"Log2": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			if !ok || v <= 0 || (v&(v-1)) != 0 {
				return false // not a power of 2
			}
			// Store log2 result; value is power of 2
			return true
		},

		// CanFoldConstOffset checks if compile-time offset can be folded into immediate
		"CanFoldConstOffset": func(env map[string]any, args []*Value) bool {
			if len(args) != 1 {
				return false
			}
			v, ok := resolvePredicateInt(args[0], env)
			if !ok {
				return false
			}
			// Check if offset fits in 12-bit signed immediate (ARM64) or 12-bit signed (RV64)
			return v >= -2048 && v <= 4095
		},
	}
}

func resolvePredicateInt(v *Value, env map[string]any) (int64, bool) {
	if v == nil {
		return 0, false
	}

	if strings.HasPrefix(v.Lit, "$") {
		if resolved, ok := env[trimDollar(v.Lit)]; ok {
			switch x := resolved.(type) {
			case *mir.Immediate:
				return immediateToInt64(x)
			case int64:
				return x, true
			case int:
				return int64(x), true
			case string:
				parsed, err := strconv.ParseInt(x, 10, 64)
				return parsed, err == nil
			}
		}
	}

	switch v.Kind {
	case ValueString, ValueIdent, ValuePattern, ValueList, ValueRecord:
		return 0, false
	case ValueNumber:
		parsed, err := strconv.ParseInt(v.Lit, 10, 64)
		return parsed, err == nil
	case ValueRef:
		if resolved, ok := env[trimDollar(v.Ref)]; ok {
			switch x := resolved.(type) {
			case *mir.Immediate:
				return immediateToInt64(x)
			case int64:
				return x, true
			case int:
				return int64(x), true
			case string:
				parsed, err := strconv.ParseInt(x, 10, 64)
				return parsed, err == nil
			}
		}
	default:
		return 0, false
	}

	return 0, false
}

func immediateToInt64(i *mir.Immediate) (int64, bool) {
	if i == nil {
		return 0, false
	}
	switch v := i.Value.(type) {
	case int:
		return int64(v), true
	case int8:
		return int64(v), true
	case int16:
		return int64(v), true
	case int32:
		return int64(v), true
	case int64:
		return v, true
	case uint:
		return int64(v), true
	case uint8:
		return int64(v), true
	case uint16:
		return int64(v), true
	case uint32:
		return int64(v), true
	case uint64:
		if v > ^uint64(0)>>1 {
			return 0, false
		}
		return int64(v), true
	case string:
		parsed, err := strconv.ParseInt(v, 10, 64)
		return parsed, err == nil
	default:
		return 0, false
	}
}
