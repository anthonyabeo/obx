package miniropt

import (
	"fmt"
	"io"
	"log"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/minir"
)

// PassID uniquely identifies a pass.
type PassID string

const (
	PassMem2Reg     PassID = "mem2reg"
	PassLoadForward PassID = "loadfwd"
	PassConstFold   PassID = "constfold"
	PassCleanCFG    PassID = "cleancfg"
	PassSimplify    PassID = "simplify"
	PassStrength    PassID = "strength"
)

// Pass describes a single optimization pass.
type Pass struct {
	ID          PassID
	Name        string
	Description string
	// Fn is the pass implementation: func(*minir.Function) int (returns changes count)
	Fn func(*minir.Function) int
}

// PassStats tracks statistics about pass execution.
type PassStats struct {
	PassID         PassID
	PassName       string
	FunctionName   string
	ChangesCount   int
	InstrsInBefore int
	InstrsInAfter  int
}

// PassRegistry maps pass IDs to Pass implementations.
type PassRegistry struct {
	passes map[PassID]Pass
	order  []PassID // canonical pass ordering
}

// NewPassRegistry creates a new registry with all built-in passes.
func NewPassRegistry() *PassRegistry {
	pr := &PassRegistry{
		passes: make(map[PassID]Pass),
		order:  []PassID{PassMem2Reg, PassLoadForward, PassConstFold, PassCleanCFG, PassSimplify, PassStrength},
	}

	// Register passes in canonical order
	pr.Register(Pass{
		ID:          PassMem2Reg,
		Name:        "mem2reg",
		Description: "Promote non-escaping scalar allocas to SSA values",
		Fn:          Mem2Reg,
	})
	pr.Register(Pass{
		ID:          PassLoadForward,
		Name:        "loadfwd",
		Description: "Forward stored values through loads (redundant load elimination)",
		Fn:          LoadForward,
	})
	pr.Register(Pass{
		ID:          PassConstFold,
		Name:        "constfold",
		Description: "Fold instructions with constant operands",
		Fn:          ConstantFold,
	})
	pr.Register(Pass{
		ID:          PassCleanCFG,
		Name:        "cleancfg",
		Description: "Clean up control-flow graph (remove dead blocks, merge blocks, etc.)",
		Fn:          CleanCFG,
	})
	pr.Register(Pass{
		ID:          PassSimplify,
		Name:        "simplify",
		Description: "Algebraic simplification using identity and annihilator rules",
		Fn:          AlgebraicSimplify,
	})
	pr.Register(Pass{
		ID:          PassStrength,
		Name:        "strength",
		Description: "Strength reduction of expensive operations",
		Fn:          StrengthReduce,
	})

	return pr
}

// Register adds a pass to the registry.
func (pr *PassRegistry) Register(p Pass) {
	pr.passes[p.ID] = p
}

// Lookup retrieves a pass by ID.
func (pr *PassRegistry) Lookup(id PassID) (Pass, bool) {
	p, ok := pr.passes[id]
	return p, ok
}

// All returns all registered passes in canonical order.
func (pr *PassRegistry) All() []Pass {
	out := make([]Pass, 0, len(pr.order))
	for _, id := range pr.order {
		if p, ok := pr.passes[id]; ok {
			out = append(out, p)
		}
	}
	return out
}

// PassLevel defines which passes to run at each optimization level.
type PassLevel struct {
	Level  int      // 0, 1, 2, 3
	Passes []PassID // passes to run at this level
}

// DefaultPassLevels returns the standard pass configuration for each opt level.
func DefaultPassLevels() map[int][]PassID {
	return map[int][]PassID{
		0: {},                                                                                      // None
		1: {PassMem2Reg, PassLoadForward, PassConstFold, PassCleanCFG},                             // Basic
		2: {PassMem2Reg, PassLoadForward, PassConstFold, PassCleanCFG, PassSimplify},               // Default
		3: {PassMem2Reg, PassLoadForward, PassConstFold, PassCleanCFG, PassSimplify, PassStrength}, // Aggressive
	}
}

// PassConfiguration holds the resolved pass list and execution options.
type PassConfiguration struct {
	Passes         []PassID
	FixedPoint     bool      // repeat passes to fixed point
	FixedPointIter int       // max iterations for fixed-point
	Verbose        bool      // print pass statistics
	LogWriter      io.Writer // where to write verbose output
}

// PassManager orchestrates pass execution on a minir program.
type PassManager struct {
	registry *PassRegistry
	config   PassConfiguration
	stats    []PassStats
}

// NewPassManager creates a new pass manager with default configuration.
func NewPassManager() *PassManager {
	return &PassManager{
		registry: NewPassRegistry(),
		config: PassConfiguration{
			Passes:         []PassID{},
			FixedPoint:     false,
			FixedPointIter: 10,
			Verbose:        false,
			LogWriter:      log.Writer(),
		},
		stats: []PassStats{},
	}
}

// ConfigureFromLevel sets up passes based on optimization level.
func (pm *PassManager) ConfigureFromLevel(level int) {
	passLevels := DefaultPassLevels()
	if passes, ok := passLevels[level]; ok {
		pm.config.Passes = passes
	}
	// Set fixed-point for higher optimization levels
	pm.config.FixedPoint = level >= 2
}

// ConfigureFromPassList sets up passes from an explicit pass list.
// passNames should be comma-separated pass names (e.g., "mem2reg,constfold").
func (pm *PassManager) ConfigureFromPassList(passNames string) error {
	if strings.TrimSpace(passNames) == "" {
		pm.config.Passes = []PassID{}
		return nil
	}

	names := strings.Split(passNames, ",")
	var passes []PassID
	for _, name := range names {
		name = strings.TrimSpace(name)
		if name == "" {
			continue
		}
		id := PassID(name)
		if _, ok := pm.registry.Lookup(id); !ok {
			return fmt.Errorf("unknown pass: %q", name)
		}
		passes = append(passes, id)
	}
	pm.config.Passes = passes
	pm.config.FixedPoint = false // explicit list disables fixed-point by default
	return nil
}

// DisablePasses removes passes from the configuration.
// passNames should be comma-separated pass names.
func (pm *PassManager) DisablePasses(passNames string) error {
	if strings.TrimSpace(passNames) == "" {
		return nil
	}

	toDisable := make(map[PassID]bool)
	names := strings.Split(passNames, ",")
	for _, name := range names {
		name = strings.TrimSpace(name)
		if name == "" {
			continue
		}
		id := PassID(name)
		if _, ok := pm.registry.Lookup(id); !ok {
			return fmt.Errorf("unknown pass: %q", name)
		}
		toDisable[id] = true
	}

	filtered := make([]PassID, 0, len(pm.config.Passes))
	for _, p := range pm.config.Passes {
		if !toDisable[p] {
			filtered = append(filtered, p)
		}
	}
	pm.config.Passes = filtered
	return nil
}

// SetVerbose enables/disables verbose output.
func (pm *PassManager) SetVerbose(verbose bool) {
	pm.config.Verbose = verbose
}

// SetLogWriter sets where verbose output is written.
func (pm *PassManager) SetLogWriter(w io.Writer) {
	pm.config.LogWriter = w
}

// SetFixedPoint enables/disables fixed-point iteration.
func (pm *PassManager) SetFixedPoint(fp bool) {
	pm.config.FixedPoint = fp
}

// SetFixedPointIterations sets the maximum iterations for fixed-point.
func (pm *PassManager) SetFixedPointIterations(iters int) {
	if iters <= 0 {
		iters = 10
	}
	pm.config.FixedPointIter = iters
}

// Stats returns collected pass statistics.
func (pm *PassManager) Stats() []PassStats {
	return pm.stats
}

// ClearStats clears collected statistics.
func (pm *PassManager) ClearStats() {
	pm.stats = []PassStats{}
}

// RunOnFunction runs the configured passes on a single function.
// Returns the total number of changes made across all passes.
func (pm *PassManager) RunOnFunction(fn *minir.Function) int {
	if fn == nil {
		return 0
	}

	if len(pm.config.Passes) == 0 {
		return 0
	}

	totalChanges := 0

	if pm.config.FixedPoint {
		totalChanges = pm.runFixedPoint(fn)
	} else {
		for _, passID := range pm.config.Passes {
			totalChanges += pm.runPass(fn, passID)
		}
	}

	return totalChanges
}

// runPass executes a single pass on a function and records statistics.
func (pm *PassManager) runPass(fn *minir.Function, passID PassID) int {
	pass, ok := pm.registry.Lookup(passID)
	if !ok {
		return 0
	}

	instrsBefore := pm.countInstrs(fn)
	changes := pass.Fn(fn)
	instrsAfter := pm.countInstrs(fn)

	stat := PassStats{
		PassID:         passID,
		PassName:       pass.Name,
		FunctionName:   fn.FnName,
		ChangesCount:   changes,
		InstrsInBefore: instrsBefore,
		InstrsInAfter:  instrsAfter,
	}

	if pm.config.Verbose {
		fmt.Fprintf(pm.config.LogWriter, "[%s] %s::%s changed=%d instrs=%d→%d\n",
			passID, fn.FnName, pass.Name, changes, instrsBefore, instrsAfter)
	}

	pm.stats = append(pm.stats, stat)
	return changes
}

// runFixedPoint runs passes repeatedly until no changes occur.
func (pm *PassManager) runFixedPoint(fn *minir.Function) int {
	totalChanges := 0
	for iter := 0; iter < pm.config.FixedPointIter; iter++ {
		iterChanges := 0
		for _, passID := range pm.config.Passes {
			iterChanges += pm.runPass(fn, passID)
		}
		totalChanges += iterChanges
		if iterChanges == 0 {
			break
		}
	}
	return totalChanges
}

// RunOnModule runs the configured passes on all functions in a module.
func (pm *PassManager) RunOnModule(mod *minir.Module) int {
	if mod == nil {
		return 0
	}
	totalChanges := 0
	for _, fn := range mod.Functions {
		totalChanges += pm.RunOnFunction(fn)
	}
	return totalChanges
}

// RunOnProgram runs the configured passes on all functions in all modules.
func (pm *PassManager) RunOnProgram(prog *minir.Program) int {
	if prog == nil {
		return 0
	}
	totalChanges := 0
	for _, mod := range prog.Modules {
		totalChanges += pm.RunOnModule(mod)
	}
	return totalChanges
}

// countInstrs counts the total number of instructions in a function.
func (pm *PassManager) countInstrs(fn *minir.Function) int {
	if fn == nil {
		return 0
	}
	count := 0
	for _, b := range fn.Blocks {
		count += len(b.Instrs)
		if b.Term != nil {
			count++ // count the terminator
		}
	}
	return count
}

// AvailablePasses returns a list of available pass names.
func (pm *PassManager) AvailablePasses() []string {
	passes := pm.registry.All()
	names := make([]string, len(passes))
	for i, p := range passes {
		names[i] = p.Name
	}
	return names
}

// PassHelp returns a formatted help string for available passes.
func (pm *PassManager) PassHelp() string {
	passes := pm.registry.All()
	var sb strings.Builder
	sb.WriteString("Available passes:\n")
	for _, p := range passes {
		sb.WriteString(fmt.Sprintf("  %-16s %s\n", p.Name, p.Description))
	}
	return sb.String()
}
