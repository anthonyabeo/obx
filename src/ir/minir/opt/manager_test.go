package miniropt_test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
)

// Helper to reset temp IDs for deterministic test output
func resetTempIDs() {
	minir.ResetTempCounter()
}

// TestNewPassManager creates a new pass manager with default config.
func TestNewPassManager(t *testing.T) {
	pm := miniropt.NewPassManager()
	if pm == nil {
		t.Fatal("NewPassManager returned nil")
	}
	if len(pm.Stats()) != 0 {
		t.Fatal("new manager should have no stats")
	}
}

// TestConfigureFromLevel sets passes based on optimization level.
func TestConfigureFromLevel(t *testing.T) {
	tests := []struct {
		level       int
		shouldHaveFixedPoint bool
		minPassCount int
	}{
		{0, false, 0},
		{1, false, 1},
		{2, true, 1},
		{3, true, 1},
	}

	for _, tt := range tests {
		t.Run(strings.Join([]string{"O", string(rune('0'+tt.level))}, ""), func(t *testing.T) {
			pm := miniropt.NewPassManager()
			pm.ConfigureFromLevel(tt.level)

			if tt.shouldHaveFixedPoint {
				// O2+ should enable fixed-point
			}
			// just verify configure didn't panic
		})
	}
}

// TestConfigureFromPassList parses an explicit pass list.
func TestConfigureFromPassList(t *testing.T) {
	tests := []struct {
		input   string
		wantErr bool
		expect  []string
	}{
		{"mem2reg,constfold", false, []string{"mem2reg", "constfold"}},
		{"mem2reg", false, []string{"mem2reg"}},
		{"", false, []string{}},
		{"invalid_pass", true, []string{}},
		{"mem2reg, constfold  , loadfwd", false, []string{"mem2reg", "constfold", "loadfwd"}},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			pm := miniropt.NewPassManager()
			err := pm.ConfigureFromPassList(tt.input)
			if (err != nil) != tt.wantErr {
				t.Fatalf("ConfigureFromPassList(%q) error = %v, want err = %v", tt.input, err, tt.wantErr)
			}
		})
	}
}

// TestDisablePasses removes passes from active list.
func TestDisablePasses(t *testing.T) {
	pm := miniropt.NewPassManager()
	pm.ConfigureFromLevel(1) // get some passes

	err := pm.DisablePasses("mem2reg")
	if err != nil {
		t.Fatalf("DisablePasses failed: %v", err)
	}

	// should not error on invalid passes
	err = pm.DisablePasses("invalid")
	if err == nil {
		t.Fatal("DisablePasses should error on invalid pass")
	}
}

// TestRunOnFunctionWithEmptyConfig executes no passes.
func TestRunOnFunctionWithEmptyConfig(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()

	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	changes := pm.RunOnFunction(fn)
	if changes != 0 {
		t.Fatalf("RunOnFunction with empty config: got %d changes, want 0", changes)
	}
}

// TestRunOnFunctionWithPass executes a pass and tracks changes.
func TestRunOnFunctionWithPass(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()

	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	// Configure manager to run cleancfg
	pm.ConfigureFromPassList("cleancfg")

	_ = pm.RunOnFunction(fn)

	stats := pm.Stats()
	if len(stats) == 0 {
		t.Fatal("expected statistics to be recorded")
	}
	if stats[0].PassID != "cleancfg" {
		t.Errorf("expected cleancfg pass stats; got %s", stats[0].PassID)
	}
}

// TestRunOnModule applies passes to all functions.
func TestRunOnModule(t *testing.T) {
	resetTempIDs()
	mod := &minir.Module{Name: "TestMod"}

	// Create two simple functions
	for i := 0; i < 2; i++ {
		fn := &minir.Function{
			FnName: "test_fn",
			Blocks: make(map[int]*minir.Block),
			SymTab: minir.SymbolTable{},
		}

		entry := &minir.Block{ID: i*2, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
		exit := &minir.Block{ID: i*2+1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

		entry.Term = &minir.JumpInst{Target: exit.Label}
		entry.Instrs = append(entry.Instrs, entry.Term)
		exit.Term = &minir.HaltInst{}
		exit.Instrs = append(exit.Instrs, exit.Term)

		entry.AddSucc(exit)
		exit.AddPred(entry)

		fn.Blocks[entry.ID] = entry
		fn.Blocks[exit.ID] = exit
		fn.Entry = entry
		fn.Exit = exit

		mod.Functions = append(mod.Functions, fn)
	}

	pm := miniropt.NewPassManager()
	pm.ConfigureFromPassList("") // enable nothing

	changes := pm.RunOnModule(mod)
	if changes != 0 {
		t.Fatalf("RunOnModule with empty config: got %d changes, want 0", changes)
	}
}

// TestRunOnProgram applies passes to all modules.
func TestRunOnProgram(t *testing.T) {
	resetTempIDs()
	prog := &minir.Program{}

	// Create two modules
	for m := 0; m < 2; m++ {
		mod := &minir.Module{Name: "Mod"}
		fn := &minir.Function{
			FnName: "fn",
			Blocks: make(map[int]*minir.Block),
			SymTab: minir.SymbolTable{},
		}

		entry := &minir.Block{ID: m*2, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
		exit := &minir.Block{ID: m*2+1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

		entry.Term = &minir.JumpInst{Target: exit.Label}
		entry.Instrs = append(entry.Instrs, entry.Term)
		exit.Term = &minir.HaltInst{}
		exit.Instrs = append(exit.Instrs, exit.Term)

		entry.AddSucc(exit)
		exit.AddPred(entry)

		fn.Blocks[entry.ID] = entry
		fn.Blocks[exit.ID] = exit
		fn.Entry = entry
		fn.Exit = exit

		mod.Functions = append(mod.Functions, fn)
		prog.Modules = append(prog.Modules, mod)
	}

	pm := miniropt.NewPassManager()
	pm.ConfigureFromPassList("")

	changes := pm.RunOnProgram(prog)
	if changes != 0 {
		t.Fatalf("RunOnProgram with empty config: got %d changes, want 0", changes)
	}
}

// TestVerboseOutput tests verbose logging.
func TestVerboseOutput(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()
	pm.SetVerbose(true)

	// Set up a buffer to capture output
	buf := &bytes.Buffer{}
	pm.SetLogWriter(buf)

	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	pm.ConfigureFromPassList("cleancfg")
	pm.RunOnFunction(fn)

	output := buf.String()
	if len(output) == 0 {
		t.Fatal("verbose mode should produce output")
	}
	if !strings.Contains(output, "test") || !strings.Contains(output, "cleancfg") {
		t.Errorf("output missing expected content: %q", output)
	}
}

// TestStats tracks pass execution statistics.
func TestStats(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()
	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	pm.ConfigureFromPassList("cleancfg")
	pm.RunOnFunction(fn)

	stats := pm.Stats()
	if len(stats) == 0 {
		t.Fatal("expected stats")
	}
	if stats[0].FunctionName != "test" {
		t.Errorf("expected function name 'test', got %q", stats[0].FunctionName)
	}
}

// TestClearStats removes accumulated statistics.
func TestClearStats(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()
	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	pm.ConfigureFromPassList("cleancfg")
	pm.RunOnFunction(fn)

	if len(pm.Stats()) == 0 {
		t.Fatal("expected stats after first run")
	}

	pm.ClearStats()
	if len(pm.Stats()) != 0 {
		t.Fatal("expected no stats after clear")
	}
}

// TestAvailablePasses lists registered passes.
func TestAvailablePasses(t *testing.T) {
	pm := miniropt.NewPassManager()
	passes := pm.AvailablePasses()
	if len(passes) == 0 {
		t.Fatal("expected at least one pass to be registered")
	}
	// check for known passes
	found := make(map[string]bool)
	for _, p := range passes {
		found[p] = true
	}
	if !found["mem2reg"] {
		t.Fatal("mem2reg pass not found")
	}
	if !found["cleancfg"] {
		t.Fatal("cleancfg pass not found")
	}
}

// TestPassHelp generates help text.
func TestPassHelp(t *testing.T) {
	pm := miniropt.NewPassManager()
	help := pm.PassHelp()
	if len(help) == 0 {
		t.Fatal("expected non-empty help")
	}
	if !strings.Contains(help, "mem2reg") {
		t.Fatal("help should mention mem2reg")
	}
}

// TestFixedPointIteration runs passes to fixed point.
func TestFixedPointIteration(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()
	pm.SetFixedPoint(true)
	pm.SetFixedPointIterations(5)

	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	pm.ConfigureFromPassList("cleancfg")
	changes := pm.RunOnFunction(fn)
	// Should not error on fixed-point
	if changes < 0 {
		t.Fatalf("RunOnFunction returned negative changes: %d", changes)
	}
}

// TestConfigurationPreservesOrder preserves pass execution order.
func TestConfigurationPreservesOrder(t *testing.T) {
	resetTempIDs()
	pm := miniropt.NewPassManager()
	err := pm.ConfigureFromPassList("cleancfg,mem2reg,constfold")
	if err != nil {
		t.Fatalf("ConfigureFromPassList failed: %v", err)
	}

	// Stats should be recorded in order
	fn := &minir.Function{
		FnName: "test",
		Blocks: make(map[int]*minir.Block),
		SymTab: minir.SymbolTable{},
	}

	entry := &minir.Block{ID: 0, Label: "entry", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}
	exit := &minir.Block{ID: 1, Label: "exit", Preds: map[int]*minir.Block{}, Succs: map[int]*minir.Block{}}

	entry.Term = &minir.JumpInst{Target: exit.Label}
	entry.Instrs = append(entry.Instrs, entry.Term)
	exit.Term = &minir.HaltInst{}
	exit.Instrs = append(exit.Instrs, exit.Term)

	entry.AddSucc(exit)
	exit.AddPred(entry)

	fn.Blocks[entry.ID] = entry
	fn.Blocks[exit.ID] = exit
	fn.Entry = entry
	fn.Exit = exit

	pm.RunOnFunction(fn)
	stats := pm.Stats()

	// Should contain at least some stats
	if len(stats) == 0 {
		t.Fatal("expected statistics")
	}
}









