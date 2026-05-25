package mir

import (
	"testing"
)

// TestFormatFunction tests the pretty-printing of a simple function.
func TestFormatFunction(t *testing.T) {
	// Create a simple function: func add(x i32, y i32) -> i32
	i32Type := NewScalarType("i32", 4)
	fn := NewFunction("add", i32Type)

	// Add parameters
	fn.AddParam(&Param{Name: "x", Type: i32Type})
	fn.AddParam(&Param{Name: "y", Type: i32Type})

	// Add locals
	fn.AddLocal(&Local{Name: "result", Type: i32Type})

	// Create entry block
	entry := NewBlock(0, "entry")

	// Create some registers
	x := NewRegister("x", VirtualReg, i32Type)
	y := NewRegister("y", VirtualReg, i32Type)
	result := NewRegister("t0", VirtualReg, i32Type)

	// Add instructions: result = add x, y
	addInstr := &BinaryInstr{
		Dst:   result,
		Op:    "add",
		Left:  x,
		Right: y,
	}
	entry.AddInstr(addInstr)

	// Add terminator: ret result
	retInstr := &ReturnInstr{Value: result}
	entry.SetTerminator(retInstr)

	fn.AddBlock(entry)
	fn.SetEntry(entry)
	fn.SetExit(entry)

	// Format and verify output contains expected elements
	output := FormatFunction(fn)
	t.Logf("Formatted function:\n%s", output)

	if output == "" {
		t.Fatalf("expected non-empty formatted output")
	}

	// Verify key elements are present
	if !contains(output, "func add") {
		t.Errorf("output missing 'func add'")
	}
	if !contains(output, "entry:") {
		t.Errorf("output missing 'entry:' label")
	}
	if !contains(output, "add") {
		t.Errorf("output missing 'add' instruction")
	}
	if !contains(output, "ret") {
		t.Errorf("output missing 'ret' instruction")
	}
}

// TestFormatModule tests the pretty-printing of a module.
func TestFormatModule(t *testing.T) {
	m := NewModule("test")

	// Add a global
	i32Type := NewScalarType("i32", 4)
	g := NewGlobalDecl("counter", i32Type, InternalLinkage, NewImmediate(0, i32Type))
	m.AddGlobal(g)

	// Add an external
	e := NewExternDecl("printf", nil)
	m.AddExtern(e)

	// Add a simple function
	fn := NewFunction("main", i32Type)
	entry := NewBlock(0, "entry")
	retInstr := &ReturnInstr{Value: NewImmediate(0, i32Type)}
	entry.SetTerminator(retInstr)
	fn.AddBlock(entry)
	fn.SetEntry(entry)
	fn.SetExit(entry)
	m.AddFunction(fn)

	output := FormatModule(m)
	t.Logf("Formatted module:\n%s", output)

	if output == "" {
		t.Fatalf("expected non-empty formatted output")
	}

	// Verify key elements
	if !contains(output, "module test") {
		t.Errorf("output missing 'module test'")
	}
	if !contains(output, "global") {
		t.Errorf("output missing 'global' declaration")
	}
	if !contains(output, "declare") {
		t.Errorf("output missing 'declare' for extern")
	}
}

// TestOperandString tests the OperandString formatter.
func TestOperandString(t *testing.T) {
	i32Type := NewScalarType("i32", 4)

	tests := []struct {
		name     string
		operand  Operand
		expected string
	}{
		{
			"Register",
			NewRegister("r0", VirtualReg, i32Type),
			"r0:i32",
		},
		{
			"Symbol",
			NewSymbol("@myvar", i32Type),
			"@@myvar:i32",
		},
		{
			"Immediate",
			NewImmediate(42, i32Type),
			"42:i32",
		},
		{
			"Memory simple",
			NewMemory(NewRegister("r0", VirtualReg, i32Type), nil, i32Type),
			"[r0:i32]",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := OperandString(tt.operand)
			if got != tt.expected {
				t.Errorf("OperandString(%v) = %q, want %q", tt.operand, got, tt.expected)
			}
		})
	}
}

// TestFormatInstr tests instruction formatting.
func TestFormatInstr(t *testing.T) {
	i32Type := NewScalarType("i32", 4)

	x := NewRegister("x", VirtualReg, i32Type)
	y := NewRegister("y", VirtualReg, i32Type)
	result := NewRegister("t0", VirtualReg, i32Type)

	tests := []struct {
		name     string
		instr    Instr
		contains string
	}{
		{
			"BinaryInstr",
			&BinaryInstr{Dst: result, Op: "add", Left: x, Right: y},
			"add",
		},
		{
			"LoadInstr",
			&LoadInstr{Dst: result, Addr: x},
			"load",
		},
		{
			"StoreInstr",
			&StoreInstr{Addr: x, Value: y},
			"store",
		},
		{
			"JumpInstr",
			&JumpInstr{Target: "loop"},
			"loop",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := FormatInstr(tt.instr)
			if !contains(got, tt.contains) {
				t.Errorf("FormatInstr(%T) = %q, missing %q", tt.instr, got, tt.contains)
			}
		})
	}
}

// Helper function to check if string contains substring
func contains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

