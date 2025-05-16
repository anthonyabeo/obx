package parser

import (
	"testing"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/scan"
)

func TestParseCaseStatement(t *testing.T) {
	input := `module Main
proc ReadIdentifier()
end ReadIdentifier

proc ReadNumber()
end ReadNumber

proc ReadString()
end ReadString

proc SpecialCharacter()
end SpecialCharacter
begin
   case ch of
     "A" .. "Z": ReadIdentifier()
   | "0" .. "9": ReadNumber()
   | "'", '"': ReadString()
   else SpecialCharacter()
   end
end Main`

	//table := verify.NewSymbolTable(nil, "Main")
	//table.Insert(verify.NewProcedureSymbol("ReadIdentifier", ast.Unexported))
	//table.Insert(verify.NewProcedureSymbol("ReadNumber", ast.Unexported))
	//table.Insert(verify.NewProcedureSymbol("ReadString", ast.Unexported))
	//table.Insert(verify.NewProcedureSymbol("SpecialCharacter", ast.Unexported))

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

}

func TestParseOberonMinimalProgram(t *testing.T) {
	input := `
module Main
	proc fib(n : integer): integer
		var a, b: integer 

  	begin
		if (n = 0) or (n = 1) then
			return n
		else
		  a := fib(n - 1)
		  b := fib(n - 2)
		  return a + b
		end
  	end fib

begin
	res := fib(21)
  	assert(res = 10946)
end Main
`

	//table := verify.NewSymbolTable(verify.Global, "Main")
	//table.Insert(verify.NewProcedureSymbol("fib", ast.Unexported))

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != 1 {
		t.Errorf("expected 1 declaration in '%v' module, found %d",
			main.BName, len(main.DeclSeq))
	}

	if len(main.StmtSeq) != 2 {
		t.Errorf("expected 2 statements in '%s' module, found %d",
			main.BName, len(main.StmtSeq))
	}
}

func TestParseWhileStatement(t *testing.T) {
	input := `
module Main
begin
	while i > 0 do 
		i := i div 2; 
		k := k + 1 
	end

	while m > n do
		m := m - n
	elsif n > m do
		n := n - m
	end
end Main
`
	//table := verify.NewSymbolTable(verify.Global, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != 2 {
		t.Errorf("expected 2 statements in '%s' module, found %d",
			main.BName, len(main.StmtSeq))
	}
}

func TestParseExpressions(t *testing.T) {
	input := `
module Main
	type CenterTree = record
		x,y: integer 
	end
begin
	phi := 1991                   
	phi := i div 3                
	phi := ~p or q                
	phi := (i+j) * (i-j)          
	phi := s - {8, 9, 13}         
	phi := i + x                  
	phi := a[i+j] * a[i-j]        
	phi := (0<=i) & (i<100)       
	phi := t.key = 0              
	phi := k in {i..j-1}          
	phi := w[i].name <= "John"   
	phi := t is CenterTree      
	phi := t{CenterTree}.subnode
	phi := t.left.right 
	phi := w[3].name[i]
	phi := t{CenterTree}
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	tests := []string{
		"1991",
		"i div 3",
		"~p or q",
		"i + j * i - j",
		"s - {8, 9, 13}",
		"i + x",
		"a[i + j] * a[i - j]",
		"0 <= i & i < 100",
		"t.key = 0",
		"k in {i..j - 1}",
		"w[i].name <= John",
		"t is CenterTree",
		"t{CenterTree}.subnode",
		"t.left.right",
		"w[3].name[i]",
		"t{CenterTree}",
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), main.BName, len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		st := stmt.(*ast.AssignmentStmt)
		if st.RValue.String() != tests[i] {
			t.Errorf("Expected '%s', got '%s'", tests[i], st.RValue.String())
		}
	}
}

func TestParseProcedureCall(t *testing.T) {
	input := `
module Main
	proc WriteInt(i: integer)
	end WriteInt

	proc (t: CenterTree) Insert(s: array of char)
	end Insert
begin
	WriteInt(i*2+1)
	inc(w[k].count)
	t.Insert("John")
end Main
`
	//table := verify.NewSymbolTable(verify.Global, "Main")
	//table.Insert(verify.NewProcedureSymbol("WriteInt", ast.Unexported))
	//table.Insert(verify.NewProcedureSymbol("t.Insert", ast.Unexported))

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != 3 {
		t.Errorf("expected 3 statements in '%s' module, found %d",
			main.BName, len(main.StmtSeq))
	}

	tests := []struct {
		procName string
		args     []string
	}{
		{"WriteInt", []string{"i * 2 + 1"}},
		{"inc", []string{"w[k].count"}},
		{"t.Insert", []string{"John"}},
	}

	for idx, stmt := range main.StmtSeq {
		proc := stmt.(*ast.ProcedureCall)
		if proc.Callee.String() != tests[idx].procName {
			t.Errorf("expected procedure name '%s', got '%s'", tests[idx].procName, proc.Callee.String())
		}

		for i := 0; i < len(tests[idx].args); i++ {
			if proc.ActualParams[i].String() != tests[idx].args[i] {
				t.Errorf("argument number %d, %s (of procedure %s) does not match expected argument '%s'",
					i, proc.ActualParams[i].String(), tests[idx].procName, tests[idx].args[i])
			}
		}
	}
}

func TestParseAssignStmt(t *testing.T) {
	input := `
module Main
proc log2(x: real)
end log2

begin
	i := 0
	p := i = j
	x := i + 1
	k := log2(i+j)
	F := log2
	s := {2, 3, 5, 7, 11, 13}
	a[i] := (x+y) * (x-y)
	t.key := i
	w[i+1].name := "John"
	t := c
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")
	//table.Insert(verify.NewProcedureSymbol("log2", ast.Unexported))

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	tests := []struct {
		lvalue string
		rvalue string
	}{
		{"i", "0"},
		{"p", "i = j"},
		{"x", "i + 1"},
		{"k", "log2(i + j)"},
		{"F", "log2"},
		{"s", "{2, 3, 5, 7, 11, 13}"},
		{"a[i]", "x + y * x - y"},
		{"t.key", "i"},
		{"w[i + 1].name", "John"},
		{"t", "c"},
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), main.BName, len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		st := stmt.(*ast.AssignmentStmt)

		if st.LValue.String() != tests[i].lvalue {
			t.Errorf("Expected LValue '%s', got '%s'", tests[i].lvalue, st.LValue.String())
		}

		if st.RValue.String() != tests[i].rvalue {
			t.Errorf("Expected RValue '%s', got '%s'", tests[i].rvalue, st.RValue.String())
		}
	}
}

func TestParseImportDecl(t *testing.T) {
	input := `module Drawing
 import F := Fibonacci
        C := Collections(Figure)
		Foobar
		a.b.c.d
		D := a.b.c
		E := a.b.c.d(Bar)
		F := a.b.c.d(Bar, Baz)
		G := a.b.c.d(Foo Bar Baz)
		Out
end Drawing
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	tests := []string{
		"F := Fibonacci",
		"C := Collections(Figure)",
		"Foobar",
		"a.b.c.d",
		"D := a.b.c",
		"E := a.b.c.d(Bar)",
		"F := a.b.c.d(Bar, Baz)",
		"G := a.b.c.d(Foo, Bar, Baz)",
		"Out",
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.ImportList) != len(tests) {
		t.Errorf("expected %d import statements, found %d", len(tests), len(main.ImportList))
	}

	for i, imp := range main.ImportList {
		if tests[i] != imp.String() {
			t.Errorf("expected import %s, got %s instead", tests[i], imp.String())
		}
	}
}

func TestParseOOPExample(t *testing.T) {
	input := `module Drawing
 import F := Fibonacci
        C := Collections(Figure)

 type Figure* = pointer to record
                  position: record
                    x,y: integer
                  end
				 end
 proc (this: Figure) draw*() end

 type
    Circle* = pointer to record (Figure)
                 diameter: integer
			   end
    Square* = pointer to record (Figure)
                         width: integer end
 proc (this: Circle) draw*() end
 proc (this: Square) draw*() end

 var figures: C.Deque
      circle: Circle
      square: Square

 proc drawAll()
   type I = record(C.Iterator) count: integer end
   proc (var this: I) apply( in figure: Figure )
   begin
     figure.draw(); inc(this.count)
   end apply
   var i: I
 begin
   figures.forEach(i)
   assert(i.count = 2)
 end drawAll
begin
 figures := C.createDeque()
 new(circle)
 circle.position.x := F.calc(3)
 circle.position.y := F.calc(4)
 circle.diameter := 3
 figures.append(circle)
 new(square)
 square.position.x := F.calc(5)
 square.position.y := F.calc(6)
 square.width := 4
 figures.append(square)
 drawAll()
end Drawing
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()

	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	stmtTests := []string{
		"figures := C.createDeque()",
		"new(circle)",
		"circle.position.x := F.calc(3)",
		"circle.position.y := F.calc(4)",
		"circle.diameter := 3",
		"figures.append(circle)",
		"new(square)",
		"square.position.x := F.calc(5)",
		"square.position.y := F.calc(6)",
		"square.width := 4",
		"figures.append(square)",
		"drawAll()",
	}

	if len(stmtTests) != len(main.StmtSeq) {
		t.Errorf("expected %d statements, found %d", len(stmtTests), len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		if stmtTests[i] != stmt.String() {
			t.Errorf("expected statement '%s', got '%s'",
				stmtTests[i], stmt.String())
		}
	}

	Decls := []string{
		"Figure* = ^record{record{integer; integer}}",
		"(this: Figure) draw*()",
		"Circle* = ^record(Figure){integer}",
		"Square* = ^record(Figure){integer}",
		"(this: Circle) draw*()",
		"(this: Square) draw*()",
		"figures: C.Deque",
		"circle: Circle",
		"square: Square",
		"drawAll()",
	}

	for i, decl := range main.DeclSeq {
		switch d := decl.(type) {
		case *ast.VariableDecl:
			if Decls[i] != d.String() {
				t.Errorf("expected variable declaration '%s', got '%s'", Decls[i], d.String())
			}
		case *ast.TypeDecl:
			if Decls[i] != d.String() {
				t.Errorf("expected type declaration '%s', got '%s'", Decls[i], d.String())
			}
		case *ast.ProcedureDecl:
			if Decls[i] != d.Head.String() {
				t.Errorf("expected procedure declaration '%s', got '%s'", Decls[i], d.Head.String())
			}
		}
	}
}

func TestParseModuleWithVariableDecl(t *testing.T) {
	input := `
module Main
	var i, j, k: integer
		x, y: real
		p, q: bool
		s: set
		F: Function
		a: array 100 of real
		w: array 16 of record
			 name: array 32 of char
			 count: integer
		   end
		t, c: Tree
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()
	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != 8 {
		t.Errorf("expected 8 declarations in '%v' module, found %d",
			main.BName, len(main.DeclSeq))
	}

	var tests = []struct {
		declIndex  int
		identCount int
		declType   string
		identNames []string
	}{
		{0, 3, "integer", []string{"i", "j", "k"}},
		{1, 2, "real", []string{"x", "y"}},
		{2, 2, "bool", []string{"p", "q"}},
		{3, 1, "set", []string{"s"}},
		{4, 1, "Function", []string{"F"}},
		{5, 1, "[100]real", []string{"a"}},
		{6, 1, "[16]record{[32]char; integer}", []string{"w"}},
		{7, 2, "Tree", []string{"t", "c"}},
	}

	for _, tt := range tests {
		decl, ok := main.DeclSeq[tt.declIndex].(*ast.VariableDecl)
		if !ok {
			t.Errorf("expected variable declaration, got '%s'", main.DeclSeq[tt.declIndex])
		}

		if len(decl.IdentList) != tt.identCount {
			t.Errorf("expected %d identifiers in variable declaration, got %d", tt.identCount, len(decl.IdentList))
		}

		for i, ident := range decl.IdentList {
			if ident.Name != tt.identNames[i] {
				t.Errorf("expected identifier '%s', got '%s'", tt.identNames[i], ident.Name)
			}
		}

		if decl.Type.String() != tt.declType {
			t.Errorf("expected type '%s', got '%s'", tt.declType, decl.Type.String())
		}
	}

}

func TestParseTypeDeclaration(t *testing.T) {
	input := `
module Main
type a = array 10, N of integer
	 b = array of char
     c = [N][M] T
	 Table = array N of real
	 Tree = pointer to Node
	 Node = record
	   key: integer
	   left, right: Tree
	 end
	 CenterTree = pointer to CenterNode
	 CenterNode = record (Node)
		width: integer
	  	subnode: Tree
		end
	Function = procedure(x: integer): integer
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()
	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != 9 {
		t.Errorf("expected 9 type declarations in '%v' module, found %d",
			main.BName, len(main.DeclSeq))
	}

	tests := []struct {
		declIndex int
		declType  string
		declName  string
	}{
		{0, "[10, N]integer", "a"},
		{1, "[]char", "b"},
		{2, "[N][M]T", "c"},
		{3, "[N]real", "Table"},
		{4, "^Node", "Tree"},
		{5, "record{integer; Tree; Tree}", "Node"},
		{6, "^CenterNode", "CenterTree"},
		{7, "record(Node){integer; Tree}", "CenterNode"},
		{8, "procedure(x: integer): integer", "Function"},
	}

	for _, tt := range tests {
		decl, ok := main.DeclSeq[tt.declIndex].(*ast.TypeDecl)
		if !ok {
			t.Errorf("expected type declaration, got '%s'", main.DeclSeq[tt.declIndex])
		}

		if decl.DenotedType.String() != tt.declType {
			t.Errorf("expected type '%s', got '%s'", tt.declType, decl.DenotedType.String())
		}

		if decl.Name.Name != tt.declName {
			t.Errorf("expected type name '%s', got '%s'", tt.declName, decl.Name.Name)
		}
	}
}

func TestParseConstantDeclaration(t *testing.T) {
	input := `
module Main
const
	pi = 3.14159
	e = 2.718281828459
	phi = 1.6180339887
	zero = 0
	one = 1
	ten = 10
	oneHundred = 100
	oneThousand = 1000
	oneMillion = 1000000
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()
	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != 9 {
		t.Errorf("expected 8 constant declarations in '%v' module, found %d",
			main.BName, len(main.DeclSeq))
	}

	tests := []struct {
		declIndex int
		declName  string
		declValue string
	}{
		{0, "pi", "3.14159"},
		{1, "e", "2.718281828459"},
		{2, "phi", "1.6180339887"},
		{3, "zero", "0"},
		{4, "one", "1"},
		{5, "ten", "10"},
		{6, "oneHundred", "100"},
		{7, "oneThousand", "1000"},
		{8, "oneMillion", "1000000"},
	}

	for _, tt := range tests {
		decl, ok := main.DeclSeq[tt.declIndex].(*ast.ConstantDecl)
		if !ok {
			t.Errorf("expected constant declaration, got '%s'", main.DeclSeq[tt.declIndex])
		}

		if decl.Name.Name != tt.declName {
			t.Errorf("expected constant name '%s', got '%s'", tt.declName, decl.Name.Name)
		}

		if decl.Value.String() != tt.declValue {
			t.Errorf("expected constant value '%s', got '%s'", tt.declValue, decl.Value.String())
		}
	}
}

func TestParseExpression(t *testing.T) {
	input := `module Main
begin
	i := 1991
	i := i div 3
	i := ~p or q
	i := (i + j) * (i - j)
	i := s - {8, 9, 13}
	i := i + x
	i := a[i + j] * a[i - j]
	i := (0 <= i) & (i < 100)
	i := t.key = 0
	i := k in {i..j - 1}
	i := w[i].name <= John
	i := t is CenterTree
	i := a[i]
	i := w[3].name[i]
	i := t.left.right
	i := t{CenterTree}.subnode
	t{CenterTree}.subnode := t{CenterTree}.subnode
end Main
`
	//table := verify.NewSymbolTable(nil, "Main")

	lex := scan.Scan("test.obx", input)
	p := NewParser(lex, diagnostics.NewStdErrReporter(10) /*, table*/)
	unit := p.Parse()
	if p.err.ErrCount() > 0 {
		t.Error("found parse errors")
		for _, err := range p.err.Errors() {
			t.Log(err.Error())
		}
	}

	tests := []string{
		"1991",
		"i div 3",
		"~p or q",
		"i + j * i - j",
		"s - {8, 9, 13}",
		"i + x",
		"a[i + j] * a[i - j]",
		"0 <= i & i < 100",
		"t.key = 0",
		"k in {i..j - 1}",
		"w[i].name <= John",
		"t is CenterTree",
		"a[i]",
		"w[3].name[i]",
		"t.left.right",
		"t{CenterTree}.subnode",
		"t{CenterTree}.subnode",
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), main.BName, len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		st := stmt.(*ast.AssignmentStmt)
		if st.RValue.String() != tests[i] {
			t.Errorf("Expected '%s', got '%s'", tests[i], st.RValue.String())
		}
	}
}
