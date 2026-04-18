package parser

import (
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/src/support/compiler"
	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/diag/formatter"
	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestParseCaseStatement(t *testing.T) {
	input := []byte(`module Main
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
end Main`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)

	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

}

func TestParseOberonMinimalProgram(t *testing.T) {
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`
module Main
	type Tree = record end
	type CenterTree = record
		x,y: integer 
	end
	var t: Tree
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
	phi := a[i]
	phi := t(CenterTree).subnode
	phi := t.left.right 
	phi := w[3].name[i]
	phi := t(CenterTree)
	t(CenterTree).subnode := t(CenterTree).subnode
end Main
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
		"t(CenterTree).subnode",
		"t.left.right",
		"w[3].name[i]",
		"t(CenterTree)",
		"t(CenterTree).subnode",
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
	input := []byte(`
module Main
	type CenterTree = POINTER TO record
		x,y: integer 
	end

	proc WriteInt(i: integer)
	end WriteInt

	proc (t: CenterTree) Insert(s: array of char)
	end Insert
begin
	WriteInt(i*2+1)
	inc(w[k].count)
	t.Insert("John")
end Main
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`module Drawing
 import F := Fibonacci
        C := Collections(Figure)
		Foobar
		a.b.c.d
		D := a.b.c
		E := a.b.c.d(Bar)
		H := a.b.c.d(Bar, Baz)
		G := a.b.c.d(Foo, Bar, Baz)
		Out
end Drawing
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
	}

	tests := []string{
		"F := Fibonacci",
		"C := Collections(Figure)",
		"Foobar",
		"a.b.c.d",
		"D := a.b.c",
		"E := a.b.c.d(Bar)",
		"H := a.b.c.d(Bar, Baz)",
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
	input := []byte(`module Drawing
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
`)
	figures := ast.NewLexicalScope(ast.Global, "Figure")
	figures.Insert(ast.NewProcedureSymbol("calc", ast.Exported, nil, nil, ast.FunctionProcedureKind))

	collections := ast.NewLexicalScope(ast.Global, "Collections")
	collections.Insert(ast.NewTypeSymbol("Iterator", ast.Exported, &ast.RecordType{}))
	collections.Insert(ast.NewProcedureSymbol("createDeque", ast.Exported, nil, nil, ast.FunctionProcedureKind))

	env := ast.NewRecordScope(nil)
	env.Insert(ast.NewProcedureSymbol("forEach", ast.Exported, nil, nil, ast.ProperProcedureKind))
	env.Insert(ast.NewProcedureSymbol("append", ast.Exported, nil, nil, ast.ProperProcedureKind))
	collections.Insert(ast.NewTypeSymbol("Deque", ast.Exported, &ast.RecordType{Env: env}))

	envs := ast.NewEnv()
	envs.AddModuleScope("Fibonacci", figures)
	envs.AddModuleScope("F", figures)
	envs.AddModuleScope("Collections", collections)
	envs.AddModuleScope("C", collections)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), envs, 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)

	p := NewParser(ctx, filename, input)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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
		{8, "procedure(_: integer): integer", "Function"},
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
	input := []byte(`
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
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)

	p := NewParser(ctx, filename, input)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
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

func TestParseProcedureWithRecord(t *testing.T) {
	input := []byte(`
module Main
	proc WriteInt()
	end WriteInt

	type Tree = pointer to record
		key: integer
		left, right: Tree
	end
	type CenterTree = pointer to record (Tree)
		width: integer
		subnode: Tree
	end
	proc (t: Tree) Insert(node: Tree)
		var p, father: Tree
	begin
		p := t
		repeat 
			father := p
			if node.key = p.key then return end
			if node.key < p.key then
				p := p.left
			else
				p := p.right
			end
		until p = nil
		if node.key < father.key then
			father.left := node
		else
			father.right := node
		end
		node.left := nil
		node.right := nil
	end Insert
	proc (t: CenterTree) Insert(node: Tree)
	begin
		WriteInt(node(CenterTree).width)
		t.Insert^(node)
	end Insert

end Main
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)

	p := NewParser(ctx, filename, input)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
	}

	tests := []struct {
		declIndex int
		declType  string
		declName  string
	}{
		{0, "WriteInt()", "WriteInt"},
		{1, "^record{integer; Tree; Tree}", "Tree"},
		{2, "^record(Tree){integer; Tree}", "CenterTree"},
		{3, "(t: Tree) Insert(node: Tree)", "Insert"},
		{4, "(t: CenterTree) Insert(node: Tree)", "Insert"},
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != len(tests) {
		t.Errorf("expected %d declarations in '%v' module, found %d",
			len(tests), main.BName, len(main.DeclSeq))
	}

	for _, tt := range tests {
		switch decl := main.DeclSeq[tt.declIndex].(type) {
		case *ast.ProcedureDecl:
			if decl.Head.String() != tt.declType {
				t.Errorf("expected type '%s', got '%s'", tt.declType, decl.Head.String())
			}

			if decl.Head.Name.Name != tt.declName {
				t.Errorf("expected type name '%s', got '%s'", tt.declName, decl.Head.Name.Name)
			}
		case *ast.TypeDecl:
			if decl.DenotedType.String() != tt.declType {
				t.Errorf("expected type '%s', got '%s'", tt.declType, decl.DenotedType.String())
			}

			if decl.Name.Name != tt.declName {
				t.Errorf("expected type name '%s', got '%s'", tt.declName, decl.Name.Name)
			}

		default:
			t.Errorf("expected type or procedure declaration, got '%s'", main.DeclSeq[tt.declIndex])
		}
	}
}

func TestParseProcedure(t *testing.T) {
	input := []byte(`
module Main
 	proc Read(var ch: char)
	end Read

	proc Write(ch: char)
	end Write

	proc ReadInt(var x: integer)
		var i: integer; ch: char
	begin i := 0; Read(ch)
		while ("0" <= ch) & (ch <= "9") do
			i := 10*i + (ord(ch)-ord("0")); Read(ch)
		end
		x := i
	end ReadInt
	proc WriteInt(x: integer) 
		var i: integer; buf: [5]integer
	begin i := 0
		repeat buf[i] := x mod 10; x := x div 10; inc(i) 
		until x = 0
		repeat dec(i); Write(chr(buf[i] + ord("0"))) 
		until i = 0
	end WriteInt
	proc WriteString(s: []char)
		var i: integer
	begin i := 0
		while (i < len(s)) & (s[i] # 5) do Write(s[i]); inc(i) end
	end WriteString
	proc log2(x: integer): integer
		var y: integer 
	begin
		y := 0; 
		while x > 1 do 
			x := x div 2; 
			inc(y) 
		end
		return y
	end log2

end Main
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		t.Error("found parse errors")
		ctx.Reporter.Flush()
	}

	tests := []struct {
		declIndex int
		declType  string
		declName  string
	}{
		{0, "Read(var ch: char)", "Read"},
		{1, "Write(ch: char)", "Write"},
		{2, "ReadInt(var x: integer)", "ReadInt"},
		{3, "WriteInt(x: integer)", "WriteInt"},
		{4, "WriteString(s: []char)", "WriteString"},
		{5, "log2(x: integer): integer", "log2"},
	}

	main := unit.(*ast.Module)
	if main.BName != main.EName {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != len(tests) {
		t.Errorf("expected %d declarations in '%v' module, found %d",
			len(tests), main.BName, len(main.DeclSeq))
	}

	for _, tt := range tests {
		decl, ok := main.DeclSeq[tt.declIndex].(*ast.ProcedureDecl)
		if !ok {
			t.Errorf("expected procedure declaration, got '%s'", main.DeclSeq[tt.declIndex])
		}

		if decl.Head.String() != tt.declType {
			t.Errorf("expected type '%s', got '%s'", tt.declType, decl.Head.String())
		}

		if decl.Head.Name.Name != tt.declName {
			t.Errorf("expected type name '%s', got '%s'", tt.declName, decl.Head.Name.Name)
		}
	}
}

func TestParserWithSyntaxErrors(t *testing.T) {
	input := []byte(`
MODULE BadModule;

VAR x, y: INTEGER

BEGIN
  y := @;             
  while x > 0zx do     
    x := x div 2      
    inc(y);;
  end                 
EN BadModule.         
`)

	filename := "test.obx"
	mgr := source.NewSourceManager()
	ctx := compiler.New(filename, mgr, diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0))), ast.NewEnv(), 0)
	p := NewParser(ctx, filename, input)
	unit := p.Parse()

	if _, ok := unit.(*ast.Module); !ok {
		t.Fatal("Expected a partially parsed module, got nil")
	}

	// Check that errors were recorded
	diags := ctx.Reporter.Diagnostics()
	if len(diags) == 0 {
		t.Error("Expected diagnostics, got none")
	}

	// Assert specific diagnostics
	expectedMessages := []string{
		"invalid character",                       // y := @;
		"malformed number",                        // 0zx
		"trailing semicolon after last statement", // inc(y);;
		"is not a valid statement",
		"expected 'IDENTIFIER', found 'EOF'",
		"is not a valid statement",
		"expected 'end', found 'EOF'",
		"expected 'end', found 'EOF'",
		"expected 'IDENTIFIER', found 'EOF'",
	}

	found := make([]bool, len(expectedMessages))
	for _, d := range diags {
		for i, msg := range expectedMessages {
			if strings.Contains(d.Message, msg) {
				found[i] = true
			}
		}
	}
	for i, ok := range found {
		if !ok {
			t.Errorf("Expected diagnostic containing: %q", expectedMessages[i])
		}
	}
}

// TestNestedBlockComments verifies that the parser correctly handles block
// comments at all nesting depths and reports an error for unterminated ones.
func TestNestedBlockComments(t *testing.T) {
	newCtx := func(filename string) *compiler.Context {
		mgr := source.NewSourceManager()
		rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
		return compiler.New(filename, mgr, rep, ast.NewEnv(), 0)
	}

	t.Run("flat comment before module body", func(t *testing.T) {
		src := []byte(`MODULE M;
(* this is a flat block comment *)
VAR x: INTEGER;
BEGIN x := 1 END M.`)
		ctx := newCtx("flat.obx")
		p := NewParser(ctx, "flat.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors for flat comment")
		}
	})

	t.Run("singly nested comment", func(t *testing.T) {
		// (* outer (* inner *) outer *) — the first *) must NOT end the outer comment
		src := []byte(`MODULE M;
(* outer (* inner *) still outer *)
VAR x: INTEGER;
BEGIN x := 2 END M.`)
		ctx := newCtx("nested1.obx")
		p := NewParser(ctx, "nested1.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors for singly nested comment")
		}
	})

	t.Run("deeply nested comment (3 levels)", func(t *testing.T) {
		src := []byte(`MODULE M;
(* L1 (* L2 (* L3 *) back-L2 *) back-L1 *)
VAR x: INTEGER;
BEGIN x := 3 END M.`)
		ctx := newCtx("nested3.obx")
		p := NewParser(ctx, "nested3.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors for 3-level nested comment")
		}
	})

	t.Run("comment immediately followed by token (no newline)", func(t *testing.T) {
		// Regression: the old code fetched one extra token after *) and discarded it.
		src := []byte(`MODULE M; (* comment *) VAR x: INTEGER; BEGIN x := 4 END M.`)
		ctx := newCtx("inline.obx")
		p := NewParser(ctx, "inline.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors for inline comment (extra-fetch regression)")
		}
	})

	t.Run("nested comment immediately followed by token (no newline)", func(t *testing.T) {
		src := []byte(`MODULE M; (* a (* b *) c *) VAR x: INTEGER; BEGIN x := 5 END M.`)
		ctx := newCtx("inline_nested.obx")
		p := NewParser(ctx, "inline_nested.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors for inline nested comment")
		}
	})

	t.Run("unterminated block comment reports error", func(t *testing.T) {
		src := []byte(`MODULE M; (* unterminated...`)
		ctx := newCtx("unterm.obx")
		p := NewParser(ctx, "unterm.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() == 0 {
			t.Fatal("expected an error for unterminated block comment, got none")
		}
		found := false
		for _, d := range ctx.Reporter.Diagnostics() {
			if strings.Contains(d.Message, "unterminated block comment") {
				found = true
				break
			}
		}
		if !found {
			t.Fatal("expected 'unterminated block comment' diagnostic, not found")
		}
	})

	t.Run("unterminated nested block comment reports error", func(t *testing.T) {
		// The inner *) closes the inner comment but the outer is still open.
		src := []byte(`MODULE M; (* outer (* inner *) still open...`)
		ctx := newCtx("unterm_nested.obx")
		p := NewParser(ctx, "unterm_nested.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() == 0 {
			t.Fatal("expected an error for unterminated nested block comment, got none")
		}
	})
}

// TestSourceDirectives exercises the <* IF *> / <* ASSERT *> directives.
func TestSourceDirectives(t *testing.T) {
	newCtx := func(filename string) *compiler.Context {
		mgr := source.NewSourceManager()
		rep := diag.NewBufferedReporter(mgr, 25, diag.Stdout(formatter.NewTextFormatter(mgr, 0)))
		return compiler.New(filename, mgr, rep, ast.NewEnv(), 0)
	}

	// ── IF true → body is included ───────────────────────────────────────────
	t.Run("IF true includes body", func(t *testing.T) {
		src := []byte(`
module M
<* IF true THEN *>
var x: integer
<* END *>
begin
  x := 1
end M`)
		ctx := newCtx("if_true.obx")
		p := NewParser(ctx, "if_true.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		if len(m.DeclSeq) != 1 {
			t.Fatalf("expected 1 declaration, got %d", len(m.DeclSeq))
		}
	})

	// ── IF false → body is skipped, ELSE is taken ────────────────────────────
	t.Run("IF false takes ELSE", func(t *testing.T) {
		src := []byte(`
module M
<* IF false THEN *>
var x: integer
<* ELSE *>
var y: integer
<* END *>
begin
  y := 2
end M`)
		ctx := newCtx("if_else.obx")
		p := NewParser(ctx, "if_else.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		if len(m.DeclSeq) != 1 {
			t.Fatalf("expected 1 declaration, got %d", len(m.DeclSeq))
		}
		vd := m.DeclSeq[0].(*ast.VariableDecl)
		if vd.IdentList[0].Name != "y" {
			t.Fatalf("expected 'y', got %q", vd.IdentList[0].Name)
		}
	})

	// ── ELSIF chain selects correct arm ──────────────────────────────────────
	t.Run("ELSIF chain selects correct arm", func(t *testing.T) {
		src := []byte(`
module M
<* IF false THEN *>
var a: integer
<* ELSIF true THEN *>
var b: integer
<* ELSE *>
var c: integer
<* END *>
begin
  b := 3
end M`)
		ctx := newCtx("elsif.obx")
		p := NewParser(ctx, "elsif.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		if len(m.DeclSeq) != 1 {
			t.Fatalf("expected 1 declaration, got %d", len(m.DeclSeq))
		}
		vd := m.DeclSeq[0].(*ast.VariableDecl)
		if vd.IdentList[0].Name != "b" {
			t.Fatalf("expected 'b', got %q", vd.IdentList[0].Name)
		}
	})

	// ── no branch taken → nothing is included ───────────────────────────────
	t.Run("IF false no ELSE → no declarations", func(t *testing.T) {
		src := []byte(`
module M
<* IF false THEN *>
var x: integer
<* END *>
end M`)
		ctx := newCtx("if_false_noelse.obx")
		p := NewParser(ctx, "if_false_noelse.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		if len(m.DeclSeq) != 0 {
			t.Fatalf("expected 0 declarations, got %d", len(m.DeclSeq))
		}
	})

	// ── ctx.SetDirective flag ────────────────────────────────────────────────
	t.Run("SetDirective flag controls branch", func(t *testing.T) {
		src := []byte(`
module M
<* IF RISCV64 THEN *>
var reg: integer
<* ELSE *>
var reg: integer
<* END *>
end M`)
		ctx := newCtx("directive_flag.obx")
		ctx.SetDirective("RISCV64", true)
		p := NewParser(ctx, "directive_flag.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		if len(m.DeclSeq) != 1 {
			t.Fatalf("expected 1 declaration, got %d", len(m.DeclSeq))
		}
	})

	// ── CONST-backed condition (Option B) ────────────────────────────────────
	t.Run("CONST-backed condition", func(t *testing.T) {
		src := []byte(`
module M
const Version = 3
<* IF Version >= 3 THEN *>
var newFeature: integer
<* ELSE *>
var oldFeature: integer
<* END *>
end M`)
		ctx := newCtx("const_cond.obx")
		p := NewParser(ctx, "const_cond.obx", src)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("unexpected parse errors")
		}
		m := unit.(*ast.Module)
		// DeclSeq = CONST Version + VAR newFeature
		if len(m.DeclSeq) != 2 {
			t.Fatalf("expected 2 declarations, got %d", len(m.DeclSeq))
		}
		vd, ok := m.DeclSeq[1].(*ast.VariableDecl)
		if !ok {
			t.Fatalf("expected VariableDecl at index 1")
		}
		if vd.IdentList[0].Name != "newFeature" {
			t.Fatalf("expected 'newFeature', got %q", vd.IdentList[0].Name)
		}
	})

	// ── ASSERT passes silently ───────────────────────────────────────────────
	t.Run("ASSERT true is silent", func(t *testing.T) {
		src := []byte(`
module M
<* ASSERT true, "should not fire" *>
end M`)
		ctx := newCtx("assert_pass.obx")
		p := NewParser(ctx, "assert_pass.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			t.Fatal("expected no diagnostics for passing ASSERT")
		}
	})

	// ── ASSERT false emits the required message ──────────────────────────────
	t.Run("ASSERT false emits diagnostic", func(t *testing.T) {
		src := []byte(`
module M
<* ASSERT false, "boom" *>
end M`)
		ctx := newCtx("assert_fail.obx")
		p := NewParser(ctx, "assert_fail.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() == 0 {
			t.Fatal("expected a diagnostic from failing ASSERT, got none")
		}
		found := false
		for _, d := range ctx.Reporter.Diagnostics() {
			if strings.Contains(d.Message, "boom") {
				found = true
				break
			}
		}
		if !found {
			t.Fatal("expected diagnostic containing 'boom', not found")
		}
	})

	// ── ASSERT missing message is an error ───────────────────────────────────
	t.Run("ASSERT missing message is an error", func(t *testing.T) {
		src := []byte(`
module M
<* ASSERT true *>
end M`)
		ctx := newCtx("assert_no_msg.obx")
		p := NewParser(ctx, "assert_no_msg.obx", src)
		p.Parse()
		if ctx.Reporter.ErrorCount() == 0 {
			t.Fatal("expected a diagnostic for missing ASSERT message, got none")
		}
	})
}

