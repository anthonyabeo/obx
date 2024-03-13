package parser

import (
	"testing"

	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/token"
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

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()

	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

}

func TestParseTypeDeclaration(t *testing.T) {
	input := `
module Main
  type a = array 10, N of integer
  type b = array of char
  type c = [N][M] T
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	tests := []struct {
		dim    int
		elemTy string
	}{
		{2, "integer"},
		{0, "char"},
		{1, "[M]T"},
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.DeclSeq) != len(tests) {
		t.Errorf("expected 1 declaration in '%v' module, found %d",
			main.BName, len(main.DeclSeq))
	}

	for i, tt := range tests {
		td, ok := main.DeclSeq[i].(*ast.TypeDecl)
		if !ok {
			t.Errorf("expected type declaration, got '%s'", main.DeclSeq[i])
		}

		denoTy, ok := td.DenotedType.(*ast.ArrayType)
		if !ok {
			t.Errorf("'%s' is not an array type-expression", td.DenotedType)
		}

		if denoTy.LenList == nil && (tt.dim != 0) {
			t.Errorf("expected an array of dimension %d, got %d instead", tt.dim, len(denoTy.LenList.List))
		}

		if denoTy.LenList != nil && (tt.dim != len(denoTy.LenList.List)) {
			t.Errorf("expected an array of dimension %d, got %d instead", tt.dim, len(denoTy.LenList.List))
		}

		if tt.elemTy != denoTy.ElemType.String() {
			t.Errorf("expected an element type of  '%s', got '%s'", tt.elemTy, denoTy.ElemType.String())
		}
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

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
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

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
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
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
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
	if main.BName.Name != main.EName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), main.BName, len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		st := stmt.(*ast.AssignStmt)
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
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
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
		proc := stmt.(*ast.ProcCall)
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
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
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
	if main.BName.Name != main.EName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	if len(main.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), main.BName, len(main.StmtSeq))
	}

	for i, stmt := range main.StmtSeq {
		st := stmt.(*ast.AssignStmt)

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
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
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
	if main.BName.Name != main.EName.Name {
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

 var figures: C::Deque
      circle: Circle
      square: Square

 proc drawAll()
   type I = record(C::Iterator) count: integer end
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
 figures := C::createDeque()
 new(circle)
 circle.position.x := F::calc(3)
 circle.position.y := F::calc(4)
 circle.diameter := 3
 figures.append(circle)
 new(square)
 square.position.x := F::calc(5)
 square.position.y := F::calc(6)
 square.width := 4
 figures.append(square)
 drawAll()
end Drawing
`
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := NewParser(lex)
	unit := p.Parse()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	main := unit.(*ast.Module)
	if main.BName.Name != main.EName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			main.BName, main.EName)
	}

	stmtTests := []string{
		"figures := C::createDeque()",
		"new(circle)",
		"circle.position.x := F::calc(3)",
		"circle.position.y := F::calc(4)",
		"circle.diameter := 3",
		"figures.append(circle)",
		"new(square)",
		"square.position.x := F::calc(5)",
		"square.position.y := F::calc(6)",
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
		"figures: C::Deque",
		"circle: Circle",
		"square: Square",
		"drawAll()",
	}

	for i, decl := range main.DeclSeq {
		switch d := decl.(type) {
		case *ast.VarDecl:
			if Decls[i] != d.String() {
				t.Errorf("expected variable declaration '%s', got '%s'", Decls[i], d.String())
			}
		case *ast.TypeDecl:
			if Decls[i] != d.String() {
				t.Errorf("expected type declaration '%s', got '%s'", Decls[i], d.String())
			}
		case *ast.ProcDecl:
			if Decls[i] != d.Head.String() {
				t.Errorf("expected procedure declaration '%s', got '%s'", Decls[i], d.Head.String())
			}
		}
	}
}
