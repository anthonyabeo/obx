package parser

import (
	"testing"

	"github.com/anthonyabeo/obx/src/sema/scope"
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
	}

	if len(mainMod.DeclSeq) != 3 {
		t.Errorf("expected 1 declaration in '%v' module, found %d",
			mainMod.BeginName, len(mainMod.DeclSeq))
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
	}

	if len(mainMod.DeclSeq) != 1 {
		t.Errorf("expected 1 declaration in '%v' module, found %d",
			mainMod.BeginName, len(mainMod.DeclSeq))
	}

	if len(mainMod.StmtSeq) != 2 {
		t.Errorf("expected 2 statements in '%s' module, found %d",
			mainMod.BeginName, len(mainMod.StmtSeq))
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
	}

	if len(mainMod.StmtSeq) != 2 {
		t.Errorf("expected 2 statements in '%s' module, found %d",
			mainMod.BeginName, len(mainMod.StmtSeq))
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
	phi := t(CenterTree).subnode
	phi := t.left.right 
	phi := w[3].name[i]
<<<<<<< HEAD
	phi := t(CenterTree)
	
=======
	phi := t{CenterTree}
>>>>>>> 52ef4c4 (update TestParseExpressions)
end Main
`
	file := token.NewFile("test.obx", len([]byte(input)))
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
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
		"t(CenterTree).subnode",
		"t.left.right",
		"w[3].name[i]",
		"t(CenterTree)",
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
	}

	if len(mainMod.StmtSeq) != len(tests) {
		t.Errorf("expected %d statements in '%s' module, found %d",
			len(tests), mainMod.BeginName, len(mainMod.StmtSeq))
	}

	for i, stmt := range mainMod.StmtSeq {
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := &Parser{}
	p.InitParser(lex, scp)

	ob := p.Oberon()
	if len(p.errors) > 0 {
		t.Error("found parse errors")
		for _, err := range p.errors {
			t.Log(err)
		}
	}

	mainMod := ob.Program["Main"]
	if mainMod.BeginName.Name != mainMod.EndName.Name {
		t.Errorf("start module name, '%s' does not match end module name '%s'",
			mainMod.BeginName, mainMod.EndName)
	}

	if len(mainMod.StmtSeq) != 3 {
		t.Errorf("expected 3 statements in '%s' module, found %d",
			mainMod.BeginName, len(mainMod.StmtSeq))
	}

	tests := []struct {
		procName string
		args     []string
	}{
		{"WriteInt", []string{"i * 2 + 1"}},
		{"inc", []string{"w[k].count"}},
		{"t.Insert", []string{"John"}},
	}

	for idx, stmt := range mainMod.StmtSeq {
		proc := stmt.(*ast.ProcCall)
		if proc.Dsg.String() != tests[idx].procName {
			t.Errorf("expected procedure name '%s', got '%s'", tests[idx].procName, proc.Dsg.String())
		}

		for i := 0; i < len(tests[idx].args); i++ {
			if proc.ActualParams[i].String() != tests[idx].args[i] {
				t.Errorf("argument number %d, %s (of procedure %s) does not match expected argument '%s'",
					i, proc.ActualParams[i].String(), tests[idx].procName, tests[idx].args[i])
			}
		}
	}
}
