package parser

import "testing"

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

	lexer := &Lexer{}
	lexer.InitLexer([]byte(input))

	p := &Parser{}
	p.InitParser(lexer)

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

	lexer := &Lexer{}
	lexer.InitLexer([]byte(input))

	p := &Parser{}
	p.InitParser(lexer)

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