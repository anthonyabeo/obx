package sema

import (
	"testing"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestTypeCheckMinimalProgram(t *testing.T) {
	input := `
module Main
	var res: integer

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

	p := &parser.Parser{}
	p.InitParser(lex, scp)
	ob := p.Oberon()

	sema := &Visitor{}
	sema.InitSemaVisitor(ob, scope.Global)
	sema.VisitModule("Main")
	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}
