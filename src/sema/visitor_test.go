package sema

import (
	"testing"

<<<<<<< HEAD
	"github.com/anthonyabeo/obx/src/sema/scope"
=======
	"github.com/anthonyabeo/obx/src/syntax/ast"
>>>>>>> 422f461 (Update sema.VisitModule)
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
	lex := lexer.NewLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := parser.NewParser(lex, scp)
	unit := p.Parse()

<<<<<<< HEAD
	sema := NewVisitor(scope.Global)
=======
	obx := ast.NewOberon()
	sema := NewVisitor(obx)
>>>>>>> 422f461 (Update sema.VisitModule)
	unit.Accept(sema)

	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}

func TestTypeCheckEnumType(t *testing.T) {
	input := `
module Main
	var primary: (red, green, blue)

begin
	primary := red
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := parser.NewParser(lex, scp)
	unit := p.Parse()

<<<<<<< HEAD
	sema := NewVisitor(scope.Global)
=======
	obx := ast.NewOberon()
	sema := NewVisitor(obx)
>>>>>>> 422f461 (Update sema.VisitModule)
	unit.Accept(sema)
	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}

func TestTypeCheckRecordType(t *testing.T) {
	input := `
module Main
	const foo = 255
	var employee: record
	  name, firstname: array 32 of char
	  age: integer
	  salary: real
	end

end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	scp := scope.NewScope(scope.Global, "Main")

	p := parser.NewParser(lex, scp)
	unit := p.Parse()

<<<<<<< HEAD
	sema := NewVisitor(scope.Global)
=======
	obx := ast.NewOberon()
	sema := NewVisitor(obx)
>>>>>>> 422f461 (Update sema.VisitModule)
	unit.Accept(sema)
	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}
