package sema

import (
	"testing"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
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

	p := parser.NewParser(lex)
	unit := p.Parse()

	obx := ast.NewOberon()
	scopes := map[string]*scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	sema := NewVisitor(scopes)
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

	p := parser.NewParser(lex)
	unit := p.Parse()

	obx := ast.NewOberon()
	scopes := map[string]*scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	sema := NewVisitor(scopes)
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

	p := parser.NewParser(lex)
	unit := p.Parse()

	obx := ast.NewOberon()
	scopes := map[string]*scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	sema := NewVisitor(scopes)
	unit.Accept(sema)
	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}

func TestTypeCheckExpressions(t *testing.T) {
	input := `
module Main
	type Person = record
		age: integer
		name: array 10 of char
	end

	type a = array 10 of integer
		 w = array 10 of Person

	type CenterTree = record
		key: integer
	end

	var i, j, x, k: integer
		b, p, q: bool
		s: set
		t, u: CenterTree
		c: char

begin
	i := 1991
	i := i div 3
	b := ~p or q
	i := (i+j) * (i-j)
	s := s - {8, 9, 13}
	i := i + x
	i := a[i+j] * a[i-j]
	b := (0<=i) & (i<100)
	b := t.key = 0
	b := k in {i..j-1}
	b := w[i].age <= 15
	c := w[3].name[i]
	b := t is CenterTree
	i := t{CenterTree}.key
	u := t{CenterTree}
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	p := parser.NewParser(lex)
	unit := p.Parse()

	obx := ast.NewOberon()
	scopes := map[string]*scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	sema := NewVisitor(scopes)
	unit.Accept(sema)
	if len(sema.errors) > 0 {
		t.Error("found semantic errors")
		for _, err := range sema.errors {
			t.Log(err.Error())
		}
	}
}
