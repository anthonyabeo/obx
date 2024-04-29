package translate

import (
	"testing"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestIRCodegenMinimalProgram(t *testing.T) {
	input := `
module Main
	var a, b, res: integer

begin
	a := 4
	b := 3
	res := a + b
    assert(res = 7)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenBasicWhileLoop(t *testing.T) {
	input := `
module Main
	var a, b, total: integer

begin
	a := 0
    b := 10
	total := 0

	while a < b do
		total := total + 1
		a := a + 1
	end

    assert(total = 55)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenWhileLoopWithSingleElseIfBranch(t *testing.T) {
	input := `
module Main
	var m, n, gcd: integer

begin
	m := 24
    n := 48
	gcd := 0
	
	while m > n do
		m := m + n
    elsif n > m do
		n := n + m
	end

    assert(gcd = 55)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenWhileLoopWithTwoElseIfBranch(t *testing.T) {
	input := `
module Main
	var m, n, gcd: integer

begin
	m := 24
    n := 48
	gcd := 0
	
	while m > n do
		m := m + n
    elsif n > m do
		n := n + m
	elsif m # n do
		n := n + m
	end

    assert(gcd = 55)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenRepeatStmt(t *testing.T) {
	input := `
module Main
	var a, b, total: integer

begin
	a := 0
    b := 10
	total := 0
	
	repeat
		total := total + 1
		a := a + 1
	until a >= b

    assert(total = 55)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenIfThenElse(t *testing.T) {
	input := `
module Main
	var a, b, max: integer

begin
	a := 5
    b := 10
	
	if a > b then
		max := a
	else
		max := b
	end

    assert(max = 10)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenIfThenElsifElse(t *testing.T) {
	input := `
module Main
	var a, b, max: integer

begin
	a := 5
    b := 10
	
	if a > b then
		max := a
	elsif a = b then
		max := 15
	else
		max := b
	end

    assert(max = 10)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}

func TestIRCodegenIfThenElsifWithNoElse(t *testing.T) {
	input := `
module Main
	var a, b, max: integer

begin
	a := 5
    b := 10
	
	if a > b then
		max := a
	elsif a = b then
		max := 15
	elsif a # b then
		max := 42
	end

    assert(max = 10)
end Main
`

	file := token.NewFile("test.obx", len([]byte(input)))
	lex := lexer.NewLexer(file, []byte(input))

	errReporter := diagnostics.NewStdErrReporter(10)
	p := parser.NewParser(lex, errReporter)
	unit := p.Parse()

	obx := ast.NewOberon()

	scopes := map[string]scope.Scope{}
	for _, unit := range obx.Units() {
		scopes[unit.Name()] = nil
	}

	s := sema.NewVisitor(scopes, errReporter)
	unit.Accept(s)

	cgen := NewVisitor(scopes)
	unit.Accept(cgen)
}
