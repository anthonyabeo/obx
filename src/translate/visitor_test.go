package translate

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/sema"
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	p := &parser.Parser{}
	p.InitParser(lex)
	ob := p.Oberon()

	scp := sema.NewScope(sema.Global, "Main")

	sema_vst := &sema.Visitor{}
	sema_vst.InitSemaVisitor(ob, scp)
	sema_vst.VisitModule("Main")

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	fmt.Println(Main)
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	p := &parser.Parser{}
	p.InitParser(lex)
	ob := p.Oberon()

	scp := sema.NewScope(sema.Global, "Main")

	sema_vst := &sema.Visitor{}
	sema_vst.InitSemaVisitor(ob, scp)
	sema_vst.VisitModule("Main")

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	fmt.Println(Main)
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	p := &parser.Parser{}
	p.InitParser(lex)
	ob := p.Oberon()

	scp := sema.NewScope(sema.Global, "Main")

	sema_vst := &sema.Visitor{}
	sema_vst.InitSemaVisitor(ob, scp)
	sema_vst.VisitModule("Main")

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	fmt.Println(Main)
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	p := &parser.Parser{}
	p.InitParser(lex)
	ob := p.Oberon()

	scp := sema.NewScope(sema.Global, "Main")

	sema_vst := &sema.Visitor{}
	sema_vst.InitSemaVisitor(ob, scp)
	sema_vst.VisitModule("Main")

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	fmt.Println(Main)
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
	lex := &lexer.Lexer{}
	lex.InitLexer(file, []byte(input))

	p := &parser.Parser{}
	p.InitParser(lex)
	ob := p.Oberon()

	scp := sema.NewScope(sema.Global, "Main")

	sema_vst := &sema.Visitor{}
	sema_vst.InitSemaVisitor(ob, scp)
	sema_vst.VisitModule("Main")

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	fmt.Println(Main)
}
