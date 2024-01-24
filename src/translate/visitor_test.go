package translate

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestILOCCodegenMinimalProgram(t *testing.T) {
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
	//if len(sema.errors) > 0 {
	//	t.Error("found semantic errors")
	//	for _, err := range sema.errors {
	//		t.Log(err.Error())
	//	}
	//}

	cgen := NewVisitor(ob, scp)
	cgen.VisitModule("Main")

	for _, instr := range cgen.Instr {
		fmt.Println(instr)
	}
}
