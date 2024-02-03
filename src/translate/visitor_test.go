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

	cgen := NewVisitor(ob, scp)
	module := cgen.VisitModule("Main")

	Main := module.GetFunction("main")
	blks := Main.Blocks()
	for name, BB := range blks {
		fmt.Print(fmt.Sprintf("%s:\n\t", name))
		l := BB.Instr()
		for inst := l.Front(); inst != nil; {
			if inst.Next() != nil {
				fmt.Print(fmt.Sprintf("%s\n\t", inst.Value))
			} else {
				fmt.Print(fmt.Sprintf("%s\n", inst.Value))
			}

			inst = inst.Next()
		}
	}

	fmt.Println("foo")
}
