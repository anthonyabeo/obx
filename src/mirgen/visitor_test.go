package mirgen

import (
	"github.com/anthonyabeo/obx/src/meer"
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	a := meer.CreateIdent("a")
	b := meer.CreateIdent("b")
	res := meer.CreateIdent("res")
	assert := meer.CreateIdent("assert")
	args := meer.CreateBinaryOp(meer.Eq, res, &meer.IntegerConst{Value: 7})

	tests := []meer.Instruction{
		meer.NewLabel("Main"),
		meer.CreateAssign(&meer.IntegerConst{Value: 4}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 3}, b),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, b), res),
		meer.CreateProcCall(assert, []meer.Expression{args}),
	}

	Main := program.Units["Main"]

	if len(Main.Inst) != len(tests) {
		t.Errorf("inaccurate number of instructions. Expected '%d', Got '%d'",
			len(tests), len(Main.Inst))
	}

	for idx, i := range Main.Inst {
		switch inst := i.(type) {
		case *meer.Label:
			testLabel(t, inst.Name, tests[idx].(*meer.Label).Name)
		case *meer.AssignInst:
			exp := tests[idx].(*meer.AssignInst)
			testAssign(t, inst, exp.Dst.String(), exp.Value.String())
		}
	}

}

func testLabel(t *testing.T, rcv, exp string) {
	if rcv != exp {
		t.Errorf("Expected Label with name '%s', got '%s' instead", exp, rcv)
	}
}

func testAssign(t *testing.T, inst *meer.AssignInst, expDst, expValue string) {
	if inst.Value.String() != expValue {
		t.Errorf("Expected value in assignment to be '%s', got '%s' instead",
			expValue, inst.Value.String())
	}

	if inst.Dst.String() != expDst {
		t.Errorf("Expected destination in assignment to be '%s', got '%s' instead",
			expDst, inst.Dst.String())
	}

}
