package mirgen

import (
	"testing"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/meer"
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	loop := meer.NewLabel("loop")
	ifThen := meer.NewLabel("if.then")
	ifElse := meer.NewLabel("if.else")
	a := meer.CreateIdent("a")
	b := meer.CreateIdent("b")
	total := meer.CreateIdent("total")
	assert := meer.CreateIdent("assert")
	cond := meer.CreateCmpInst(meer.Lt, a, b)
	condBr := meer.CreateCondBrInst(cond, ifThen, ifElse)
	cont := meer.NewLabel("cont")
	args := meer.CreateBinaryOp(meer.Eq, total, &meer.IntegerConst{Value: 55})

	tests := []meer.Instruction{
		meer.NewLabel("Main"),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, total),
		loop,
		condBr,
		ifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, total, &meer.IntegerConst{Value: 1}), total),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, &meer.IntegerConst{Value: 1}), a),
		meer.CreateJmp(loop),
		ifElse,
		meer.CreateJmp(cont),
		cont,
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
		case *meer.CondBrInst:
			expect := tests[idx].(*meer.CondBrInst)
			testCondBr(t, inst, expect.Op, expect.IfTrue.String(), expect.IfFalse.String(), expect.Cond.String())
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

func testCondBr(t *testing.T, inst *meer.CondBrInst, pred meer.Opcode, expTrueLabel, expFalseLabel, cond string) {
	if inst.Op != pred {
		t.Errorf("expected logical operator to be %s, Got %s", pred, inst.Op)
	}

	if inst.Cond.String() != cond {
		t.Errorf("expected logical condition to be '%s', Got '%s'", cond, inst.Cond.String())
	}

	testLabel(t, inst.IfTrue.String(), expTrueLabel)
	testLabel(t, inst.IfFalse.String(), expFalseLabel)
}

func testBinaryOp(t *testing.T, expr *meer.BinaryOp, left, right string, op meer.Opcode) {
	if expr.Op != op {
		t.Errorf("expected operator of binary operation to be '%s', Got '%s'", op, expr.Op)
	}

	if expr.X.String() != left {
		t.Errorf("expected LHS of binary operation to be '%s', Got '%s'", left, expr.X.String())
	}

	if expr.Y.String() != right {
		t.Errorf("expected RHS of binary operation to be '%s', Got '%s'", right, expr.Y.String())
	}
}
