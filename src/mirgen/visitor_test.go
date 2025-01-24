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

	t0 := meer.CreateIdent("t0", nil)
	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	res := meer.CreateIdent("res", nil)
	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateCmpOp(meer.Eq, res, &meer.IntegerConst{Value: 7}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 4}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 3}, b),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, b, nil), res),

		meer.CreateAssign(args, t0),
		meer.CreateProcCall(assert, []meer.Expression{t0}),
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
	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	total := meer.CreateIdent("total", nil)
	assert := meer.CreateIdent("assert", nil)
	cond := meer.CreateCmpOp(meer.Lt, a, b, nil)
	condBr := meer.CreateCondBrInst(t0, ifThen, ifElse)
	cont := meer.NewLabel("cont")
	args := meer.CreateCmpOp(meer.Eq, total, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, total),
		loop,
		meer.CreateAssign(cond, t0),
		condBr,
		ifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, total, &meer.IntegerConst{Value: 1}, nil), total),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, &meer.IntegerConst{Value: 1}, nil), a),
		meer.CreateJmp(loop),
		ifElse,
		meer.CreateJmp(cont),
		cont,
		meer.CreateAssign(args, t1),
		meer.CreateProcCall(assert, []meer.Expression{t1}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	m := meer.CreateIdent("m", nil)
	n := meer.CreateIdent("n", nil)
	gcd := meer.CreateIdent("gcd", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)

	loop := meer.NewLabel("loop")
	ifThen := meer.NewLabel("if.then")
	ifElse := meer.NewLabel("if.else")
	ElifThen := meer.NewLabel("elif.then.0")
	ElifElse := meer.NewLabel("elif.else.0")

	cont := meer.NewLabel("cont")

	cond := meer.CreateCmpOp(meer.Gt, m, n, nil)
	//condBr := meer.CreateCondBrInst(cond, ifThen, ifElse)

	cond1 := meer.CreateCmpOp(meer.Gt, n, m, nil)

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, gcd, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),
		meer.CreateAssign(&meer.IntegerConst{Value: 24}, m),
		meer.CreateAssign(&meer.IntegerConst{Value: 48}, n),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, gcd),

		loop,
		meer.CreateAssign(cond, t0),
		meer.CreateCondBrInst(t0, ifThen, ifElse),

		ifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, m, n, nil), m),
		meer.CreateJmp(loop),

		ifElse,
		meer.CreateAssign(cond1, t1),
		meer.CreateCondBrInst(t1, ElifThen, ElifElse),

		ElifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, n, m, nil), n),
		meer.CreateJmp(loop),

		ElifElse,
		meer.CreateJmp(cont),

		cont,
		meer.CreateAssign(args, t2),
		meer.CreateProcCall(assert, []meer.Expression{t2}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	m := meer.CreateIdent("m", nil)
	n := meer.CreateIdent("n", nil)
	gcd := meer.CreateIdent("gcd", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)
	t3 := meer.CreateIdent("t3", nil)

	loop := meer.NewLabel("loop")
	ifThen := meer.NewLabel("if.then")
	ifElse := meer.NewLabel("if.else")
	ElifThen := meer.NewLabel("elif.then.0")
	ElifElse := meer.NewLabel("elif.else.0")
	ElifThen1 := meer.NewLabel("elif.then.1")
	ElifElse1 := meer.NewLabel("elif.else.1")
	cont := meer.NewLabel("cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, gcd, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 24}, m),
		meer.CreateAssign(&meer.IntegerConst{Value: 48}, n),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, gcd),

		loop,
		meer.CreateAssign(meer.CreateCmpOp(meer.Gt, m, n, nil), t0),
		meer.CreateCondBrInst(t0, ifThen, ifElse),

		ifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, m, n, nil), m),
		meer.CreateJmp(loop),

		ifElse,
		meer.CreateAssign(meer.CreateCmpOp(meer.Gt, n, m, nil), t1),
		meer.CreateCondBrInst(t1, ElifThen, ElifElse),

		ElifThen,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, n, m, nil), n),
		meer.CreateJmp(loop),

		ElifElse,
		meer.CreateAssign(meer.CreateCmpOp(meer.Ne, m, n, nil), t2),
		meer.CreateCondBrInst(t2, ElifThen1, ElifElse1),

		ElifThen1,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, n, m, nil), n),
		meer.CreateJmp(loop),

		ElifElse1,
		meer.CreateJmp(cont),

		cont,
		meer.CreateAssign(args, t3),
		meer.CreateProcCall(assert, []meer.Expression{t3}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	total := meer.CreateIdent("total", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)

	body := meer.NewLabel("repeat.body")
	cont := meer.NewLabel("cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, total, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 0}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, total),

		body,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, total, &meer.IntegerConst{Value: 1}, nil), total),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, &meer.IntegerConst{Value: 1}, nil), a),
		meer.CreateAssign(meer.CreateCmpOp(meer.Ge, a, b, nil), t0),
		meer.CreateCondBrInst(t0, body, cont),

		cont,
		meer.CreateAssign(args, t1),
		meer.CreateProcCall(assert, []meer.Expression{t1}),
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	max := meer.CreateIdent("max", nil)
	t1 := meer.CreateIdent("t1", nil)
	t0 := meer.CreateIdent("t0", nil)

	ifThen := meer.NewLabel("if.then")
	ifElse := meer.NewLabel("if.else")
	ifCont := meer.NewLabel("if.cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, max, &meer.IntegerConst{Value: 10}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 5}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(meer.CreateCmpOp(meer.Gt, a, b, nil), t0),
		meer.CreateCondBrInst(t0, ifThen, ifElse),

		ifThen,
		meer.CreateAssign(a, max),
		meer.CreateJmp(ifCont),

		ifElse,
		meer.CreateAssign(b, max),
		meer.CreateJmp(ifCont),

		ifCont,
		meer.CreateAssign(args, t1),
		meer.CreateProcCall(assert, []meer.Expression{t1}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	max := meer.CreateIdent("max", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)

	Then := meer.NewLabel("if.then")
	ElseIf := meer.NewLabel("elsif")
	Else := meer.NewLabel("if.else")
	ElifThen := meer.NewLabel("elif.then.0")
	ElifElse := meer.NewLabel("elif.else.0")
	ifCont := meer.NewLabel("if.cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, max, &meer.IntegerConst{Value: 10}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 5}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(meer.CreateCmpOp(meer.Gt, a, b, nil), t0),
		meer.CreateCondBrInst(t0, Then, ElseIf),

		Then,
		meer.CreateAssign(a, max),
		meer.CreateJmp(ifCont),

		ElseIf,
		meer.CreateAssign(meer.CreateCmpOp(meer.Eq, a, b, nil), t1),
		meer.CreateCondBrInst(t1, ElifThen, ElifElse),

		ElifThen,
		meer.CreateAssign(&meer.IntegerConst{Value: 15}, max),
		meer.CreateJmp(ifCont),

		ElifElse,
		meer.CreateJmp(Else),

		Else,
		meer.CreateAssign(b, max),
		meer.CreateJmp(ifCont),

		ifCont,
		meer.CreateAssign(args, t2),
		meer.CreateProcCall(assert, []meer.Expression{t2}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
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

	obx.AddUnit(unit.Name(), unit)

	mir := NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	max := meer.CreateIdent("max", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)
	t3 := meer.CreateIdent("t3", nil)

	Then := meer.NewLabel("if.then")
	ElseIf := meer.NewLabel("elsif")
	ElifThen := meer.NewLabel("elif.then.0")
	ElifElse := meer.NewLabel("elif.else.0")
	ElifThen1 := meer.NewLabel("elif.then.1")
	ElifElse1 := meer.NewLabel("elif.else.1")
	ifCont := meer.NewLabel("if.cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, max, &meer.IntegerConst{Value: 10}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 5}, a),
		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(meer.CreateCmpOp(meer.Gt, a, b, nil), t0),
		meer.CreateCondBrInst(t0, Then, ElseIf),

		Then,
		meer.CreateAssign(a, max),
		meer.CreateJmp(ifCont),

		ElseIf,
		meer.CreateAssign(meer.CreateCmpOp(meer.Eq, a, b, nil), t1),
		meer.CreateCondBrInst(t1, ElifThen, ElifElse),

		ElifThen,
		meer.CreateAssign(&meer.IntegerConst{Value: 15}, max),
		meer.CreateJmp(ifCont),

		ElifElse,
		meer.CreateAssign(meer.CreateCmpOp(meer.Ne, a, b, nil), t2),
		meer.CreateCondBrInst(t2, ElifThen1, ElifElse1),

		ElifThen1,
		meer.CreateAssign(&meer.IntegerConst{Value: 42}, max),
		meer.CreateJmp(ifCont),

		ElifElse1,
		meer.CreateJmp(ifCont),

		ifCont,
		meer.CreateAssign(args, t3),
		meer.CreateProcCall(assert, []meer.Expression{t3}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
}

func TestIRCodegenForStatement(t *testing.T) {
	input := `module Main
var a, b, total: integer

begin
    b := 10
	total := 0

	for a := 0 to b do 
		total := total + 1 
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

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	total := meer.CreateIdent("total", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)

	Body := meer.NewLabel("body")
	Cont := meer.NewLabel("cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, total, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, total),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, a),
		meer.CreateAssign(meer.CreateCmpOp(meer.Lt, a, b, nil), t0),
		meer.CreateCondBrInst(t0, Body, Cont),

		Body,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, total, &meer.IntegerConst{Value: 1}, nil), total),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, &meer.IntegerConst{Value: 1}, nil), a),
		meer.CreateAssign(meer.CreateCmpOp(meer.Lt, a, b, nil), t1),
		meer.CreateCondBrInst(t1, Body, Cont),

		Cont,
		meer.CreateAssign(args, t2),
		meer.CreateProcCall(assert, []meer.Expression{t2}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
}

func TestIRCodegenForStatementWithStep(t *testing.T) {
	input := `module Main
var a, b, step, total: integer

begin
    b := 10
	total := 0
	step := 3

	for a := 0 to b by step do 
		total := total + 1 
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

	a := meer.CreateIdent("a", nil)
	b := meer.CreateIdent("b", nil)
	total := meer.CreateIdent("total", nil)
	step := meer.CreateIdent("step", nil)
	t0 := meer.CreateIdent("t0", nil)
	t1 := meer.CreateIdent("t1", nil)
	t2 := meer.CreateIdent("t2", nil)

	Body := meer.NewLabel("body")
	Cont := meer.NewLabel("cont")

	assert := meer.CreateIdent("assert", nil)
	args := meer.CreateBinaryOp(meer.Eq, total, &meer.IntegerConst{Value: 55}, nil)

	tests := []meer.Instruction{
		meer.NewLabel("Main"),

		meer.CreateAssign(&meer.IntegerConst{Value: 10}, b),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, total),
		meer.CreateAssign(&meer.IntegerConst{Value: 3}, step),
		meer.CreateAssign(&meer.IntegerConst{Value: 0}, a),
		meer.CreateAssign(meer.CreateCmpOp(meer.Lt, a, b, nil), t0),
		meer.CreateCondBrInst(t0, Body, Cont),

		Body,
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, total, &meer.IntegerConst{Value: 1}, nil), total),
		meer.CreateAssign(meer.CreateBinaryOp(meer.Add, a, step, nil), a),
		meer.CreateAssign(meer.CreateCmpOp(meer.Lt, a, b, nil), t1),
		meer.CreateCondBrInst(t1, Body, Cont),

		Cont,
		meer.CreateAssign(args, t2),
		meer.CreateProcCall(assert, []meer.Expression{t2}),
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
		case *meer.JumpInst:
			jmp := tests[idx].(*meer.JumpInst)
			testJmp(t, inst, jmp)
		}
	}
}
