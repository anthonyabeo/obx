package analy

import (
	"testing"

	"github.com/anthonyabeo/obx/src/diagnostics"
	"github.com/anthonyabeo/obx/src/meer"
	"github.com/anthonyabeo/obx/src/mirgen"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/lexer"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

func TestBasicBlockLeadersInWhileLoop(t *testing.T) {
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

	mir := mirgen.NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	Main := program.Units["Main"]
	leaders := findLeaders(Main.Inst)

	// Order of label and block creation is important
	LblMain := meer.NewLabel("Main")
	LblLoop := meer.NewLabel("loop")
	LblIfThen := meer.NewLabel("if.then")
	LblIfElse := meer.NewLabel("if.else")
	LblCont := meer.NewLabel("cont")

	BlockMain := meer.NewBasicBlock(LblMain)
	BlockLoop := meer.NewBasicBlock(LblLoop)
	BlockIfThen := meer.NewBasicBlock(LblIfThen)
	BlockIfElse := meer.NewBasicBlock(LblIfElse)
	BlockCont := meer.NewBasicBlock(LblCont)

	tests := []struct {
		id       meer.BasicBlockID
		numInstr int
	}{
		{BlockIfThen.ID(), 3},
		{BlockIfElse.ID(), 1},
		{BlockLoop.ID(), 2},
		{BlockMain.ID(), 3},
		{BlockCont.ID(), 2},
	}

	for _, tt := range tests {
		BB, ok := leaders[tt.id]
		if !ok {
			t.Errorf("expected Basic Block leader '%s' not found", leaders[tt.id].Name())
			return
		}

		if tt.numInstr != BB.Instr().Len() {
			t.Errorf("expected Basic Block '%s' to have %d instructions, got %d",
				leaders[tt.id].Name(), tt.numInstr, BB.Instr().Len())
			return
		}
	}
}

func TestBuildCFGForWhileLoopProgram(t *testing.T) {
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

	mir := mirgen.NewVisitor(scopes)
	program := mir.Translate(obx, []string{unit.Name()})

	cfg := BuildCFG(program)

	// Order of label and block creation is important
	LblMain := meer.NewLabel("Main")
	LblLoop := meer.NewLabel("loop")
	LblIfThen := meer.NewLabel("if.then")
	LblIfElse := meer.NewLabel("if.else")
	LblCont := meer.NewLabel("cont")

	BlockMain := meer.NewBasicBlock(LblMain)
	BlockLoop := meer.NewBasicBlock(LblLoop)
	BlockIfThen := meer.NewBasicBlock(LblIfThen)
	BlockIfElse := meer.NewBasicBlock(LblIfElse)
	BlockCont := meer.NewBasicBlock(LblCont)

	Blocks := map[meer.BasicBlockID]*meer.BasicBlock{
		BlockMain.ID():   BlockMain,
		BlockLoop.ID():   BlockLoop,
		BlockCont.ID():   BlockCont,
		BlockIfElse.ID(): BlockIfElse,
		BlockIfThen.ID(): BlockIfThen,
	}

	successors := map[meer.BasicBlockID][]meer.BasicBlockID{
		BlockMain.ID():   {BlockLoop.ID()},
		BlockLoop.ID():   {BlockIfThen.ID(), BlockIfElse.ID()},
		BlockIfThen.ID(): {BlockLoop.ID()},
		BlockIfElse.ID(): {BlockCont.ID()},
		//BlockCont.ID():   {},
	}

	predecessors := map[meer.BasicBlockID][]meer.BasicBlockID{
		//BlockMain.ID():   {},
		BlockLoop.ID():   {BlockMain.ID(), BlockIfThen.ID()},
		BlockIfThen.ID(): {BlockLoop.ID()},
		BlockIfElse.ID(): {BlockLoop.ID()},
		BlockCont.ID():   {BlockIfElse.ID()},
	}

	if cfg.Nodes.Size() != len(Blocks) {
		t.Errorf("expected %d basic block in CFG, got %d",
			len(Blocks), cfg.Nodes.Size())
	}

	for id, suc := range successors {
		cfgSuc, ok := cfg.Suc[id]
		if !ok {
			t.Errorf("no successors found for block '%s'", Blocks[id].Name())
			return
		}

		for _, ttSuc := range suc {
			if !cfgSuc.Contains(ttSuc) {
				t.Errorf("expected '%s' to be a successor of '%s'",
					Blocks[ttSuc].Name(), Blocks[id].Name())
			}
		}
	}

	for id, pred := range predecessors {
		cfgPred, ok := cfg.Pred[id]
		if !ok {
			t.Errorf("no predecessors found for block '%s'",
				Blocks[id].Name())
		}

		for _, ttPred := range pred {
			if !cfgPred.Contains(ttPred) {
				t.Errorf("expected '%s' to be a predecessor of '%s'",
					Blocks[ttPred].Name(), Blocks[id].Name())
			}
		}
	}
}

func TestBuildCFGForWhileLoopWithSingleElseIfBranch(t *testing.T) {

}

func TestComputingExtendedBasicBlocks(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl0 := meer.NewLabel("B0")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lbl4 := meer.NewLabel("B4")
	lbl5 := meer.NewLabel("B5")
	lbl6 := meer.NewLabel("B6")
	lbl7 := meer.NewLabel("B7")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b0 := meer.NewBasicBlock(lbl0)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	b4 := meer.NewBasicBlock(lbl4)
	b5 := meer.NewBasicBlock(lbl5)
	b6 := meer.NewBasicBlock(lbl6)

	cfg := meer.NewCFG()
	cfg.Entry = entry

	cfg.Nodes.Add(entry, b0, b1, b2, b3, b4, b5, b6)

	cfg.AddSuc(entry.ID(), b0.ID())
	cfg.AddSuc(b0.ID(), b1.ID(), b3.ID())
	cfg.AddSuc(b1.ID(), b2.ID())
	cfg.AddSuc(b2.ID(), b0.ID())
	cfg.AddSuc(b3.ID(), b4.ID(), b6.ID())
	cfg.AddSuc(b4.ID(), b5.ID())
	cfg.AddSuc(b5.ID(), b2.ID())
	cfg.AddSuc(b6.ID(), b5.ID())

	cfg.AddPred(b0.ID())
	cfg.AddPred(b1.ID(), b0.ID())
	cfg.AddPred(b2.ID(), b1.ID(), b5.ID())
	cfg.AddPred(b3.ID(), b0.ID())
	cfg.AddPred(b4.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b6.ID(), b4.ID())
	cfg.AddPred(b6.ID(), b3.ID())

	tests := []struct {
		root   meer.BasicBlockID
		blocks []meer.BasicBlockID
	}{
		{b0.ID(), []meer.BasicBlockID{b0.ID(), b1.ID(), b3.ID(), b4.ID(), b6.ID()}},
		{b5.ID(), []meer.BasicBlockID{b5.ID()}},
		{b2.ID(), []meer.BasicBlockID{b2.ID()}},
	}

	extBBs := ExtendedBasicBlocks(cfg, b0.ID())
	for _, tt := range tests {
		ebb, found := extBBs[tt.root]
		if !found {
			t.Errorf("no EBB found for root '%d'", tt.root)
		}

		if len(tt.blocks) != ebb.Size() {
			t.Errorf("expected an EBB of size %d, got %d",
				len(tt.blocks), ebb.Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			if !ebb.Contains(tt.blocks[i]) {
				t.Errorf("expected %d to be in the EBB of root %d. It doesn't",
					tt.blocks[i], tt.root)
			}
		}
	}

	entry = meer.NewBasicBlock(lblEntry)
	b1 = meer.NewBasicBlock(lbl1)
	b2 = meer.NewBasicBlock(lbl2)
	b3 = meer.NewBasicBlock(lbl3)
	b4 = meer.NewBasicBlock(lbl4)
	b5 = meer.NewBasicBlock(lbl5)
	b6 = meer.NewBasicBlock(lbl6)
	b7 := meer.NewBasicBlock(lbl7)
	exit := meer.NewBasicBlock(lblExit)

	cfg = meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)

	cfg.AddSuc(entry.ID(), b1.ID())
	cfg.AddSuc(b1.ID(), b2.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b4.ID())
	cfg.AddSuc(b3.ID(), b4.ID())
	cfg.AddSuc(b4.ID(), b5.ID(), b6.ID())
	cfg.AddSuc(b5.ID(), b7.ID())
	cfg.AddSuc(b6.ID(), exit.ID(), b1.ID())
	cfg.AddSuc(b7.ID(), exit.ID(), b5.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), b6.ID(), entry.ID())
	cfg.AddPred(b2.ID(), b1.ID())
	cfg.AddPred(b3.ID(), b1.ID())
	cfg.AddPred(b4.ID(), b2.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b7.ID(), b4.ID())
	cfg.AddPred(b6.ID(), b4.ID())
	cfg.AddPred(b7.ID(), b5.ID())
	cfg.AddPred(exit.ID(), b6.ID(), b7.ID())

	tests = []struct {
		root   meer.BasicBlockID
		blocks []meer.BasicBlockID
	}{
		{entry.ID(), []meer.BasicBlockID{entry.ID()}},
		{b1.ID(), []meer.BasicBlockID{b1.ID(), b2.ID(), b3.ID()}},
		{b4.ID(), []meer.BasicBlockID{b4.ID(), b6.ID()}},
		{b5.ID(), []meer.BasicBlockID{b5.ID(), b7.ID()}},
		{exit.ID(), []meer.BasicBlockID{exit.ID()}},
	}

	extBBs = ExtendedBasicBlocks(cfg, entry.ID())
	for _, tt := range tests {
		ebb, found := extBBs[tt.root]
		if !found {
			t.Errorf("no EBB found for root '%d'", tt.root)
		}

		if len(tt.blocks) != ebb.Size() {
			t.Errorf("expected an EBB of size %d, got %d", len(tt.blocks), ebb.Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			if !ebb.Contains(tt.blocks[i]) {
				t.Errorf("expected %d to be in the EBB of root %d. It doesn't",
					tt.blocks[i], tt.root)
			}
		}
	}
}

func TestComputeDominance(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lbl4 := meer.NewLabel("B4")
	lbl5 := meer.NewLabel("B5")
	lbl6 := meer.NewLabel("B6")
	lbl7 := meer.NewLabel("B7")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	b4 := meer.NewBasicBlock(lbl4)
	b5 := meer.NewBasicBlock(lbl5)
	b6 := meer.NewBasicBlock(lbl6)
	b7 := meer.NewBasicBlock(lbl7)
	exit := meer.NewBasicBlock(lblExit)

	cfg := meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		entry.ID(): entry,
		b1.ID():    b1,
		b2.ID():    b2,
		b3.ID():    b3,
		b4.ID():    b4,
		b5.ID():    b5,
		b6.ID():    b6,
		b7.ID():    b7,
		exit.ID():  exit,
	}

	cfg.AddSuc(entry.ID(), b1.ID())
	cfg.AddSuc(b1.ID(), b2.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b4.ID())
	cfg.AddSuc(b3.ID(), b4.ID())
	cfg.AddSuc(b4.ID(), b5.ID(), b6.ID())
	cfg.AddSuc(b5.ID(), b7.ID())
	cfg.AddSuc(b6.ID(), exit.ID(), b1.ID())
	cfg.AddSuc(b7.ID(), exit.ID(), b5.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), b6.ID(), entry.ID())
	cfg.AddPred(b2.ID(), b1.ID())
	cfg.AddPred(b3.ID(), b1.ID())
	cfg.AddPred(b4.ID(), b2.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b7.ID(), b4.ID())
	cfg.AddPred(b6.ID(), b4.ID())
	cfg.AddPred(b7.ID(), b5.ID())
	cfg.AddPred(exit.ID(), b6.ID(), b7.ID())

	tests := []struct {
		id     meer.BasicBlockID
		blocks []*meer.BasicBlock
	}{
		{entry.ID(), []*meer.BasicBlock{entry}},
		{b1.ID(), []*meer.BasicBlock{entry, b1}},
		{b2.ID(), []*meer.BasicBlock{entry, b1, b2}},
		{b3.ID(), []*meer.BasicBlock{entry, b1, b3}},
		{b4.ID(), []*meer.BasicBlock{entry, b1, b4}},
		{b5.ID(), []*meer.BasicBlock{entry, b1, b4, b5}},
		{b6.ID(), []*meer.BasicBlock{entry, b1, b4, b6}},
		{b7.ID(), []*meer.BasicBlock{entry, b1, b4, b5, b7}},
		{exit.ID(), []*meer.BasicBlock{entry, b1, b4, exit}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != dom[tt.id].Size() {
			t.Errorf("expected a dom set of size %d, got %d",
				len(tt.blocks), dom[tt.id].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := dom[tt.id]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DOM['%s'] to contain '%s'",
					cfg.Blocks[tt.id], tt.blocks[i])
			}
		}
	}
}

func TestComputeDominance2(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	exit := meer.NewBasicBlock(lblExit)

	cfg := meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		entry.ID(): entry,
		b1.ID():    b1,
		b2.ID():    b2,
		b3.ID():    b3,
		exit.ID():  exit,
	}

	cfg.AddSuc(entry.ID(), b1.ID(), b2.ID())
	cfg.AddSuc(b1.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b3.ID())
	cfg.AddSuc(b3.ID(), exit.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), entry.ID())
	cfg.AddPred(b2.ID(), entry.ID())
	cfg.AddPred(b3.ID(), b1.ID(), b2.ID())
	cfg.AddPred(exit.ID(), b3.ID())

	tests := []struct {
		id     meer.BasicBlockID
		blocks []*meer.BasicBlock
	}{
		{entry.ID(), []*meer.BasicBlock{entry}},
		{b1.ID(), []*meer.BasicBlock{entry, b1}},
		{b2.ID(), []*meer.BasicBlock{entry, b2}},
		{b3.ID(), []*meer.BasicBlock{entry, b3}},
		{exit.ID(), []*meer.BasicBlock{entry, b3, exit}},
	}

	dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != dom[tt.id].Size() {
			t.Errorf("expected a dom set of size %d, got %d",
				len(tt.blocks), dom[tt.id].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := dom[tt.id]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected %s to be in the DOM of %s. It doesn't",
					tt.blocks[i], cfg.Blocks[tt.id])

			}
		}
	}
}

func TestComputeDominance3(t *testing.T) {
	lbl0 := meer.NewLabel("B0")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lbl4 := meer.NewLabel("B4")
	lbl5 := meer.NewLabel("B5")
	lbl6 := meer.NewLabel("B6")
	lbl7 := meer.NewLabel("B7")
	lbl8 := meer.NewLabel("B8")

	b0 := meer.NewBasicBlock(lbl0)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	b4 := meer.NewBasicBlock(lbl4)
	b5 := meer.NewBasicBlock(lbl5)
	b6 := meer.NewBasicBlock(lbl6)
	b7 := meer.NewBasicBlock(lbl7)
	b8 := meer.NewBasicBlock(lbl8)

	cfg := meer.NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		b0.ID(): b0,
		b1.ID(): b1,
		b2.ID(): b2,
		b3.ID(): b3,
		b4.ID(): b4,
		b5.ID(): b5,
		b6.ID(): b6,
		b7.ID(): b7,
		b8.ID(): b8,
	}

	cfg.AddSuc(b0.ID(), b1.ID())
	cfg.AddSuc(b1.ID(), b2.ID(), b5.ID())
	cfg.AddSuc(b2.ID(), b3.ID())
	cfg.AddSuc(b3.ID(), b1.ID(), b4.ID())
	cfg.AddSuc(b4.ID())
	cfg.AddSuc(b5.ID(), b6.ID(), b8.ID())
	cfg.AddSuc(b6.ID(), b7.ID())
	cfg.AddSuc(b7.ID(), b3.ID())
	cfg.AddSuc(b8.ID(), b7.ID())

	cfg.AddPred(b0.ID())
	cfg.AddPred(b1.ID(), b0.ID(), b3.ID())
	cfg.AddPred(b2.ID(), b1.ID())
	cfg.AddPred(b3.ID(), b2.ID(), b7.ID())
	cfg.AddPred(b4.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b1.ID())
	cfg.AddPred(b6.ID(), b5.ID())
	cfg.AddPred(b7.ID(), b6.ID(), b8.ID())
	cfg.AddPred(b8.ID(), b5.ID())

	tests := []struct {
		id     meer.BasicBlockID
		blocks []*meer.BasicBlock
	}{
		{b0.ID(), []*meer.BasicBlock{b0}},
		{b1.ID(), []*meer.BasicBlock{b0, b1}},
		{b2.ID(), []*meer.BasicBlock{b0, b1, b2}},
		{b3.ID(), []*meer.BasicBlock{b0, b1, b3}},
		{b4.ID(), []*meer.BasicBlock{b0, b1, b3, b4}},
		{b5.ID(), []*meer.BasicBlock{b0, b1, b5}},
		{b6.ID(), []*meer.BasicBlock{b0, b1, b5, b6}},
		{b7.ID(), []*meer.BasicBlock{b0, b1, b5, b7}},
		{b8.ID(), []*meer.BasicBlock{b0, b1, b5, b8}},
	}

	Dom := Dominance(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != Dom[tt.id].Size() {
			t.Errorf("expected a dom set of size %d, got %d",
				len(tt.blocks), Dom[tt.id].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exists := Dom[tt.id]
			if !exists || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DOM['%s'] to contain '%s'",
					cfg.Blocks[tt.id], tt.blocks[i])
			}
		}
	}
}

func TestComputeImmediateDominance(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	exit := meer.NewBasicBlock(lblExit)

	cfg := meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		entry.ID(): entry,
		b1.ID():    b1,
		b2.ID():    b2,
		b3.ID():    b3,
		exit.ID():  exit,
	}

	cfg.AddSuc(entry.ID(), b1.ID(), b2.ID())
	cfg.AddSuc(b1.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b3.ID())
	cfg.AddSuc(b3.ID(), exit.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), entry.ID())
	cfg.AddPred(b2.ID(), entry.ID())
	cfg.AddPred(b3.ID(), b1.ID(), b2.ID())
	cfg.AddPred(exit.ID(), b3.ID())

	dom := Dominance(cfg)
	iDom := ImmDominator(cfg, dom)

	tests := []struct {
		id  meer.BasicBlockID
		blk string
	}{
		{b1.ID(), "entry"},
		{b2.ID(), "entry"},
		{b3.ID(), "entry"},
		{exit.ID(), "B3"},
	}

	for _, tt := range tests {
		if _, found := iDom[tt.id]; !found {
			t.Errorf("no value for IDom(%s)", cfg.Blocks[tt.id])
			continue
		}

		if tt.blk != iDom[tt.id].Name() {
			t.Errorf("expected IDom(%s) --> %s; got %s instead",
				cfg.Blocks[tt.id], tt.blk, iDom[tt.id])
		}
	}
}

func TestComputeNaturalLoop(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	exit := meer.NewBasicBlock(lblExit)

	cfg := meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		entry.ID(): entry,
		b1.ID():    b1,
		b2.ID():    b2,
		b3.ID():    b3,
		exit.ID():  exit,
	}

	cfg.AddSuc(entry.ID(), b1.ID(), b2.ID())
	cfg.AddSuc(b1.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b3.ID())
	cfg.AddSuc(b3.ID(), exit.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), entry.ID())
	cfg.AddPred(b2.ID(), entry.ID())
	cfg.AddPred(b3.ID(), b1.ID(), b2.ID())
	cfg.AddPred(exit.ID(), b3.ID())

	nat := NaturalLoop(cfg, b3, b1)

	tests := []*meer.BasicBlock{b1, b2, b3}
	for _, tt := range tests {
		if !nat.Contains(tt.ID()) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestComputeNaturalLoop2(t *testing.T) {
	lblEntry := meer.NewLabel("entry")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lbl4 := meer.NewLabel("B4")
	lbl5 := meer.NewLabel("B5")
	lbl6 := meer.NewLabel("B6")
	lbl7 := meer.NewLabel("B7")
	lblExit := meer.NewLabel("exit")

	entry := meer.NewBasicBlock(lblEntry)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	b4 := meer.NewBasicBlock(lbl4)
	b5 := meer.NewBasicBlock(lbl5)
	b6 := meer.NewBasicBlock(lbl6)
	b7 := meer.NewBasicBlock(lbl7)
	exit := meer.NewBasicBlock(lblExit)

	cfg := meer.NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		entry.ID(): entry,
		b1.ID():    b1,
		b2.ID():    b2,
		b3.ID():    b3,
		b4.ID():    b4,
		b5.ID():    b5,
		b6.ID():    b6,
		b7.ID():    b7,
		exit.ID():  exit,
	}

	cfg.AddSuc(entry.ID(), b1.ID())
	cfg.AddSuc(b1.ID(), b2.ID(), b3.ID())
	cfg.AddSuc(b2.ID(), b4.ID())
	cfg.AddSuc(b3.ID(), b4.ID())
	cfg.AddSuc(b4.ID(), b5.ID(), b6.ID())
	cfg.AddSuc(b5.ID(), b7.ID())
	cfg.AddSuc(b6.ID(), exit.ID(), b1.ID())
	cfg.AddSuc(b7.ID(), exit.ID(), b5.ID())
	cfg.AddSuc(exit.ID())

	cfg.AddPred(entry.ID())
	cfg.AddPred(b1.ID(), b6.ID(), entry.ID())
	cfg.AddPred(b2.ID(), b1.ID())
	cfg.AddPred(b3.ID(), b1.ID())
	cfg.AddPred(b4.ID(), b2.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b7.ID(), b4.ID())
	cfg.AddPred(b6.ID(), b4.ID())
	cfg.AddPred(b7.ID(), b5.ID())
	cfg.AddPred(exit.ID(), b6.ID(), b7.ID())

	nat := NaturalLoop(cfg, b6, b1)
	tests := []*meer.BasicBlock{b1, b2, b3, b4, b6}
	for _, tt := range tests {
		if !nat.Contains(tt.ID()) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}

	natLoop := NaturalLoop(cfg, b7, b5)
	tests = []*meer.BasicBlock{b7, b5}
	for _, tt := range tests {
		if !natLoop.Contains(tt.ID()) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}
}

func TestDominanceFrontier(t *testing.T) {
	lbl0 := meer.NewLabel("B0")
	lbl1 := meer.NewLabel("B1")
	lbl2 := meer.NewLabel("B2")
	lbl3 := meer.NewLabel("B3")
	lbl4 := meer.NewLabel("B4")
	lbl5 := meer.NewLabel("B5")
	lbl6 := meer.NewLabel("B6")
	lbl7 := meer.NewLabel("B7")
	lbl8 := meer.NewLabel("B8")

	b0 := meer.NewBasicBlock(lbl0)
	b1 := meer.NewBasicBlock(lbl1)
	b2 := meer.NewBasicBlock(lbl2)
	b3 := meer.NewBasicBlock(lbl3)
	b4 := meer.NewBasicBlock(lbl4)
	b5 := meer.NewBasicBlock(lbl5)
	b6 := meer.NewBasicBlock(lbl6)
	b7 := meer.NewBasicBlock(lbl7)
	b8 := meer.NewBasicBlock(lbl8)

	cfg := meer.NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)
	cfg.Blocks = map[meer.BasicBlockID]*meer.BasicBlock{
		b0.ID(): b0,
		b1.ID(): b1,
		b2.ID(): b2,
		b3.ID(): b3,
		b4.ID(): b4,
		b5.ID(): b5,
		b6.ID(): b6,
		b7.ID(): b7,
		b8.ID(): b8,
	}

	cfg.AddSuc(b0.ID(), b1.ID())
	cfg.AddSuc(b1.ID(), b2.ID(), b5.ID())
	cfg.AddSuc(b2.ID(), b3.ID())
	cfg.AddSuc(b3.ID(), b1.ID(), b4.ID())
	cfg.AddSuc(b4.ID())
	cfg.AddSuc(b5.ID(), b6.ID(), b8.ID())
	cfg.AddSuc(b6.ID(), b7.ID())
	cfg.AddSuc(b7.ID(), b3.ID())
	cfg.AddSuc(b8.ID(), b7.ID())

	cfg.AddPred(b0.ID())
	cfg.AddPred(b1.ID(), b0.ID(), b3.ID())
	cfg.AddPred(b2.ID(), b1.ID())
	cfg.AddPred(b3.ID(), b2.ID(), b7.ID())
	cfg.AddPred(b4.ID(), b3.ID())
	cfg.AddPred(b5.ID(), b1.ID())
	cfg.AddPred(b6.ID(), b5.ID())
	cfg.AddPred(b7.ID(), b6.ID(), b8.ID())
	cfg.AddPred(b8.ID(), b5.ID())

	tests := []struct {
		id     meer.BasicBlockID
		blocks []*meer.BasicBlock
	}{
		{b0.ID(), []*meer.BasicBlock{}},
		{b1.ID(), []*meer.BasicBlock{b1}},
		{b2.ID(), []*meer.BasicBlock{b3}},
		{b3.ID(), []*meer.BasicBlock{b1}},
		{b4.ID(), []*meer.BasicBlock{}},
		{b5.ID(), []*meer.BasicBlock{b3}},
		{b6.ID(), []*meer.BasicBlock{b7}},
		{b7.ID(), []*meer.BasicBlock{b3}},
		{b8.ID(), []*meer.BasicBlock{b7}},
	}

	DF := DominanceFrontier(cfg)
	for _, tt := range tests {
		if len(tt.blocks) != DF[tt.id].Size() {
			t.Errorf("expected a dom set of size %d, got %d", len(tt.blocks), DF[tt.id].Size())
		}

		for i := 0; i < len(tt.blocks); i++ {
			set, exist := DF[tt.id]
			if !exist || !set.Contains(tt.blocks[i]) {
				t.Errorf("expected DF of '%s' to contain '%s'", cfg.Blocks[tt.id], tt.blocks[i])
			}
		}
	}

}
