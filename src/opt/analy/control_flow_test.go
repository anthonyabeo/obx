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

	BlockMain := NewBasicBlock(LblMain)
	BlockLoop := NewBasicBlock(LblLoop)
	BlockIfThen := NewBasicBlock(LblIfThen)
	BlockIfElse := NewBasicBlock(LblIfElse)
	BlockCont := NewBasicBlock(LblCont)

	tests := []struct {
		id       uint
		numInstr int
	}{
		{BlockIfThen.id, 3},
		{BlockIfElse.id, 1},
		{BlockLoop.id, 1},
		{BlockMain.id, 3},
		{BlockCont.id, 1},
	}

	for _, tt := range tests {
		BB, ok := leaders[tt.id]
		if !ok {
			t.Errorf("expected Basic Block leader '%s' not found", leaders[tt.id].name)
			return
		}

		if tt.numInstr != BB.instr.Len() {
			t.Errorf("expected Basic Block '%s' to have %d instructions, got %d",
				leaders[tt.id].name, tt.numInstr, BB.instr.Len())
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

	BlockMain := NewBasicBlock(LblMain)
	BlockLoop := NewBasicBlock(LblLoop)
	BlockIfThen := NewBasicBlock(LblIfThen)
	BlockIfElse := NewBasicBlock(LblIfElse)
	BlockCont := NewBasicBlock(LblCont)

	Blocks := map[uint]*BasicBlock{
		BlockMain.id:   BlockMain,
		BlockLoop.id:   BlockLoop,
		BlockCont.id:   BlockCont,
		BlockIfElse.id: BlockIfElse,
		BlockIfThen.id: BlockIfThen,
	}

	successors := map[uint][]uint{
		BlockMain.id:   {BlockLoop.id},
		BlockLoop.id:   {BlockIfThen.id, BlockIfElse.id},
		BlockIfThen.id: {BlockLoop.id},
		BlockIfElse.id: {BlockCont.id},
		//BlockCont.id:   {},
	}

	predecessors := map[uint][]uint{
		//BlockMain.id:   {},
		BlockLoop.id:   {BlockMain.id, BlockIfThen.id},
		BlockIfThen.id: {BlockLoop.id},
		BlockIfElse.id: {BlockLoop.id},
		BlockCont.id:   {BlockIfElse.id},
	}

	if cfg.Nodes.Size() != len(Blocks) {
		t.Errorf("expected %d basic block in CFG, got %d",
			len(Blocks), cfg.Nodes.Size())
	}

	for id, suc := range successors {
		cfgSuc, ok := cfg.Suc[id]
		if !ok {
			t.Errorf("no successors found for block '%s'", Blocks[id].name)
			return
		}

		for _, ttSuc := range suc {
			if !cfgSuc.Contains(ttSuc) {
				t.Errorf("expected '%s' to be a successor of '%s'",
					Blocks[ttSuc].name, Blocks[id].name)
			}
		}
	}

	for id, pred := range predecessors {
		cfgPred, ok := cfg.Pred[id]
		if !ok {
			t.Errorf("no predecessors found for block '%s'",
				Blocks[id].name)
		}

		for _, ttPred := range pred {
			if !cfgPred.Contains(ttPred) {
				t.Errorf("expected '%s' to be a predecessor of '%s'",
					Blocks[ttPred].name, Blocks[id].name)
			}
		}
	}
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

	entry := NewBasicBlock(lblEntry)
	b0 := NewBasicBlock(lbl0)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	b4 := NewBasicBlock(lbl4)
	b5 := NewBasicBlock(lbl5)
	b6 := NewBasicBlock(lbl6)

	cfg := NewCFG()
	cfg.Entry = entry

	cfg.Nodes.Add(entry, b0, b1, b2, b3, b4, b5, b6)

	cfg.AddSuc(entry.id, b0.id)
	cfg.AddSuc(b0.id, b1.id, b3.id)
	cfg.AddSuc(b1.id, b2.id)
	cfg.AddSuc(b2.id, b0.id)
	cfg.AddSuc(b3.id, b4.id, b6.id)
	cfg.AddSuc(b4.id, b5.id)
	cfg.AddSuc(b5.id, b2.id)
	cfg.AddSuc(b6.id, b5.id)

	cfg.AddPred(b0.id)
	cfg.AddPred(b1.id, b0.id)
	cfg.AddPred(b2.id, b1.id, b5.id)
	cfg.AddPred(b3.id, b0.id)
	cfg.AddPred(b4.id, b3.id)
	cfg.AddPred(b5.id, b6.id, b4.id)
	cfg.AddPred(b6.id, b3.id)

	tests := []struct {
		root   uint
		blocks []uint
	}{
		{b0.id, []uint{b0.id, b1.id, b3.id, b4.id, b6.id}},
		{b5.id, []uint{b5.id}},
		{b2.id, []uint{b2.id}},
	}

	extBBs := ExtendedBasicBlocks(cfg, b0.id)
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

	entry = NewBasicBlock(lblEntry)
	b1 = NewBasicBlock(lbl1)
	b2 = NewBasicBlock(lbl2)
	b3 = NewBasicBlock(lbl3)
	b4 = NewBasicBlock(lbl4)
	b5 = NewBasicBlock(lbl5)
	b6 = NewBasicBlock(lbl6)
	b7 := NewBasicBlock(lbl7)
	exit := NewBasicBlock(lblExit)

	cfg = NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)

	cfg.AddSuc(entry.id, b1.id)
	cfg.AddSuc(b1.id, b2.id, b3.id)
	cfg.AddSuc(b2.id, b4.id)
	cfg.AddSuc(b3.id, b4.id)
	cfg.AddSuc(b4.id, b5.id, b6.id)
	cfg.AddSuc(b5.id, b7.id)
	cfg.AddSuc(b6.id, exit.id, b1.id)
	cfg.AddSuc(b7.id, exit.id, b5.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, b6.id, entry.id)
	cfg.AddPred(b2.id, b1.id)
	cfg.AddPred(b3.id, b1.id)
	cfg.AddPred(b4.id, b2.id, b3.id)
	cfg.AddPred(b5.id, b7.id, b4.id)
	cfg.AddPred(b6.id, b4.id)
	cfg.AddPred(b7.id, b5.id)
	cfg.AddPred(exit.id, b6.id, b7.id)

	tests = []struct {
		root   uint
		blocks []uint
	}{
		{entry.id, []uint{entry.id}},
		{b1.id, []uint{b1.id, b2.id, b3.id}},
		{b4.id, []uint{b4.id, b6.id}},
		{b5.id, []uint{b5.id, b7.id}},
		{exit.id, []uint{exit.id}},
	}

	extBBs = ExtendedBasicBlocks(cfg, entry.id)
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

	entry := NewBasicBlock(lblEntry)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	b4 := NewBasicBlock(lbl4)
	b5 := NewBasicBlock(lbl5)
	b6 := NewBasicBlock(lbl6)
	b7 := NewBasicBlock(lbl7)
	exit := NewBasicBlock(lblExit)

	cfg := NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)
	cfg.Blocks = map[uint]*BasicBlock{
		entry.id: entry,
		b1.id:    b1,
		b2.id:    b2,
		b3.id:    b3,
		b4.id:    b4,
		b5.id:    b5,
		b6.id:    b6,
		b7.id:    b7,
		exit.id:  exit,
	}

	cfg.AddSuc(entry.id, b1.id)
	cfg.AddSuc(b1.id, b2.id, b3.id)
	cfg.AddSuc(b2.id, b4.id)
	cfg.AddSuc(b3.id, b4.id)
	cfg.AddSuc(b4.id, b5.id, b6.id)
	cfg.AddSuc(b5.id, b7.id)
	cfg.AddSuc(b6.id, exit.id, b1.id)
	cfg.AddSuc(b7.id, exit.id, b5.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, b6.id, entry.id)
	cfg.AddPred(b2.id, b1.id)
	cfg.AddPred(b3.id, b1.id)
	cfg.AddPred(b4.id, b2.id, b3.id)
	cfg.AddPred(b5.id, b7.id, b4.id)
	cfg.AddPred(b6.id, b4.id)
	cfg.AddPred(b7.id, b5.id)
	cfg.AddPred(exit.id, b6.id, b7.id)

	tests := []struct {
		id     uint
		blocks []*BasicBlock
	}{
		{entry.id, []*BasicBlock{entry}},
		{b1.id, []*BasicBlock{entry, b1}},
		{b2.id, []*BasicBlock{entry, b1, b2}},
		{b3.id, []*BasicBlock{entry, b1, b3}},
		{b4.id, []*BasicBlock{entry, b1, b4}},
		{b5.id, []*BasicBlock{entry, b1, b4, b5}},
		{b6.id, []*BasicBlock{entry, b1, b4, b6}},
		{b7.id, []*BasicBlock{entry, b1, b4, b5, b7}},
		{exit.id, []*BasicBlock{entry, b1, b4, exit}},
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

	entry := NewBasicBlock(lblEntry)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	exit := NewBasicBlock(lblExit)

	cfg := NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[uint]*BasicBlock{
		entry.id: entry,
		b1.id:    b1,
		b2.id:    b2,
		b3.id:    b3,
		exit.id:  exit,
	}

	cfg.AddSuc(entry.id, b1.id, b2.id)
	cfg.AddSuc(b1.id, b3.id)
	cfg.AddSuc(b2.id, b3.id)
	cfg.AddSuc(b3.id, exit.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, entry.id)
	cfg.AddPred(b2.id, entry.id)
	cfg.AddPred(b3.id, b1.id, b2.id)
	cfg.AddPred(exit.id, b3.id)

	tests := []struct {
		id     uint
		blocks []*BasicBlock
	}{
		{entry.id, []*BasicBlock{entry}},
		{b1.id, []*BasicBlock{entry, b1}},
		{b2.id, []*BasicBlock{entry, b2}},
		{b3.id, []*BasicBlock{entry, b3}},
		{exit.id, []*BasicBlock{entry, b3, exit}},
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

	b0 := NewBasicBlock(lbl0)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	b4 := NewBasicBlock(lbl4)
	b5 := NewBasicBlock(lbl5)
	b6 := NewBasicBlock(lbl6)
	b7 := NewBasicBlock(lbl7)
	b8 := NewBasicBlock(lbl8)

	cfg := NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)
	cfg.Blocks = map[uint]*BasicBlock{
		b0.id: b0,
		b1.id: b1,
		b2.id: b2,
		b3.id: b3,
		b4.id: b4,
		b5.id: b5,
		b6.id: b6,
		b7.id: b7,
		b8.id: b8,
	}

	cfg.AddSuc(b0.id, b1.id)
	cfg.AddSuc(b1.id, b2.id, b5.id)
	cfg.AddSuc(b2.id, b3.id)
	cfg.AddSuc(b3.id, b1.id, b4.id)
	cfg.AddSuc(b4.id)
	cfg.AddSuc(b5.id, b6.id, b8.id)
	cfg.AddSuc(b6.id, b7.id)
	cfg.AddSuc(b7.id, b3.id)
	cfg.AddSuc(b8.id, b7.id)

	cfg.AddPred(b0.id)
	cfg.AddPred(b1.id, b0.id, b3.id)
	cfg.AddPred(b2.id, b1.id)
	cfg.AddPred(b3.id, b2.id, b7.id)
	cfg.AddPred(b4.id, b3.id)
	cfg.AddPred(b5.id, b1.id)
	cfg.AddPred(b6.id, b5.id)
	cfg.AddPred(b7.id, b6.id, b8.id)
	cfg.AddPred(b8.id, b5.id)

	tests := []struct {
		id     uint
		blocks []*BasicBlock
	}{
		{b0.id, []*BasicBlock{b0}},
		{b1.id, []*BasicBlock{b0, b1}},
		{b2.id, []*BasicBlock{b0, b1, b2}},
		{b3.id, []*BasicBlock{b0, b1, b3}},
		{b4.id, []*BasicBlock{b0, b1, b3, b4}},
		{b5.id, []*BasicBlock{b0, b1, b5}},
		{b6.id, []*BasicBlock{b0, b1, b5, b6}},
		{b7.id, []*BasicBlock{b0, b1, b5, b7}},
		{b8.id, []*BasicBlock{b0, b1, b5, b8}},
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

	entry := NewBasicBlock(lblEntry)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	exit := NewBasicBlock(lblExit)

	cfg := NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[uint]*BasicBlock{
		entry.id: entry,
		b1.id:    b1,
		b2.id:    b2,
		b3.id:    b3,
		exit.id:  exit,
	}

	cfg.AddSuc(entry.id, b1.id, b2.id)
	cfg.AddSuc(b1.id, b3.id)
	cfg.AddSuc(b2.id, b3.id)
	cfg.AddSuc(b3.id, exit.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, entry.id)
	cfg.AddPred(b2.id, entry.id)
	cfg.AddPred(b3.id, b1.id, b2.id)
	cfg.AddPred(exit.id, b3.id)

	dom := Dominance(cfg)
	iDom := ImmDominator(cfg, dom)

	tests := []struct {
		id  uint
		blk string
	}{
		{b1.id, "entry"},
		{b2.id, "entry"},
		{b3.id, "entry"},
		{exit.id, "B3"},
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

	entry := NewBasicBlock(lblEntry)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	exit := NewBasicBlock(lblExit)

	cfg := NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, exit)
	cfg.Blocks = map[uint]*BasicBlock{
		entry.id: entry,
		b1.id:    b1,
		b2.id:    b2,
		b3.id:    b3,
		exit.id:  exit,
	}

	cfg.AddSuc(entry.id, b1.id, b2.id)
	cfg.AddSuc(b1.id, b3.id)
	cfg.AddSuc(b2.id, b3.id)
	cfg.AddSuc(b3.id, exit.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, entry.id)
	cfg.AddPred(b2.id, entry.id)
	cfg.AddPred(b3.id, b1.id, b2.id)
	cfg.AddPred(exit.id, b3.id)

	nat := NaturalLoop(cfg, b3, b1)

	tests := []*BasicBlock{b1, b2, b3}
	for _, tt := range tests {
		if !nat.Contains(tt.id) {
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

	entry := NewBasicBlock(lblEntry)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	b4 := NewBasicBlock(lbl4)
	b5 := NewBasicBlock(lbl5)
	b6 := NewBasicBlock(lbl6)
	b7 := NewBasicBlock(lbl7)
	exit := NewBasicBlock(lblExit)

	cfg := NewCFG()
	cfg.Entry = entry
	cfg.Exit = exit

	cfg.Nodes.Add(entry, b1, b2, b3, b4, b5, b6, b7, exit)
	cfg.Blocks = map[uint]*BasicBlock{
		entry.id: entry,
		b1.id:    b1,
		b2.id:    b2,
		b3.id:    b3,
		b4.id:    b4,
		b5.id:    b5,
		b6.id:    b6,
		b7.id:    b7,
		exit.id:  exit,
	}

	cfg.AddSuc(entry.id, b1.id)
	cfg.AddSuc(b1.id, b2.id, b3.id)
	cfg.AddSuc(b2.id, b4.id)
	cfg.AddSuc(b3.id, b4.id)
	cfg.AddSuc(b4.id, b5.id, b6.id)
	cfg.AddSuc(b5.id, b7.id)
	cfg.AddSuc(b6.id, exit.id, b1.id)
	cfg.AddSuc(b7.id, exit.id, b5.id)
	cfg.AddSuc(exit.id)

	cfg.AddPred(entry.id)
	cfg.AddPred(b1.id, b6.id, entry.id)
	cfg.AddPred(b2.id, b1.id)
	cfg.AddPred(b3.id, b1.id)
	cfg.AddPred(b4.id, b2.id, b3.id)
	cfg.AddPred(b5.id, b7.id, b4.id)
	cfg.AddPred(b6.id, b4.id)
	cfg.AddPred(b7.id, b5.id)
	cfg.AddPred(exit.id, b6.id, b7.id)

	nat := NaturalLoop(cfg, b6, b1)
	tests := []*BasicBlock{b1, b2, b3, b4, b6}
	for _, tt := range tests {
		if !nat.Contains(tt.id) {
			t.Errorf("'%s' should not be part of the natural loop of B3->B1", tt)
		}
	}

	natLoop := NaturalLoop(cfg, b7, b5)
	tests = []*BasicBlock{b7, b5}
	for _, tt := range tests {
		if !natLoop.Contains(tt.id) {
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

	b0 := NewBasicBlock(lbl0)
	b1 := NewBasicBlock(lbl1)
	b2 := NewBasicBlock(lbl2)
	b3 := NewBasicBlock(lbl3)
	b4 := NewBasicBlock(lbl4)
	b5 := NewBasicBlock(lbl5)
	b6 := NewBasicBlock(lbl6)
	b7 := NewBasicBlock(lbl7)
	b8 := NewBasicBlock(lbl8)

	cfg := NewCFG()
	cfg.Entry = b0
	cfg.Exit = b4

	cfg.Nodes.Add(b0, b1, b2, b3, b4, b5, b6, b7, b8)
	cfg.Blocks = map[uint]*BasicBlock{
		b0.id: b0,
		b1.id: b1,
		b2.id: b2,
		b3.id: b3,
		b4.id: b4,
		b5.id: b5,
		b6.id: b6,
		b7.id: b7,
		b8.id: b8,
	}

	cfg.AddSuc(b0.id, b1.id)
	cfg.AddSuc(b1.id, b2.id, b5.id)
	cfg.AddSuc(b2.id, b3.id)
	cfg.AddSuc(b3.id, b1.id, b4.id)
	cfg.AddSuc(b4.id)
	cfg.AddSuc(b5.id, b6.id, b8.id)
	cfg.AddSuc(b6.id, b7.id)
	cfg.AddSuc(b7.id, b3.id)
	cfg.AddSuc(b8.id, b7.id)

	cfg.AddPred(b0.id)
	cfg.AddPred(b1.id, b0.id, b3.id)
	cfg.AddPred(b2.id, b1.id)
	cfg.AddPred(b3.id, b2.id, b7.id)
	cfg.AddPred(b4.id, b3.id)
	cfg.AddPred(b5.id, b1.id)
	cfg.AddPred(b6.id, b5.id)
	cfg.AddPred(b7.id, b6.id, b8.id)
	cfg.AddPred(b8.id, b5.id)

	tests := []struct {
		id     uint
		blocks []*BasicBlock
	}{
		{b0.id, []*BasicBlock{}},
		{b1.id, []*BasicBlock{b1}},
		{b2.id, []*BasicBlock{b3}},
		{b3.id, []*BasicBlock{b1}},
		{b4.id, []*BasicBlock{}},
		{b5.id, []*BasicBlock{b3}},
		{b6.id, []*BasicBlock{b7}},
		{b7.id, []*BasicBlock{b3}},
		{b8.id, []*BasicBlock{b7}},
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
