package selector

import (
	"strings"
	"testing"
)

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

func mustParse(t *testing.T, src string) *File {
	t.Helper()
	f, err := ParseFile(src)
	if err != nil {
		t.Fatalf("ParseFile failed: %v", err)
	}
	return f
}

func mustFail(t *testing.T, src string) {
	t.Helper()
	_, err := ParseFile(src)
	if err == nil {
		t.Fatal("expected parse error, got nil")
	}
}

// ---------------------------------------------------------------------------
// File / Header
// ---------------------------------------------------------------------------

func TestEmptyFile(t *testing.T) {
	f := mustParse(t, "")
	if f.Header != nil {
		t.Fatal("expected nil Header for empty file")
	}
	if len(f.Targets) != 0 {
		t.Fatalf("expected no targets, got %d", len(f.Targets))
	}
}

func TestTopLevelHeader(t *testing.T) {
	f := mustParse(t, `
		header {
			ISA: "RV64IMAFD";
			ABI: "LP64D";
			version: 3;
		}
	`)
	if f.Header == nil {
		t.Fatal("expected non-nil Header")
	}
	if len(f.Header.Fields) != 3 {
		t.Fatalf("expected 3 header fields, got %d", len(f.Header.Fields))
	}
	checkField := func(i int, key string, kind ValueKind, lit string) {
		t.Helper()
		hf := f.Header.Fields[i]
		if hf.Key != key {
			t.Errorf("field[%d].Key = %q, want %q", i, hf.Key, key)
		}
		if hf.Val.Kind != kind {
			t.Errorf("field[%d].Val.Kind = %v, want %v", i, hf.Val.Kind, kind)
		}
		if hf.Val.Lit != lit {
			t.Errorf("field[%d].Val.Lit = %q, want %q", i, hf.Val.Lit, lit)
		}
	}
	checkField(0, "ISA", ValueString, "RV64IMAFD")
	checkField(1, "ABI", ValueString, "LP64D")
	checkField(2, "version", ValueNumber, "3")
}

// ---------------------------------------------------------------------------
// TargetBlock / Block / Rule containment
// ---------------------------------------------------------------------------

func TestTargetBlockWithHeaderAndBlock(t *testing.T) {
	f := mustParse(t, `
		target rv64 {
			header {
				ISA: "RV64GC";
			}
			block arith {
				rule nop {
					pattern nop;
				}
			}
		}
	`)
	if len(f.Targets) != 1 {
		t.Fatalf("len(Targets) = %d, want 1", len(f.Targets))
	}
	tb := f.Targets[0]
	if tb.Name != "rv64" {
		t.Fatalf("target name = %q, want rv64", tb.Name)
	}
	if len(tb.Items) != 2 {
		t.Fatalf("len(tb.Items) = %d, want 2", len(tb.Items))
	}
	hdr, ok := tb.Items[0].(*Header)
	if !ok {
		t.Fatalf("tb.Items[0] is %T, want *Header", tb.Items[0])
	}
	if len(hdr.Fields) != 1 || hdr.Fields[0].Key != "ISA" {
		t.Fatalf("unexpected header: %+v", hdr)
	}
	blk, ok := tb.Items[1].(*Block)
	if !ok {
		t.Fatalf("tb.Items[1] is %T, want *Block", tb.Items[1])
	}
	if blk.Name != "arith" {
		t.Fatalf("block name = %q, want arith", blk.Name)
	}
	if len(blk.Items) != 1 {
		t.Fatalf("len(blk.Items) = %d, want 1", len(blk.Items))
	}
	rule, ok := blk.Items[0].(*Rule)
	if !ok {
		t.Fatalf("blk.Items[0] is %T, want *Rule", blk.Items[0])
	}
	if rule.Name != "nop" {
		t.Fatalf("rule name = %q, want nop", rule.Name)
	}
}

func TestMultipleTargets(t *testing.T) {
	f := mustParse(t, `
		target a { rule r1 { pattern p1; } }
		target b { rule r2 { pattern p2; } }
	`)
	if len(f.Targets) != 2 {
		t.Fatalf("len(Targets) = %d, want 2", len(f.Targets))
	}
	if f.Targets[0].Name != "a" || f.Targets[1].Name != "b" {
		t.Fatalf("target names: %v, %v", f.Targets[0].Name, f.Targets[1].Name)
	}
}

// ---------------------------------------------------------------------------
// Rule sections — pattern, cost, cond
// ---------------------------------------------------------------------------

func TestPatternCostCond(t *testing.T) {
	f := mustParse(t, `
		target rv64 {
			rule ADDrr {
				pattern add($rs1, $rs2);
				cost 1;
				cond SImmFits12($val), !LargeImm($val);
			}
		}
	`)
	rule := f.Targets[0].Items[0].(*Rule)
	if len(rule.Sections) != 3 {
		t.Fatalf("len(Sections) = %d, want 3", len(rule.Sections))
	}

	// pattern
	ps, ok := rule.Sections[0].(*PatternSection)
	if !ok {
		t.Fatalf("section[0] is %T, want *PatternSection", rule.Sections[0])
	}
	if ps.Pattern.Name != "add" {
		t.Fatalf("pattern name = %q, want add", ps.Pattern.Name)
	}
	if len(ps.Pattern.Args) != 2 {
		t.Fatalf("pattern args = %d, want 2", len(ps.Pattern.Args))
	}
	arg0 := ps.Pattern.Args[0].(*Value)
	if arg0.Kind != ValueRef || arg0.Ref != "rs1" {
		t.Fatalf("arg0 = %+v, want Ref rs1", arg0)
	}

	// cost
	cs, ok := rule.Sections[1].(*CostSection)
	if !ok {
		t.Fatalf("section[1] is %T, want *CostSection", rule.Sections[1])
	}
	if cs.Cost != "1" {
		t.Fatalf("cost = %q, want 1", cs.Cost)
	}

	// cond with two predicates
	cond, ok := rule.Sections[2].(*CondSection)
	if !ok {
		t.Fatalf("section[2] is %T, want *CondSection", rule.Sections[2])
	}
	if len(cond.Predicates) != 2 {
		t.Fatalf("len(Predicates) = %d, want 2", len(cond.Predicates))
	}
	if cond.Predicates[0].Negated || cond.Predicates[0].Name != "SImmFits12" {
		t.Fatalf("pred[0] = %+v", cond.Predicates[0])
	}
	if !cond.Predicates[1].Negated || cond.Predicates[1].Name != "LargeImm" {
		t.Fatalf("pred[1] = %+v", cond.Predicates[1])
	}
}

// ---------------------------------------------------------------------------
// MatchSection — bindings, commutative, nested PatternDecl
// ---------------------------------------------------------------------------

func TestMatchSection(t *testing.T) {
	f := mustParse(t, `
		target rv64 {
			rule ADDrr {
				match {
					commutative;
					out $rd  : GPR:virt;
					in  $rs1 : GPR:virt;
					in  $rs2 : GPR:virt;
					pattern add($rs1, $rs2);
				}
				cost 1;
			}
		}
	`)
	rule := f.Targets[0].Items[0].(*Rule)
	ms, ok := rule.Sections[0].(*MatchSection)
	if !ok {
		t.Fatalf("section[0] is %T, want *MatchSection", rule.Sections[0])
	}
	if len(ms.Items) != 5 {
		t.Fatalf("len(MatchItems) = %d, want 5", len(ms.Items))
	}

	// commutative
	attr, ok := ms.Items[0].(*MatchAttr)
	if !ok {
		t.Fatalf("item[0] is %T, want *MatchAttr", ms.Items[0])
	}
	if !strings.EqualFold(attr.Name, "commutative") {
		t.Fatalf("attr.Name = %q", attr.Name)
	}

	// out $rd : GPR:virt
	outB, ok := ms.Items[1].(*Binding)
	if !ok {
		t.Fatalf("item[1] is %T, want *Binding", ms.Items[1])
	}
	if outB.Dir != "out" || outB.Name != "$rd" {
		t.Fatalf("out binding = %+v", outB)
	}
	if outB.Type.Kind != "GPR" || outB.Type.Sub != "virt" {
		t.Fatalf("out type = %+v", outB.Type)
	}

	// in $rs1 : GPR:virt
	inB, ok := ms.Items[2].(*Binding)
	if !ok {
		t.Fatalf("item[2] is %T, want *Binding", ms.Items[2])
	}
	if inB.Dir != "in" || inB.Name != "$rs1" {
		t.Fatalf("in binding = %+v", inB)
	}
	if inB.Type.Kind != "GPR" || inB.Type.Sub != "virt" {
		t.Fatalf("in type = %+v", inB.Type)
	}

	// pattern inside match
	pd, ok := ms.Items[4].(*PatternDecl)
	if !ok {
		t.Fatalf("item[4] is %T, want *PatternDecl", ms.Items[4])
	}
	if pd.Pattern.Name != "add" || len(pd.Pattern.Args) != 2 {
		t.Fatalf("pattern decl = %+v", pd.Pattern)
	}
}

func TestBindingWithDefault(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				match {
					in $x : imm = 0;
				}
			}
		}
	`)
	ms := f.Targets[0].Items[0].(*Rule).Sections[0].(*MatchSection)
	b := ms.Items[0].(*Binding)
	if b.Default == nil {
		t.Fatal("expected non-nil Default")
	}
	if b.Default.Kind != ValueNumber || b.Default.Lit != "0" {
		t.Fatalf("Default = %+v", b.Default)
	}
}

// ---------------------------------------------------------------------------
// EmitSection — instr, template, comment
// ---------------------------------------------------------------------------

func TestEmitSection(t *testing.T) {
	f := mustParse(t, `
		target rv64 {
			rule ADDrr {
				emit {
					instr {
						opcode: "add";
						dst: $rd;
						src: [$rs1, $rs2];
						def: $rd;
						uses: $rs1, $rs2;
					};
					template "# synthetic add";
					comment "two-operand form";
				}
			}
		}
	`)
	rule := f.Targets[0].Items[0].(*Rule)
	es, ok := rule.Sections[0].(*EmitSection)
	if !ok {
		t.Fatalf("section[0] is %T, want *EmitSection", rule.Sections[0])
	}
	if len(es.Stmts) != 3 {
		t.Fatalf("len(Stmts) = %d, want 3", len(es.Stmts))
	}

	// instr
	instr, ok := es.Stmts[0].(*InstrStmt)
	if !ok {
		t.Fatalf("stmt[0] is %T, want *InstrStmt", es.Stmts[0])
	}
	if len(instr.Fields) != 5 {
		t.Fatalf("len(InstrFields) = %d, want 5", len(instr.Fields))
	}
	opcodeF := instr.Fields[0]
	if opcodeF.Name != "opcode" || opcodeF.Val.Kind != ValueString || opcodeF.Val.Lit != "add" {
		t.Fatalf("opcode field = %+v", opcodeF)
	}
	dstF := instr.Fields[1]
	if dstF.Name != "dst" || dstF.Val.Kind != ValueRef || dstF.Val.Ref != "rd" {
		t.Fatalf("dst field = %+v", dstF)
	}
	srcF := instr.Fields[2]
	if srcF.Name != "src" || srcF.Val.Kind != ValueList || len(srcF.Val.Elems) != 2 {
		t.Fatalf("src field = %+v", srcF)
	}
	usesF := instr.Fields[4]
	if usesF.Name != "uses" || usesF.Val.Kind != ValueList || len(usesF.Val.Elems) != 2 {
		t.Fatalf("uses field = %+v", usesF)
	}

	// template
	tmpl, ok := es.Stmts[1].(*TemplateStmt)
	if !ok {
		t.Fatalf("stmt[1] is %T, want *TemplateStmt", es.Stmts[1])
	}
	if tmpl.Text != "# synthetic add" {
		t.Fatalf("template text = %q", tmpl.Text)
	}

	// comment
	cmt, ok := es.Stmts[2].(*CommentStmt)
	if !ok {
		t.Fatalf("stmt[2] is %T, want *CommentStmt", es.Stmts[2])
	}
	if cmt.Text != "two-operand form" {
		t.Fatalf("comment text = %q", cmt.Text)
	}
}

// ---------------------------------------------------------------------------
// LegalizeSection
// ---------------------------------------------------------------------------

func TestLegalizeSection(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				legalize {
					require IsSupported($x);
					spill $x;
					reload $y;
					move $x -> $y;
					rewrite expand($x);
					if NeedsExpand($v) then expand2($v);
				}
			}
		}
	`)
	rule := f.Targets[0].Items[0].(*Rule)
	ls, ok := rule.Sections[0].(*LegalizeSection)
	if !ok {
		t.Fatalf("section[0] is %T, want *LegalizeSection", rule.Sections[0])
	}
	if len(ls.Items) != 6 {
		t.Fatalf("len(Items) = %d, want 6", len(ls.Items))
	}

	req := ls.Items[0].(*RequireItem)
	if req.Pred.Name != "IsSupported" || len(req.Pred.Args) != 1 {
		t.Fatalf("require = %+v", req)
	}
	sp := ls.Items[1].(*SpillItem)
	if sp.Name != "$x" {
		t.Fatalf("spill name = %q", sp.Name)
	}
	rl := ls.Items[2].(*ReloadItem)
	if rl.Name != "$y" {
		t.Fatalf("reload name = %q", rl.Name)
	}
	mv := ls.Items[3].(*MoveItem)
	if mv.From.Ref != "x" || mv.To.Ref != "y" {
		t.Fatalf("move from=%+v to=%+v", mv.From, mv.To)
	}
	rw := ls.Items[4].(*RewriteItem)
	if rw.Expr.Name != "expand" || len(rw.Expr.Args) != 1 {
		t.Fatalf("rewrite = %+v", rw)
	}
	ifIt := ls.Items[5].(*IfItem)
	if ifIt.Pred.Name != "NeedsExpand" {
		t.Fatalf("if pred = %+v", ifIt.Pred)
	}
	act, ok := ifIt.Action.(*RewriteExpr)
	if !ok {
		t.Fatalf("action is %T, want *RewriteExpr", ifIt.Action)
	}
	if act.Name != "expand2" {
		t.Fatalf("action name = %q", act.Name)
	}
}

func TestLegalizeIfSpillReloadMove(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				legalize {
					if Pressure($v) then spill $v;
					if Dirty($v)    then reload $v;
					if Alias($a)    then move $a -> $b;
				}
			}
		}
	`)
	ls := f.Targets[0].Items[0].(*Rule).Sections[0].(*LegalizeSection)
	if _, ok := ls.Items[0].(*IfItem).Action.(*SpillAction); !ok {
		t.Fatalf("expected SpillAction")
	}
	if _, ok := ls.Items[1].(*IfItem).Action.(*ReloadAction); !ok {
		t.Fatalf("expected ReloadAction")
	}
	ma, ok := ls.Items[2].(*IfItem).Action.(*MoveAction)
	if !ok {
		t.Fatalf("expected MoveAction")
	}
	if ma.From.Ref != "a" || ma.To.Ref != "b" {
		t.Fatalf("move action from=%+v to=%+v", ma.From, ma.To)
	}
}

// ---------------------------------------------------------------------------
// Value forms
// ---------------------------------------------------------------------------

func TestValueListAndRecord(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				emit {
					instr {
						src: [$a, $b, 3];
						dst: { base = $sp, offset = 16 };
					};
				}
			}
		}
	`)
	fields := f.Targets[0].Items[0].(*Rule).Sections[0].(*EmitSection).Stmts[0].(*InstrStmt).Fields
	srcF := fields[0]
	if srcF.Val.Kind != ValueList || len(srcF.Val.Elems) != 3 {
		t.Fatalf("src = %+v", srcF.Val)
	}
	if srcF.Val.Elems[2].Kind != ValueNumber || srcF.Val.Elems[2].Lit != "3" {
		t.Fatalf("src[2] = %+v", srcF.Val.Elems[2])
	}
	dstF := fields[1]
	if dstF.Val.Kind != ValueRecord || len(dstF.Val.Fields) != 2 {
		t.Fatalf("dst = %+v", dstF.Val)
	}
	if dstF.Val.Fields[0].Key != "base" || dstF.Val.Fields[0].Val.Ref != "sp" {
		t.Fatalf("dst.base = %+v", dstF.Val.Fields[0])
	}
}

func TestValuePatternExpr(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				pattern load(add($rs1, $offs));
			}
		}
	`)
	ps := f.Targets[0].Items[0].(*Rule).Sections[0].(*PatternSection)
	if ps.Pattern.Name != "load" || len(ps.Pattern.Args) != 1 {
		t.Fatalf("load pattern = %+v", ps.Pattern)
	}
	inner, ok := ps.Pattern.Args[0].(*PatternExpr)
	if !ok {
		t.Fatalf("inner arg is %T, want *PatternExpr", ps.Pattern.Args[0])
	}
	if inner.Name != "add" || len(inner.Args) != 2 {
		t.Fatalf("inner = %+v", inner)
	}
}

func TestValueRefInInstr(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				emit {
					instr { opcode: "mv"; dst: $rd; src: [$rs]; };
				}
			}
		}
	`)
	fields := f.Targets[0].Items[0].(*Rule).Sections[0].(*EmitSection).Stmts[0].(*InstrStmt).Fields
	dst := fields[1]
	if dst.Val.Kind != ValueRef || dst.Val.Ref != "rd" {
		t.Fatalf("dst ref = %+v", dst.Val)
	}
}

// ---------------------------------------------------------------------------
// TypeSpec variants
// ---------------------------------------------------------------------------

func TestTypeSpecVariants(t *testing.T) {
	f := mustParse(t, `
		target t {
			rule r {
				match {
					in  $a : imm;
					in  $b : GPR:virt;
					out $c : GPR:virt;
					in  $d : label;
				}
			}
		}
	`)
	ms := f.Targets[0].Items[0].(*Rule).Sections[0].(*MatchSection)
	a := ms.Items[0].(*Binding)
	if a.Type.Kind != "imm" || a.Type.Sub != "" {
		t.Fatalf("a type = %+v", a.Type)
	}
	b := ms.Items[1].(*Binding)
	if b.Type.Kind != "GPR" || b.Type.Sub != "virt" {
		t.Fatalf("b type = %+v", b.Type)
	}
	c := ms.Items[2].(*Binding)
	if c.Type.Kind != "GPR" || c.Type.Sub != "virt" {
		t.Fatalf("c type = %+v", c.Type)
	}
	d := ms.Items[3].(*Binding)
	if d.Type.Kind != "label" {
		t.Fatalf("d type = %+v", d.Type)
	}
	mustFail(t, `target t { rule r { match { out $c : GPR:virt:$c; } } }`)
}

// ---------------------------------------------------------------------------
// Header as BlockItem inside target / block
// ---------------------------------------------------------------------------

func TestHeaderAsBlockItem(t *testing.T) {
	f := mustParse(t, `
		target rv64 {
			header { ABI: "LP64D"; }
			block grp {
				header { fmt: "elf"; }
				rule nop { pattern nop; }
			}
		}
	`)
	tb := f.Targets[0]
	hdr := tb.Items[0].(*Header)
	if len(hdr.Fields) != 1 || hdr.Fields[0].Key != "ABI" {
		t.Fatalf("target header = %+v", hdr)
	}
	blk := tb.Items[1].(*Block)
	blkHdr := blk.Items[0].(*Header)
	if blkHdr.Fields[0].Key != "fmt" {
		t.Fatalf("block header = %+v", blkHdr)
	}
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

func TestUnterminatedTargetBlock(t *testing.T) {
	mustFail(t, `target rv64 {`)
}

func TestUnterminatedRule(t *testing.T) {
	mustFail(t, `target t { rule r { `)
}

func TestUnterminatedHeader(t *testing.T) {
	mustFail(t, `header {`)
}

func TestUnexpectedTopLevel(t *testing.T) {
	mustFail(t, `rule r { }`)
}

func TestBadCostNotNumber(t *testing.T) {
	mustFail(t, `target t { rule r { cost "x"; } }`)
}

func TestMissingArrowInMove(t *testing.T) {
	mustFail(t, `target t { rule r { legalize { move $a $b; } } }`)
}

// ---------------------------------------------------------------------------
// Full round-trip smoke test using all sections
// ---------------------------------------------------------------------------

func TestFullRuleSmokeTest(t *testing.T) {
	src := `
	// RISC-V 64-bit add immediate (small immediate)
	header {
		arch: "rv64imafd";
	}

	target rv64imafd {
		header {
			ABI: "LP64D";
		}
		block integer {
			rule ADDri {
				match {
					commutative;
					out $rd  : GPR:virt;
					in  $rs1 : GPR:virt;
					in  $val : imm;
					pattern add($rs1, $val);
				}
				pattern add($rs1, $val);
				cost 1;
				cond SImmFits12($val);
				legalize {
					require SImmFits12($val);
					if !SImmFits12($val) then ADDri_large($rs1, $val);
				}
				emit {
					instr {
						opcode: "addi";
						dst: $rd;
						src: [$rs1, $val];
						def: $rd;
						uses: $rs1, $val;
					};
				}
			}
		}
	}
	`
	f, err := ParseFile(src)
	if err != nil {
		t.Fatalf("ParseFile: %v", err)
	}

	// top-level header
	if f.Header == nil || len(f.Header.Fields) != 1 {
		t.Fatalf("top-level header = %+v", f.Header)
	}
	if f.Header.Fields[0].Key != "arch" {
		t.Fatalf("header field key = %q", f.Header.Fields[0].Key)
	}

	// target
	if len(f.Targets) != 1 || f.Targets[0].Name != "rv64imafd" {
		t.Fatalf("targets = %v", f.Targets)
	}
	tb := f.Targets[0]
	if len(tb.Items) != 2 {
		t.Fatalf("target items = %d", len(tb.Items))
	}

	blk := tb.Items[1].(*Block)
	if blk.Name != "integer" {
		t.Fatalf("block name = %q", blk.Name)
	}
	rule := blk.Items[0].(*Rule)
	if rule.Name != "ADDri" {
		t.Fatalf("rule name = %q", rule.Name)
	}

	// 5 sections: match, pattern, cost, cond, legalize, emit
	if len(rule.Sections) != 6 {
		t.Fatalf("rule sections = %d, want 6", len(rule.Sections))
	}

	// match section — 5 items: commutative + 3 bindings + patternDecl
	ms := rule.Sections[0].(*MatchSection)
	if len(ms.Items) != 5 {
		t.Fatalf("match items = %d, want 5", len(ms.Items))
	}

	// pattern section
	ps := rule.Sections[1].(*PatternSection)
	if ps.Pattern.Name != "add" {
		t.Fatalf("pattern = %q", ps.Pattern.Name)
	}

	// cost
	cs := rule.Sections[2].(*CostSection)
	if cs.Cost != "1" {
		t.Fatalf("cost = %q", cs.Cost)
	}

	// cond
	cond := rule.Sections[3].(*CondSection)
	if len(cond.Predicates) != 1 || cond.Predicates[0].Name != "SImmFits12" {
		t.Fatalf("cond = %+v", cond)
	}

	// legalize
	ls := rule.Sections[4].(*LegalizeSection)
	if len(ls.Items) != 2 {
		t.Fatalf("legalize items = %d, want 2", len(ls.Items))
	}

	// emit
	es := rule.Sections[5].(*EmitSection)
	if len(es.Stmts) != 1 {
		t.Fatalf("emit stmts = %d, want 1", len(es.Stmts))
	}
	instr := es.Stmts[0].(*InstrStmt)
	if len(instr.Fields) != 5 {
		t.Fatalf("instr fields = %d, want 5", len(instr.Fields))
	}
	if instr.Fields[0].Val.Lit != "addi" {
		t.Fatalf("opcode = %q", instr.Fields[0].Val.Lit)
	}
}
