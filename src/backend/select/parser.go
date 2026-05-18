package selector

import (
	"fmt"
	"io"
	"os"
	"strings"
)

// ParseFile parses a descriptor from source text and returns the AST root.
func ParseFile(src string) (*File, error) {
	return newParser(src).parseFile()
}

// ParseFileReader parses a descriptor from an io.Reader.
func ParseFileReader(r io.Reader) (*File, error) {
	b, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	return ParseFile(string(b))
}

// ParseFilePath parses a descriptor file from disk.
func ParseFilePath(path string) (*File, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ParseFile(string(b))
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

type parser struct {
	lx   *Lexer
	cur  Token
	peek Token
}

func newParser(src string) *parser {
	lx := NewLexer(src)
	p := &parser{lx: lx}
	p.cur = lx.NextToken()
	p.peek = lx.NextToken()
	return p
}

func (p *parser) advance() {
	p.cur = p.peek
	p.peek = p.lx.NextToken()
}

func (p *parser) expect(k TokenType) error {
	if p.cur.Kind != k {
		return p.errorf("expected %s, got %s", tokenName(k), p.cur.Lit)
	}
	p.advance()
	return nil
}

func (p *parser) expectKeyword(kw string) error {
	if !p.is(kw) {
		return p.errorf("expected %q, got %q", kw, p.cur.Lit)
	}
	p.advance()
	return nil
}

func (p *parser) expectIdent(ctx string) (string, error) {
	if p.cur.Kind != TokIdent {
		return "", p.errorf("expected identifier (%s), got %s", ctx, p.cur.Lit)
	}
	v := p.cur.Lit
	p.advance()
	return v, nil
}

// is reports whether cur is a keyword (case-insensitive ident match).
func (p *parser) is(kw string) bool {
	return p.cur.Kind == TokIdent && strings.EqualFold(p.cur.Lit, kw)
}

func (p *parser) errorf(format string, args ...any) error {
	return fmt.Errorf("parse error at %d: %s", p.cur.Pos, fmt.Sprintf(format, args...))
}

// ---------------------------------------------------------------------------
// File = [Header] { TargetBlock } ;
// ---------------------------------------------------------------------------

func (p *parser) parseFile() (*File, error) {
	file := &File{}

	// optional top-level header
	if p.is("header") && p.peek.Kind == TokLBrace {
		hdr, err := p.parseHeader()
		if err != nil {
			return nil, err
		}
		file.Header = hdr
	}

	for p.cur.Kind != TokEOF {
		if !p.is("target") {
			return nil, p.errorf("expected \"target\", got %q", p.cur.Lit)
		}
		tb, err := p.parseTargetBlock()
		if err != nil {
			return nil, err
		}
		file.Targets = append(file.Targets, tb)
	}
	return file, nil
}

// ---------------------------------------------------------------------------
// Header = "header" "{" { HeaderField } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseHeader() (*Header, error) {
	if err := p.expectKeyword("header"); err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	hdr := &Header{}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated header block")
		}
		f, err := p.parseHeaderField()
		if err != nil {
			return nil, err
		}
		hdr.Fields = append(hdr.Fields, f)
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return hdr, nil
}

// HeaderField = Ident ":" Value ";" ;
func (p *parser) parseHeaderField() (*HeaderField, error) {
	key, err := p.expectIdent("header field key")
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokColon); err != nil {
		return nil, err
	}
	val, err := p.parseValue()
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return &HeaderField{Key: key, Val: val}, nil
}

// ---------------------------------------------------------------------------
// TargetBlock = "target" Ident "{" { BlockItem } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseTargetBlock() (*TargetBlock, error) {
	if err := p.expectKeyword("target"); err != nil {
		return nil, err
	}
	name, err := p.expectIdent("target name")
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	tb := &TargetBlock{Name: name}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated target block %q", name)
		}
		item, err := p.parseBlockItem()
		if err != nil {
			return nil, err
		}
		tb.Items = append(tb.Items, item)
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return tb, nil
}

// ---------------------------------------------------------------------------
// BlockItem = Header | Block | Rule ;
// ---------------------------------------------------------------------------

func (p *parser) parseBlockItem() (BlockItem, error) {
	switch {
	case p.is("header") && p.peek.Kind == TokLBrace:
		return p.parseHeader()
	case p.is("block"):
		return p.parseBlock()
	case p.is("rule"):
		return p.parseRule()
	default:
		return nil, p.errorf("expected block item (header/block/rule), got %q", p.cur.Lit)
	}
}

// ---------------------------------------------------------------------------
// Block = "block" Ident "{" { BlockItem } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseBlock() (*Block, error) {
	if err := p.expectKeyword("block"); err != nil {
		return nil, err
	}
	name, err := p.expectIdent("block name")
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	blk := &Block{Name: name}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated block %q", name)
		}
		item, err := p.parseBlockItem()
		if err != nil {
			return nil, err
		}
		blk.Items = append(blk.Items, item)
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return blk, nil
}

// ---------------------------------------------------------------------------
// Rule = "rule" Ident "{" { RuleSection } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseRule() (*Rule, error) {
	if err := p.expectKeyword("rule"); err != nil {
		return nil, err
	}

	name, err := p.expectIdent("rule name")
	if err != nil {
		return nil, err
	}

	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}

	rule := &Rule{Name: name}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated rule %q", name)
		}

		sec, err := p.parseRuleSection()
		if err != nil {
			return nil, err
		}

		rule.Sections = append(rule.Sections, sec)
	}

	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}

	return rule, nil
}

// ---------------------------------------------------------------------------
// RuleSection = MatchSection | LegalizeSection | PatternSection
//             | CostSection | CondSection | EmitSection ;
// ---------------------------------------------------------------------------

func (p *parser) parseRuleSection() (RuleSection, error) {
	switch {
	case p.is("match"):
		return p.parseMatchSection()
	case p.is("legalize"):
		return p.parseLegalizeSection()
	case p.is("pattern"):
		return p.parsePatternSection()
	case p.is("cost"):
		return p.parseCostSection()
	case p.is("cond"):
		return p.parseCondSection()
	case p.is("emit"):
		return p.parseEmitSection()
	default:
		return nil, p.errorf("expected rule section (match/legalize/pattern/cost/cond/emit), got %q", p.cur.Lit)
	}
}

// ---------------------------------------------------------------------------
// MatchSection = "match" "{" { Binding | PatternDecl | MatchAttr } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseMatchSection() (*MatchSection, error) {
	if err := p.expectKeyword("match"); err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}

	ms := &MatchSection{}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated match section")
		}

		item, err := p.parseMatchItem()
		if err != nil {
			return nil, err
		}

		ms.Items = append(ms.Items, item)
	}

	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}

	return ms, nil
}

func (p *parser) parseMatchItem() (MatchItem, error) {
	switch {
	case p.is("in") || p.is("out") || p.is("temp"):
		return p.parseBinding()
	case p.is("temps"):
		return p.parseBinding()
	case p.is("pattern"):
		return p.parsePatternDeclItem()
	case p.is("commutative"):
		lit := p.cur.Lit
		p.advance()
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &MatchAttr{Name: lit}, nil
	default:
		return nil, p.errorf("expected match item (in/out/temp/pattern/commutative), got %q", p.cur.Lit)
	}
}

// Binding = ("in" | "out" | "temp") Ident ":" TypeSpec [ "=" Value ] ";" ;
func (p *parser) parseBinding() (*Binding, error) {
	dir := p.cur.Lit
	p.advance()

	name, err := p.expectIdent("binding name")
	if err != nil {
		return nil, err
	}

	if err := p.expect(TokColon); err != nil {
		return nil, err
	}

	ts, err := p.parseTypeSpec()
	if err != nil {
		return nil, err
	}

	b := &Binding{Dir: dir, Name: name, Type: ts}
	if p.cur.Kind == TokEqual {
		p.advance()
		def, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		b.Default = def
	}
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return b, nil
}

// TypeSpec = Ident [ ":" Ident ] ;
func (p *parser) parseTypeSpec() (TypeSpec, error) {
	kind, err := p.expectIdent("type kind")
	if err != nil {
		return TypeSpec{}, err
	}

	ts := TypeSpec{Kind: kind}

	// Optional second component: ":" Ident.
	if p.cur.Kind == TokColon && p.peek.Kind == TokIdent {
		p.advance() // consume ":"
		sub, err := p.expectIdent("type sub")
		if err != nil {
			return TypeSpec{}, err
		}
		ts.Sub = sub
	}

	return ts, nil
}

// PatternDecl (inside match) = "pattern" PatternExpr ";"
func (p *parser) parsePatternDeclItem() (*PatternDecl, error) {
	if err := p.expectKeyword("pattern"); err != nil {
		return nil, err
	}

	pat, err := p.parsePatternExpr()
	if err != nil {
		return nil, err
	}

	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}

	return &PatternDecl{Pattern: pat}, nil
}

// ---------------------------------------------------------------------------
// LegalizeSection = "legalize" "{" { LegalizeItem } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseLegalizeSection() (*LegalizeSection, error) {
	if err := p.expectKeyword("legalize"); err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	ls := &LegalizeSection{}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated legalize section")
		}
		item, err := p.parseLegalizeItem()
		if err != nil {
			return nil, err
		}
		ls.Items = append(ls.Items, item)
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return ls, nil
}

func (p *parser) parseLegalizeItem() (LegalizeItem, error) {
	switch {
	case p.is("require"):
		p.advance()
		pred, err := p.parsePredicate()
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &RequireItem{Pred: pred}, nil

	case p.is("rewrite"):
		p.advance()
		rw, err := p.parseRewriteExpr()
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &RewriteItem{Expr: rw}, nil

	case p.is("spill"):
		p.advance()
		name, err := p.expectIdent("spill operand")
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &SpillItem{Name: name}, nil

	case p.is("reload"):
		p.advance()
		name, err := p.expectIdent("reload operand")
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &ReloadItem{Name: name}, nil

	case p.is("move"):
		p.advance()
		from, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		if p.cur.Kind != TokArrow {
			return nil, p.errorf("expected \"->\" in move item")
		}
		p.advance()
		to, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &MoveItem{From: from, To: to}, nil

	case p.is("if"):
		p.advance()
		pred, err := p.parsePredicate()
		if err != nil {
			return nil, err
		}
		if err := p.expectKeyword("then"); err != nil {
			return nil, err
		}
		action, err := p.parseLegalizeAction()
		if err != nil {
			return nil, err
		}
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &IfItem{Pred: pred, Action: action}, nil

	default:
		return nil, p.errorf("expected legalize item, got %q", p.cur.Lit)
	}
}

func (p *parser) parseLegalizeAction() (LegalizeAction, error) {
	switch {
	case p.is("spill"):
		p.advance()
		name, err := p.expectIdent("spill operand")
		if err != nil {
			return nil, err
		}
		return &SpillAction{Name: name}, nil

	case p.is("reload"):
		p.advance()
		name, err := p.expectIdent("reload operand")
		if err != nil {
			return nil, err
		}
		return &ReloadAction{Name: name}, nil

	case p.is("move"):
		p.advance()
		from, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		if p.cur.Kind != TokArrow {
			return nil, p.errorf("expected \"->\" in move action")
		}
		p.advance()
		to, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		return &MoveAction{From: from, To: to}, nil

	default:
		// RewriteExpr
		rw, err := p.parseRewriteExpr()
		if err != nil {
			return nil, err
		}
		return rw, nil
	}
}

func (p *parser) parseRewriteExpr() (*RewriteExpr, error) {
	name, err := p.expectIdent("rewrite name")
	if err != nil {
		return nil, err
	}
	rw := &RewriteExpr{Name: name}
	if p.cur.Kind == TokLParen {
		p.advance()
		if p.cur.Kind != TokRParen {
			args, err := p.parseValueList()
			if err != nil {
				return nil, err
			}
			rw.Args = args
		}
		if err := p.expect(TokRParen); err != nil {
			return nil, err
		}
	}
	return rw, nil
}

// ---------------------------------------------------------------------------
// PatternSection = "pattern" PatternExpr ";" ;
// ---------------------------------------------------------------------------

func (p *parser) parsePatternSection() (*PatternSection, error) {
	if err := p.expectKeyword("pattern"); err != nil {
		return nil, err
	}
	pat, err := p.parsePatternExpr()
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return &PatternSection{Pattern: pat}, nil
}

// ---------------------------------------------------------------------------
// CostSection = "cost" Number ";" ;
// ---------------------------------------------------------------------------

func (p *parser) parseCostSection() (*CostSection, error) {
	if err := p.expectKeyword("cost"); err != nil {
		return nil, err
	}
	if p.cur.Kind != TokNumber {
		return nil, p.errorf("expected number after \"cost\", got %q", p.cur.Lit)
	}
	n := p.cur.Lit
	p.advance()
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return &CostSection{Cost: n}, nil
}

// ---------------------------------------------------------------------------
// CondSection = "cond" PredicateList ";" ;
// PredicateList = Predicate { "," Predicate } ;
// ---------------------------------------------------------------------------

func (p *parser) parseCondSection() (*CondSection, error) {
	if err := p.expectKeyword("cond"); err != nil {
		return nil, err
	}
	var preds []*Predicate
	for {
		pred, err := p.parsePredicate()
		if err != nil {
			return nil, err
		}
		preds = append(preds, pred)
		if p.cur.Kind != TokComma {
			break
		}
		p.advance()
	}
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return &CondSection{Predicates: preds}, nil
}

// Predicate = ["!"] Ident [ "(" [ ValueList ] ")" ] ;
func (p *parser) parsePredicate() (*Predicate, error) {
	neg := false
	if p.cur.Kind == TokBang {
		neg = true
		p.advance()
	}
	name, err := p.expectIdent("predicate name")
	if err != nil {
		return nil, err
	}
	pred := &Predicate{Negated: neg, Name: name}
	if p.cur.Kind == TokLParen {
		p.advance()
		if p.cur.Kind != TokRParen {
			args, err := p.parseValueList()
			if err != nil {
				return nil, err
			}
			pred.Args = args
		}
		if err := p.expect(TokRParen); err != nil {
			return nil, err
		}
	}
	return pred, nil
}

// ---------------------------------------------------------------------------
// EmitSection = "emit" "{" { EmitStmt } "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseEmitSection() (*EmitSection, error) {
	if err := p.expectKeyword("emit"); err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	es := &EmitSection{}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated emit section")
		}
		stmt, err := p.parseEmitStmt()
		if err != nil {
			return nil, err
		}
		es.Stmts = append(es.Stmts, stmt)
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return es, nil
}

func (p *parser) parseEmitStmt() (EmitStmt, error) {
	switch {
	case p.is("instr"):
		return p.parseInstrStmt()
	case p.is("template"):
		p.advance()
		if p.cur.Kind != TokString {
			return nil, p.errorf("expected string after \"template\"")
		}
		text := p.cur.Lit
		p.advance()
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &TemplateStmt{Text: text}, nil
	case p.is("comment"):
		p.advance()
		if p.cur.Kind != TokString {
			return nil, p.errorf("expected string after \"comment\"")
		}
		text := p.cur.Lit
		p.advance()
		if err := p.expect(TokSemi); err != nil {
			return nil, err
		}
		return &CommentStmt{Text: text}, nil
	default:
		return nil, p.errorf("expected emit statement (instr/template/comment), got %q", p.cur.Lit)
	}
}

// Instr = "instr" "{" { InstrField } "}" ";" ;
func (p *parser) parseInstrStmt() (*InstrStmt, error) {
	if err := p.expectKeyword("instr"); err != nil {
		return nil, err
	}
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	instr := &InstrStmt{}
	for p.cur.Kind != TokRBrace {
		if p.cur.Kind == TokEOF {
			return nil, p.errorf("unterminated instr block")
		}
		f, err := p.parseInstrField()
		if err != nil {
			return nil, err
		}
		instr.Fields = append(instr.Fields, f)
		// allow optional comma separator between fields (legacy .td style)
		if p.cur.Kind == TokComma {
			p.advance()
		}
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	if err := p.expect(TokSemi); err != nil {
		return nil, err
	}
	return instr, nil
}

// InstrField = key ":" Value ";" | key ":" ValueList ";" ;
func (p *parser) parseInstrField() (*InstrField, error) {
	name, err := p.expectIdent("instr field name")
	if err != nil {
		return nil, err
	}
	if err := p.expect(TokColon); err != nil {
		return nil, err
	}

	var val *Value
	// "uses" takes a ValueList; all others take a single Value.
	if strings.EqualFold(name, "uses") {
		elems, err := p.parseValueList()
		if err != nil {
			return nil, err
		}
		val = &Value{Kind: ValueList, Elems: elems}
	} else {
		v, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		val = v
	}

	// optional trailing ";"
	if p.cur.Kind == TokSemi {
		p.advance()
	}
	return &InstrField{Name: name, Val: val}, nil
}

// ---------------------------------------------------------------------------
// PatternExpr = Ident [ "(" [ PatternArgs ] ")" ] ;
// ---------------------------------------------------------------------------

func (p *parser) parsePatternExpr() (*PatternExpr, error) {
	name, err := p.expectIdent("pattern name")
	if err != nil {
		return nil, err
	}

	pat := &PatternExpr{Name: name}
	if p.cur.Kind != TokLParen {
		return pat, nil
	}

	p.advance() // consume "("
	if p.cur.Kind == TokRParen {
		p.advance()
		return pat, nil
	}

	args, err := p.parsePatternArgs()
	if err != nil {
		return nil, err
	}
	pat.Args = args

	if err := p.expect(TokRParen); err != nil {
		return nil, err
	}

	return pat, nil
}

// PatternArgs = PatternExpr { "," PatternExpr } | Value { "," Value } ;
// Unified: an arg is a PatternExpr when cur is a non-"$" ident; otherwise a Value.
func (p *parser) parsePatternArgs() ([]PatternArg, error) {
	var args []PatternArg
	for {
		arg, err := p.parsePatternArg()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)

		if p.cur.Kind != TokComma {
			break
		}
		p.advance()
	}

	return args, nil
}

func (p *parser) parsePatternArg() (PatternArg, error) {
	// Non-$ ident → could be nested PatternExpr
	if p.cur.Kind == TokIdent && !strings.HasPrefix(p.cur.Lit, "$") {
		return p.parsePatternExpr()
	}
	return p.parseValue()
}

// ---------------------------------------------------------------------------
// Value = String | Number | Ident | Ref | List | Record | PatternExpr ;
// ---------------------------------------------------------------------------

func (p *parser) parseValue() (*Value, error) {
	switch p.cur.Kind {
	case TokString:
		v := &Value{Kind: ValueString, Lit: p.cur.Lit}
		p.advance()
		return v, nil

	case TokNumber:
		v := &Value{Kind: ValueNumber, Lit: p.cur.Lit}
		p.advance()
		return v, nil

	case TokIdent:
		return p.parseIdentValue()

	case TokLBracket:
		return p.parseListValue()

	case TokLBrace:
		return p.parseRecordValue()

	case TokBang:
		// "!" is only meaningful in predicate context; treat as error here.
		return nil, p.errorf("unexpected \"!\" in value context")

	default:
		return nil, p.errorf("expected value, got %q", p.cur.Lit)
	}
}

// parseIdentValue handles Ident, Ref ($…), and PatternExpr (ident with args).
func (p *parser) parseIdentValue() (*Value, error) {
	lit := p.cur.Lit
	p.advance()

	// $… → Ref
	if strings.HasPrefix(lit, "$") {
		return &Value{Kind: ValueRef, Ref: strings.TrimPrefix(lit, "$")}, nil
	}

	// ident "(" → PatternExpr
	if p.cur.Kind == TokLParen {
		p.advance() // consume "("
		pat := &PatternExpr{Name: lit}
		if p.cur.Kind != TokRParen {
			args, err := p.parsePatternArgs()
			if err != nil {
				return nil, err
			}
			pat.Args = args
		}
		if err := p.expect(TokRParen); err != nil {
			return nil, err
		}
		return &Value{Kind: ValuePattern, Pattern: pat}, nil
	}

	return &Value{Kind: ValueIdent, Lit: lit}, nil
}

// ---------------------------------------------------------------------------
// List = "[" [ Value { "," Value } ] "]" ;
// ---------------------------------------------------------------------------

func (p *parser) parseListValue() (*Value, error) {
	if err := p.expect(TokLBracket); err != nil {
		return nil, err
	}
	v := &Value{Kind: ValueList}
	if p.cur.Kind == TokRBracket {
		p.advance()
		return v, nil
	}
	elems, err := p.parseValueList()
	if err != nil {
		return nil, err
	}
	v.Elems = elems
	if err := p.expect(TokRBracket); err != nil {
		return nil, err
	}
	return v, nil
}

// ---------------------------------------------------------------------------
// Record = "{" [ Field { "," Field } ] "}" ;
// ---------------------------------------------------------------------------

func (p *parser) parseRecordValue() (*Value, error) {
	if err := p.expect(TokLBrace); err != nil {
		return nil, err
	}
	v := &Value{Kind: ValueRecord}
	if p.cur.Kind == TokRBrace {
		p.advance()
		return v, nil
	}
	for {
		key, err := p.expectIdent("record field key")
		if err != nil {
			return nil, err
		}
		if p.cur.Kind != TokEqual && p.cur.Kind != TokColon {
			return nil, p.errorf("expected \"=\" or \":\" after record key %q", key)
		}
		sep := p.cur.Lit
		p.advance()
		val, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		v.Fields = append(v.Fields, &RecordField{Key: key, Sep: sep, Val: val})
		if p.cur.Kind == TokComma || p.cur.Kind == TokSemi {
			p.advance()
			if p.cur.Kind == TokRBrace {
				break
			}
			continue
		}
		break
	}
	if err := p.expect(TokRBrace); err != nil {
		return nil, err
	}
	return v, nil
}

// ---------------------------------------------------------------------------
// ValueList = Value { "," Value } ;
// ---------------------------------------------------------------------------

func (p *parser) parseValueList() ([]*Value, error) {
	var vals []*Value
	for {
		v, err := p.parseValue()
		if err != nil {
			return nil, err
		}
		vals = append(vals, v)
		if p.cur.Kind != TokComma {
			break
		}
		p.advance()
	}
	return vals, nil
}

// ---------------------------------------------------------------------------
// tokenName helper (shared with lexer Token.String)
// ---------------------------------------------------------------------------

func tokenName(k TokenType) string {
	switch k {
	case TokEOF:
		return "EOF"
	case TokIdent:
		return "identifier"
	case TokString:
		return "string"
	case TokNumber:
		return "number"
	case TokLBrace:
		return "{"
	case TokRBrace:
		return "}"
	case TokLParen:
		return "("
	case TokRParen:
		return ")"
	case TokLBracket:
		return "["
	case TokRBracket:
		return "]"
	case TokColon:
		return ":"
	case TokSemi:
		return ";"
	case TokComma:
		return ","
	case TokEqual:
		return "="
	case TokBang:
		return "!"
	case TokArrow:
		return "->"
	default:
		return fmt.Sprintf("token(%d)", k)
	}
}
