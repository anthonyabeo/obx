package cache

// defparse.go — minimal hand-written parser for .def stub files.
//
// Grammar (EBNF sketch):
//
//	DefFile   = "module" Name [ ModAttrs ] NL
//	            { TypeDecl | VarDecl | ProcDecl | Comment | NL }
//	            "end" Name "."?
//
//	ModAttrs  = "[" ModAttr { "," ModAttr } "]"
//	ModAttr   = "dll" STRING | "prefix" STRING
//
//	TypeDecl  = "type" Name "=" TypeExpr NL
//	VarDecl   = "var" Name ["*"] ":" TypeExpr NL
//	ProcDecl  = "proc" Name ["*"] "(" [Params] ")" [ ":" TypeExpr ] [ ProcAttrs ] NL
//
//	ProcAttrs = "[" ProcAttr { "," ProcAttr } "]"
//	ProcAttr  = "varargs" | "cname" STRING | "dll" STRING | "prefix" STRING
//
//	Params    = Param { { ";" | "," } Param }
//	Param     = [ ParamKind ] Name { "," Name } ":" TypeExpr
//	ParamKind = "var" | "in" | "out"
//
//	TypeExpr  = "cptr" TypeExpr
//	           | "array" "of" TypeExpr
//	           | "array" INTEGER "of" TypeExpr
//	           | Name
//
//	Name      = identifier
//	STRING    = `"` … `"`

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

// ── AST nodes ─────────────────────────────────────────────────────────────────

// DefModule is the parsed representation of a .def stub file.
type DefModule struct {
	Name    string
	DLLName string // from [dll "..."]
	Prefix  string // from [prefix "..."]
	Types   []*DefType
	Vars    []*DefVar
	Procs   []*DefProc
}

// DefType represents a TYPE declaration.
type DefType struct {
	Name     string
	Exported bool
	Def      DefTypeExpr
}

// DefVar represents a VAR declaration.
type DefVar struct {
	Name     string
	Exported bool
	Typ      DefTypeExpr
}

// DefProc represents a PROC declaration.
type DefProc struct {
	Name     string
	Exported bool
	Params   []*DefParam
	Result   DefTypeExpr // nil → void
	Varargs  bool
	CName    string // effective C symbol (overrides name)
	DLLName  string // per-proc dll override
	Prefix   string // per-proc prefix override
}

// DefParam represents one parameter declaration.
type DefParam struct {
	Name string
	Kind string // "", "var", "in", "out"
	Typ  DefTypeExpr
}

// DefTypeExpr is implemented by all parsed type expressions.
type DefTypeExpr interface{ defTypeExpr() }

type BasicTExpr struct{ Name string }     // integer, longint, real, …
type CptrTExpr struct{ Base DefTypeExpr } // cptr <base>
type ArrayTExpr struct {                  // array [n] of <elem>
	Open bool
	Len  int
	Elem DefTypeExpr
}
type NamedTExpr struct{ Name string } // reference to a declared type

func (BasicTExpr) defTypeExpr()  {}
func (*CptrTExpr) defTypeExpr()  {}
func (*ArrayTExpr) defTypeExpr() {}
func (NamedTExpr) defTypeExpr()  {}

// ── tokeniser ─────────────────────────────────────────────────────────────────

type tokKind int

const (
	tokEOF tokKind = iota
	tokIdent
	tokString
	tokInt
	tokStar
	tokColon
	tokSemi
	tokComma
	tokLBrack
	tokRBrack
	tokLParen
	tokRParen
	tokEq
	tokNL
)

type tok struct {
	kind tokKind
	text string
	ival int
}

func tokenise(src string) []tok {
	var tokens []tok
	i := 0
	for i < len(src) {
		ch := rune(src[i])

		// Skip Windows \r
		if ch == '\r' {
			i++
			continue
		}

		// Newline
		if ch == '\n' {
			tokens = append(tokens, tok{kind: tokNL})
			i++
			continue
		}

		// Horizontal whitespace
		if ch == ' ' || ch == '\t' {
			i++
			continue
		}

		// Line comment
		if i+1 < len(src) && src[i] == '/' && src[i+1] == '/' {
			for i < len(src) && src[i] != '\n' {
				i++
			}
			continue
		}

		// String literal
		if ch == '"' {
			i++
			start := i
			for i < len(src) && src[i] != '"' {
				i++
			}
			s := src[start:i]
			if i < len(src) {
				i++ // consume closing "
			}
			tokens = append(tokens, tok{kind: tokString, text: s})
			continue
		}

		// Integer literal
		if unicode.IsDigit(ch) {
			start := i
			for i < len(src) && unicode.IsDigit(rune(src[i])) {
				i++
			}
			n, _ := strconv.Atoi(src[start:i])
			tokens = append(tokens, tok{kind: tokInt, ival: n})
			continue
		}

		// Identifier / keyword
		if unicode.IsLetter(ch) || ch == '_' {
			start := i
			for i < len(src) && (unicode.IsLetter(rune(src[i])) || unicode.IsDigit(rune(src[i])) || src[i] == '_') {
				i++
			}
			tokens = append(tokens, tok{kind: tokIdent, text: src[start:i]})
			continue
		}

		// Single-character tokens
		switch ch {
		case '*':
			tokens = append(tokens, tok{kind: tokStar})
		case ':':
			tokens = append(tokens, tok{kind: tokColon})
		case ';':
			tokens = append(tokens, tok{kind: tokSemi})
		case ',':
			tokens = append(tokens, tok{kind: tokComma})
		case '[':
			tokens = append(tokens, tok{kind: tokLBrack})
		case ']':
			tokens = append(tokens, tok{kind: tokRBrack})
		case '(':
			tokens = append(tokens, tok{kind: tokLParen})
		case ')':
			tokens = append(tokens, tok{kind: tokRParen})
		case '=':
			tokens = append(tokens, tok{kind: tokEq})
		case '.':
			// ignore trailing dots (end Foo.)
		}
		i++
	}
	tokens = append(tokens, tok{kind: tokEOF})
	return tokens
}

// ── parser ────────────────────────────────────────────────────────────────────

type defParser struct {
	tokens []tok
	pos    int
}

func newDefParser(tokens []tok) *defParser {
	return &defParser{tokens: tokens}
}

func (p *defParser) peek() tok {
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokNL {
		p.pos++
	}
	if p.pos >= len(p.tokens) {
		return tok{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

// peekRaw returns the next token without skipping newlines.
func (p *defParser) peekRaw() tok {
	if p.pos >= len(p.tokens) {
		return tok{kind: tokEOF}
	}
	return p.tokens[p.pos]
}

func (p *defParser) consume() tok {
	// skip newlines and return next real token
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokNL {
		p.pos++
	}
	if p.pos >= len(p.tokens) {
		return tok{kind: tokEOF}
	}
	t := p.tokens[p.pos]
	p.pos++
	return t
}

func (p *defParser) expect(k tokKind) (tok, error) {
	t := p.consume()
	if t.kind != k {
		return t, fmt.Errorf("expected token kind %d, got %v %q", k, t.kind, t.text)
	}
	return t, nil
}

func (p *defParser) skipNewlines() {
	for p.pos < len(p.tokens) && p.tokens[p.pos].kind == tokNL {
		p.pos++
	}
}

func (p *defParser) parseModule() (*DefModule, error) {
	p.skipNewlines()

	// "module"
	kw := p.consume()
	if kw.kind != tokIdent || strings.ToLower(kw.text) != "module" {
		return nil, fmt.Errorf("expected 'module', got %q", kw.text)
	}

	// module name
	nameTok, err := p.expect(tokIdent)
	if err != nil {
		return nil, fmt.Errorf("missing module name: %w", err)
	}
	m := &DefModule{Name: nameTok.text}

	// optional [attrs]
	if p.peek().kind == tokLBrack {
		if err := p.parseModAttrs(m); err != nil {
			return nil, err
		}
	}

	// body declarations
	for {
		p.skipNewlines()
		t := p.peek()
		if t.kind == tokEOF {
			break
		}
		if t.kind != tokIdent {
			p.consume()
			continue
		}
		kw := strings.ToLower(t.text)
		switch kw {
		case "end":
			p.consume() // consume "end"
			p.consume() // consume module name
			return m, nil
		case "type":
			p.consume()
			td, err := p.parseTypeDecl()
			if err != nil {
				return nil, err
			}
			m.Types = append(m.Types, td)
		case "var":
			p.consume()
			vd, err := p.parseVarDecl()
			if err != nil {
				return nil, err
			}
			m.Vars = append(m.Vars, vd)
		case "proc", "procedure":
			p.consume()
			pd, err := p.parseProcDecl(m.DLLName, m.Prefix)
			if err != nil {
				return nil, err
			}
			m.Procs = append(m.Procs, pd)
		default:
			p.consume() // skip unknown keyword
		}
	}
	return m, nil
}

func (p *defParser) parseModAttrs(m *DefModule) error {
	p.consume() // consume "["
	for {
		t := p.consume()
		if t.kind == tokRBrack || t.kind == tokEOF {
			break
		}
		if t.kind != tokIdent {
			continue
		}
		switch strings.ToLower(t.text) {
		case "dll":
			st, err := p.expect(tokString)
			if err != nil {
				return err
			}
			m.DLLName = st.text
		case "prefix":
			st, err := p.expect(tokString)
			if err != nil {
				return err
			}
			m.Prefix = st.text
		}
		// skip comma
		if p.peek().kind == tokComma {
			p.consume()
		}
	}
	return nil
}

func (p *defParser) parseTypeDecl() (*DefType, error) {
	nameTok, err := p.expect(tokIdent)
	if err != nil {
		return nil, fmt.Errorf("type name: %w", err)
	}
	exported := false
	if p.peek().kind == tokStar {
		p.consume()
		exported = true
	}
	if _, err := p.expect(tokEq); err != nil {
		return nil, fmt.Errorf("'=' after type name: %w", err)
	}
	te, err := p.parseTypeExpr()
	if err != nil {
		return nil, err
	}
	return &DefType{Name: nameTok.text, Exported: exported, Def: te}, nil
}

func (p *defParser) parseVarDecl() (*DefVar, error) {
	nameTok, err := p.expect(tokIdent)
	if err != nil {
		return nil, fmt.Errorf("var name: %w", err)
	}
	exported := false
	if p.peek().kind == tokStar {
		p.consume()
		exported = true
	}
	if _, err := p.expect(tokColon); err != nil {
		return nil, fmt.Errorf("':' in var decl: %w", err)
	}
	te, err := p.parseTypeExpr()
	if err != nil {
		return nil, err
	}
	return &DefVar{Name: nameTok.text, Exported: exported, Typ: te}, nil
}

func (p *defParser) parseProcDecl(modDLL, modPrefix string) (*DefProc, error) {
	nameTok, err := p.expect(tokIdent)
	if err != nil {
		return nil, fmt.Errorf("proc name: %w", err)
	}
	pd := &DefProc{
		Name:    nameTok.text,
		DLLName: modDLL,
		Prefix:  modPrefix,
	}
	if p.peek().kind == tokStar {
		p.consume()
		pd.Exported = true
	}

	// parameter list
	if p.peek().kind == tokLParen {
		p.consume() // consume "("
		if p.peek().kind != tokRParen {
			if err := p.parseParams(pd); err != nil {
				return nil, err
			}
		}
		if _, err := p.expect(tokRParen); err != nil {
			return nil, fmt.Errorf("')' in proc %s: %w", pd.Name, err)
		}
	}

	// optional result type
	if p.peek().kind == tokColon {
		p.consume()
		te, err := p.parseTypeExpr()
		if err != nil {
			return nil, err
		}
		pd.Result = te
	}

	// optional [attrs]
	if p.peek().kind == tokLBrack {
		if err := p.parseProcAttrs(pd); err != nil {
			return nil, err
		}
	}

	// Compute effective CName: prefix + (cname override OR proc name)
	base := pd.Name
	if pd.CName != "" {
		base = pd.CName
	}
	pd.CName = pd.Prefix + base

	return pd, nil
}

func (p *defParser) parseParams(pd *DefProc) error {
	for {
		// parse one parameter group: [kind] name {"," name} ":" typeExpr
		kind := ""
		t := p.peek()
		if t.kind == tokIdent {
			kw := strings.ToLower(t.text)
			if kw == "var" || kw == "in" || kw == "out" {
				kind = kw
				p.consume()
			}
		}

		// one or more names
		var names []string
		nameTok, err := p.expect(tokIdent)
		if err != nil {
			return fmt.Errorf("param name: %w", err)
		}
		names = append(names, nameTok.text)
		for p.peek().kind == tokComma {
			p.consume()
			// peek — if next is ident followed by colon it's a new group
			if p.peek().kind == tokIdent {
				// lookahead: is this a kind keyword or another name in same group?
				// We'll consume it as another name; if it's a kind keyword in
				// a new group, the next iteration will handle it.
				n2, _ := p.expect(tokIdent)
				names = append(names, n2.text)
			}
		}

		if _, err := p.expect(tokColon); err != nil {
			return fmt.Errorf("':' in param list: %w", err)
		}
		te, err := p.parseTypeExpr()
		if err != nil {
			return err
		}
		for _, n := range names {
			pd.Params = append(pd.Params, &DefParam{Name: n, Kind: kind, Typ: te})
		}

		// separator: ";" or "," before next group (some .def styles use ";")
		sep := p.peek()
		if sep.kind == tokSemi {
			p.consume()
		} else if sep.kind == tokComma {
			// peek further — if next after comma is an ident followed by colon
			// it's a new group; stay in loop
			p.consume()
			// check if what follows looks like a kind keyword
			next := p.peek()
			if next.kind == tokIdent {
				kw := strings.ToLower(next.text)
				if kw == "var" || kw == "in" || kw == "out" {
					continue
				}
			}
		} else {
			break
		}

		// Check for end of param list
		if p.peek().kind == tokRParen {
			break
		}
	}
	return nil
}

func (p *defParser) parseProcAttrs(pd *DefProc) error {
	p.consume() // consume "["
	for {
		t := p.consume()
		if t.kind == tokRBrack || t.kind == tokEOF {
			break
		}
		if t.kind != tokIdent {
			continue
		}
		switch strings.ToLower(t.text) {
		case "varargs":
			pd.Varargs = true
		case "cname":
			st, err := p.expect(tokString)
			if err != nil {
				return err
			}
			pd.CName = st.text
		case "dll":
			st, err := p.expect(tokString)
			if err != nil {
				return err
			}
			pd.DLLName = st.text
		case "prefix":
			st, err := p.expect(tokString)
			if err != nil {
				return err
			}
			pd.Prefix = st.text
		}
		if p.peek().kind == tokComma {
			p.consume()
		}
	}
	return nil
}

// parseTypeExpr parses a type expression.
// The type name "of" is consumed as part of "array of" / "cptr".
func (p *defParser) parseTypeExpr() (DefTypeExpr, error) {
	t := p.peek()
	if t.kind != tokIdent {
		return nil, fmt.Errorf("expected type expression, got %v %q", t.kind, t.text)
	}
	kw := strings.ToLower(t.text)
	switch kw {
	case "cptr":
		p.consume()
		base, err := p.parseTypeExpr()
		if err != nil {
			return nil, err
		}
		return &CptrTExpr{Base: base}, nil

	case "array":
		p.consume()
		// optional length
		open := false
		length := -1
		if p.peek().kind == tokInt {
			length = p.consume().ival
		} else {
			open = true
		}
		// expect "of"
		ofTok := p.consume()
		if ofTok.kind != tokIdent || strings.ToLower(ofTok.text) != "of" {
			return nil, fmt.Errorf("expected 'of' after 'array', got %q", ofTok.text)
		}
		elem, err := p.parseTypeExpr()
		if err != nil {
			return nil, err
		}
		return &ArrayTExpr{Open: open, Len: length, Elem: elem}, nil

	default:
		// identifier — either a basic type keyword or a named type
		p.consume()
		switch kw {
		case "integer", "longint", "real", "longreal", "boolean",
			"char", "wchar", "byte", "shortint", "int8", "int16", "int32",
			"int64", "set", "void":
			return BasicTExpr{Name: kw}, nil
		default:
			return NamedTExpr{Name: t.text}, nil
		}
	}
}

// ── Public entry point ────────────────────────────────────────────────────────

// ParseDef parses a .def stub source into a DefModule.
func ParseDef(src string) (*DefModule, error) {
	tokens := tokenise(src)
	p := newDefParser(tokens)
	return p.parseModule()
}
