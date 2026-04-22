package project

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode/utf8"

	"github.com/anthonyabeo/obx/src/support/source"
	"github.com/anthonyabeo/obx/src/syntax/directive"
	scansyntax "github.com/anthonyabeo/obx/src/syntax/scan"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// ── Public API ───────────────────────────────────────────────────────────────

// ScanHeader reads a single .obx file, verifies it contains exactly one
// MODULE or DEFINITION declaration whose name matches the file stem, and
// returns the populated Header.
//
// root is the source-tree root used to derive the ModuleKey from the file's
// relative path (e.g. root="/src", file="/src/Collections/Drawing.obx"
// → key "Collections.Drawing").
func ScanHeader(root, file string) (Header, error) {
	data, err := os.ReadFile(file)
	if err != nil {
		return Header{}, fmt.Errorf("read %s: %w", file, err)
	}
	key, err := KeyFromFile(root, file)
	if err != nil {
		return Header{}, fmt.Errorf("%s: %w", file, err)
	}
	return parseHeader(key, file, data)
}

// ScanHeaderWithResolver is like ScanHeader but takes a directive.Resolver that
// maps directive identifiers to compile-time values (bool/int/float). Callers
// (e.g. the CLI or web server) should supply platform flags (POSIX/WINDOWS
// etc.) so header scanning selects the correct conditional branches.
func ScanHeaderWithResolver(root, file string, resolver directive.Resolver) (Header, error) {
	data, err := os.ReadFile(file)
	if err != nil {
		return Header{}, fmt.Errorf("read %s: %w", file, err)
	}
	key, err := KeyFromFile(root, file)
	if err != nil {
		return Header{}, fmt.Errorf("%s: %w", file, err)
	}
	return parseHeaderWithResolver(key, file, data, resolver)
}

// DiscoverAndScan walks root, finds every .obx file, and returns one Header
// per file in filesystem order.
func DiscoverAndScan(root string) ([]Header, error) {
	var headers []Header
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && filepath.Ext(path) == ".obx" {
			hdr, err := ScanHeader(root, path)
			if err != nil {
				return err
			}
			headers = append(headers, hdr)
		}
		return nil
	})
	return headers, err
}

// DiscoverAndScanWithResolver walks root and scans each .obx file using the
// provided directive resolver (may be nil). This lets callers pass platform
// flags so conditional imports are evaluated consistently during discovery.
func DiscoverAndScanWithResolver(root string, resolver directive.Resolver) ([]Header, error) {
	var headers []Header
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && filepath.Ext(path) == ".obx" {
			hdr, err := ScanHeaderWithResolver(root, path, resolver)
			if err != nil {
				return err
			}
			headers = append(headers, hdr)
		}
		return nil
	})
	return headers, err
}

// DiscoverModuleFiles returns every .obx path under root (kept for
// compatibility with callers that still drive scanning themselves).
func DiscoverModuleFiles(root string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && filepath.Ext(path) == ".obx" {
			files = append(files, path)
		}
		return nil
	})
	return files, err
}

// ── Lightweight header lexer ─────────────────────────────────────────────────
//
// The lexer only needs to recognise the small token set relevant to a module
// header: keywords (module, definition, import, end, and body-start keywords
// that mark the end of the import section), identifiers, '.', ',', ';', ':='.
//
// Keywords are case-insensitive (Oberon+ allows both MODULE and module).

type hKind int

const (
	hEOF hKind = iota
	hIDENT
	hPERIOD    // .
	hCOMMA     // ,
	hSEMICOLON // ;
	hBECOMES   // :=
	hDIRECTIVE // <*
	// keywords
	hMODULE
	hDEFINITION
	hIMPORT
	hEND
	// body-start keywords — signal end of the header section
	hCONST
	hTYPE
	hVAR
	hPROC
	hPROCEDURE
	hBEGIN
)

var hKeywords = map[string]hKind{
	"module":     hMODULE,
	"definition": hDEFINITION,
	"import":     hIMPORT,
	"end":        hEND,
	"const":      hCONST,
	"type":       hTYPE,
	"var":        hVAR,
	"proc":       hPROC,
	"procedure":  hPROCEDURE,
	"begin":      hBEGIN,
}

type hToken struct {
	kind hKind
	lex  string
	pos  int // byte offset of first character
}

type hLexer struct {
	src       []byte
	pos       int
	lookahead *hToken
}

// next returns and consumes the next token.
func (l *hLexer) next() hToken {
	if l.lookahead != nil {
		t := *l.lookahead
		l.lookahead = nil
		return t
	}
	return l.scan()
}

// peek returns the next token without consuming it.
func (l *hLexer) peek() hToken {
	if l.lookahead == nil {
		t := l.scan()
		l.lookahead = &t
	}
	return *l.lookahead
}

// scan reads one token from the stream, skipping whitespace and comments.
func (l *hLexer) scan() hToken {
	for {
		l.skipWS()
		if l.pos >= len(l.src) {
			return hToken{kind: hEOF, pos: l.pos}
		}

		start := l.pos
		r := l.readRune()

		switch {
		case isHLetter(r) || r == '_':
			return l.scanIdent(start, r)

		case r == '<':
			// possible directive start '<*'
			if l.pos < len(l.src) {
				next, sz := utf8.DecodeRune(l.src[l.pos:])
				if next == '*' {
					l.pos += sz
					return hToken{kind: hDIRECTIVE, lex: "<*", pos: start}
				}
			}

		case r == '.':
			return hToken{kind: hPERIOD, lex: ".", pos: start}

		case r == ',':
			return hToken{kind: hCOMMA, lex: ",", pos: start}

		case r == ';':
			return hToken{kind: hSEMICOLON, lex: ";", pos: start}

		case r == ':':
			if l.pos < len(l.src) {
				next, sz := utf8.DecodeRune(l.src[l.pos:])
				if next == '=' {
					l.pos += sz
					return hToken{kind: hBECOMES, lex: ":=", pos: start}
				}
			}
			// bare ':' is not used in the header grammar — skip it
			continue

		case r == '/' && l.pos < len(l.src) && l.src[l.pos] == '/':
			l.skipLineComment()
			continue

		case r == '(' && l.pos < len(l.src) && l.src[l.pos] == '*':
			l.pos++ // consume '*'
			l.skipBlockComment()
			continue

		default:
			// operators, brackets, literals — not needed in the header; skip
			continue
		}
	}
}

func (l *hLexer) scanIdent(start int, first rune) hToken {
	buf := make([]rune, 0, 16)
	buf = append(buf, first)
	for l.pos < len(l.src) {
		r, sz := utf8.DecodeRune(l.src[l.pos:])
		if isHLetter(r) || isHDigit(r) || r == '_' {
			buf = append(buf, r)
			l.pos += sz
		} else {
			break
		}
	}
	lex := string(buf)
	lower := strings.ToLower(lex)
	if k, ok := hKeywords[lower]; ok {
		return hToken{kind: k, lex: lower, pos: start}
	}
	return hToken{kind: hIDENT, lex: lex, pos: start}
}

// parseHeaderWithResolver behaves like parseHeader but allows a directive
// resolver to be supplied so callers (e.g. CLI / web) can pass POSIX/WINDOWS
// flags. If resolver is nil, unknown directive identifiers will be unresolved.
func parseHeaderWithResolver(key ModuleKey, file string, src []byte, resolver directive.Resolver) (Header, error) {
	lex := &hLexer{src: src}

	// First real token must be MODULE or DEFINITION.
	tok := lex.next()
	if tok.kind != hMODULE && tok.kind != hDEFINITION {
		return Header{}, fmt.Errorf("%s: expected 'module' or 'definition', found %q", file, tok.lex)
	}
	startPos := tok.pos

	// Module name must follow immediately.
	nameTok := lex.next()
	if nameTok.kind != hIDENT {
		return Header{}, fmt.Errorf("%s: expected module name after '%s'", file, tok.lex)
	}

	if !strings.EqualFold(nameTok.lex, key.Name()) {
		return Header{}, fmt.Errorf(
			"%s: declared module name %q does not match file stem %q",
			file, nameTok.lex, key.Name(),
		)
	}

	hdr := Header{
		Key:      key,
		File:     file,
		StartPos: startPos,
		EndPos:   len(src),
	}

	// Scan forward looking for IMPORT; stop at body / end keywords or EOF.
	for {
		switch lex.peek().kind {
		case hIMPORT:
			lex.next() // consume 'import'
			imps, err := parseImportListWithResolver(lex, file, src, resolver)
			if err != nil {
				return Header{}, err
			}
			hdr.Imports = imps
			return hdr, nil

		case hEND, hCONST, hTYPE, hVAR, hPROC, hPROCEDURE, hBEGIN, hEOF:
			return hdr, nil // no import section

		case hDIRECTIVE:
			// Consume the '<*' token and process the directive. processDirective
			// will return an absolute byte offset to advance the lexer to.
			t := lex.next()
			advanceTo, err := processDirective(lex, file, src, t.pos, resolver)
			if err != nil {
				return Header{}, err
			}
			// Advance the lexer into the chosen branch or past the skipped region.
			if advanceTo >= 0 {
				if advanceTo > len(src) {
					advanceTo = len(src)
				}
				lex.pos = advanceTo
				lex.lookahead = nil
			}

		default:
			lex.next() // skip meta-params, semicolons, etc.
		}
	}
}

func (l *hLexer) skipWS() {
	for l.pos < len(l.src) {
		r, sz := utf8.DecodeRune(l.src[l.pos:])
		if r == ' ' || r == '\t' || r == '\n' || r == '\r' {
			l.pos += sz
		} else {
			break
		}
	}
}

func (l *hLexer) skipLineComment() {
	for l.pos < len(l.src) {
		r, sz := utf8.DecodeRune(l.src[l.pos:])
		l.pos += sz
		if r == '\n' {
			break
		}
	}
}

func (l *hLexer) skipBlockComment() {
	depth := 1
	for l.pos < len(l.src) && depth > 0 {
		r, sz := utf8.DecodeRune(l.src[l.pos:])
		l.pos += sz
		if r == '(' && l.pos < len(l.src) && l.src[l.pos] == '*' {
			l.pos++
			depth++
		} else if r == '*' && l.pos < len(l.src) && l.src[l.pos] == ')' {
			l.pos++
			depth--
		}
	}
}

func (l *hLexer) readRune() rune {
	r, sz := utf8.DecodeRune(l.src[l.pos:])
	l.pos += sz
	return r
}

func isHLetter(r rune) bool {
	return ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z')
}

func isHDigit(r rune) bool {
	return '0' <= r && r <= '9'
}

// ── Header parser ─────────────────────────────────────────────────────────────

func parseHeader(key ModuleKey, file string, src []byte) (Header, error) {
	return parseHeaderWithResolver(key, file, src, nil)
}

// parseImportList parses the comma-separated import list that follows the
// IMPORT keyword.
//
//	ImportList = import { [','] import } [';']
//	import     = [ ident ':=' ] ident { '.' ident }
func parseImportListWithResolver(lex *hLexer, file string, src []byte, resolver directive.Resolver) ([]Import, error) {
	var list []Import

	for {
		// Handle inline directives that may appear inside an import list.
		if lex.peek().kind == hDIRECTIVE {
			t := lex.next()
			advanceTo, err := processDirective(lex, file, src, t.pos, resolver)
			if err != nil {
				return nil, err
			}
			if advanceTo >= 0 {
				if advanceTo > len(src) {
					advanceTo = len(src)
				}
				lex.pos = advanceTo
				lex.lookahead = nil
			}
			// continue to top-of-loop to re-check what's next
			continue
		}

		t := lex.next()
		if t.kind != hIDENT {
			return nil, fmt.Errorf("%s: expected import name, got %q", file, t.lex)
		}

		imp := Import{}
		seg := t.lex

		// Optional alias:  ident ':=' …
		if lex.peek().kind == hBECOMES {
			lex.next()      // consume ':='
			imp.Alias = seg // the ident before ':=' is the alias
			t = lex.next()  // first segment of the actual import path
			if t.kind != hIDENT {
				return nil, fmt.Errorf("%s: expected module name after ':='", file)
			}
			seg = t.lex
		}

		// Collect dot-separated path segments.
		segs := []string{seg}
		for lex.peek().kind == hPERIOD {
			lex.next() // consume '.'
			t = lex.next()
			if t.kind != hIDENT {
				return nil, fmt.Errorf("%s: expected identifier after '.'", file)
			}
			segs = append(segs, t.lex)
		}

		imp.Key = KeyFromSegs(segs)
		list = append(list, imp)

		// Comma → more imports.
		if lex.peek().kind == hCOMMA {
			lex.next()
			continue
		}
		// Optional trailing semicolon.
		if lex.peek().kind == hSEMICOLON {
			lex.next()
		}
		break
	}

	return list, nil
}

// processDirective evaluates or skips a directive chain starting at byte
// offset 'start' in src. It uses a real syntax scanner + directive helpers so
// behaviour matches the full parser. Returns the absolute byte offset within
// src to advance the header lexer to (start of taken branch or past skipped
// arms). If the directive had no body to advance to, returns len(src).
func processDirective(lex *hLexer, file string, src []byte, start int, resolver directive.Resolver) (int, error) {
	if start < 0 || start >= len(src) {
		return -1, nil
	}

	// Create a scanner over the sub-slice starting at the '<*'.
	sub := src[start:]
	srcMgr := source.NewSourceManager()
	sc := scansyntax.Scan(file, sub, srcMgr)

	// Consume the initial DIRECTIVE_START emitted by the scanner so the next
	// token from sc.NextToken() is the first token after '<*'.
	_ = sc.NextToken()

	// If no resolver supplied, be conservative and do not attempt to evaluate
	// IF/ELSIF conditions here — just skip the directive chain so the header
	// scanner's behaviour remains backwards-compatible. Callers that want
	// directive-aware scanning should use ScanHeaderWithResolver.
	first := directive.DNext(sc)
	if resolver == nil {
		// If it's an IF..END chain, skip all remaining arms. Otherwise just
		// consume until the next DIRECTIVE_END.
		if first.Kind == token.IF {
			directive.SkipAllRemainingArms(sc)
			t := sc.NextToken()
			if t.Kind == token.EOF {
				return len(src), nil
			}
			return start + t.Pos, nil
		}
		directive.ConsumeDirectiveEnd(sc)
		t := sc.NextToken()
		if t.Kind == token.EOF {
			return len(src), nil
		}
		return start + t.Pos, nil
	}
	switch first.Kind {
	case token.IF:
		// NewEvalState expects the supplier to be positioned at the first
		// token of the expression (we've consumed IF above), so construct it now.
		for {
			es := directive.NewEvalState(sc, resolver)
			val, cur, err := es.EvalExpr()
			if err != nil {
				return -1, fmt.Errorf("directive IF: %w", err)
			}
			if cur.Kind != token.THEN {
				// consume until directive end to leave scanner in consistent state
				directive.ConsumeDirectiveEnd(sc)
				return -1, fmt.Errorf("directive IF: expected 'THEN', got %q", cur.Lexeme)
			}
			directive.ConsumeDirectiveEnd(sc) // consume THEN + *>

			cond, err := directive.DirIsTruthy(val)
			if err != nil {
				return -1, fmt.Errorf("directive IF: %w", err)
			}
			if cond {
				// Taken branch: next token marks start of body.
				t := sc.NextToken()
				if t.Kind == token.EOF {
					return len(src), nil
				}
				return start + t.Pos, nil
			}

			// Not taken — skip to next arm and evaluate remaining arms.
			for {
				arm, _ := directive.SkipToNextBranch(sc)
				switch arm {
				case token.ELSIF:
					// ELSIF returned — scanner positioned at condition tokens.
					es := directive.NewEvalState(sc, resolver)
					val, cur, err := es.EvalExpr()
					if err != nil {
						return -1, fmt.Errorf("directive ELSIF: %w", err)
					}
					if cur.Kind != token.THEN {
						directive.ConsumeDirectiveEnd(sc)
						return -1, fmt.Errorf("directive ELSIF: expected 'THEN', got %q", cur.Lexeme)
					}
					directive.ConsumeDirectiveEnd(sc)
					cond, err := directive.DirIsTruthy(val)
					if err != nil {
						return -1, fmt.Errorf("directive ELSIF: %w", err)
					}
					if cond {
						t := sc.NextToken()
						if t.Kind == token.EOF {
							return len(src), nil
						}
						return start + t.Pos, nil
					}
					// else continue loop to find next arm
				case token.ELSE:
					// ELSE is already consumed along with its *>, scanner at body
					t := sc.NextToken()
					if t.Kind == token.EOF {
						return len(src), nil
					}
					return start + t.Pos, nil
				default: // END or EOF
					// No arm taken — advance past the END and continue scanning
					t := sc.NextToken()
					if t.Kind == token.EOF {
						return len(src), nil
					}
					return start + t.Pos, nil
				}
			}
		}
	default:
		// ASSERT or unknown directive: if identifier "assert", handle it; otherwise
		// consume until directive end and continue scanning after it.
		if first.Kind == token.IDENTIFIER && strings.EqualFold(first.Lexeme, "assert") {
			es := directive.NewEvalState(sc, resolver)
			directive.HandleAssert(es, nil, start)
			// After handling, position is at DIRECTIVE_END; consume next token as body start
			t := sc.NextToken()
			if t.Kind == token.EOF {
				return len(src), nil
			}
			return start + t.Pos, nil
		}
		// Unknown directive — skip to end
		directive.ConsumeDirectiveEnd(sc)
		t := sc.NextToken()
		if t.Kind == token.EOF {
			return len(src), nil
		}
		return start + t.Pos, nil
	}
}
