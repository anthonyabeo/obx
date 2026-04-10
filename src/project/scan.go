package project

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"unicode/utf8"
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

	// The declared name must match the file stem (case-insensitively).
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
			imps, err := parseImportList(lex, file)
			if err != nil {
				return Header{}, err
			}
			hdr.Imports = imps
			return hdr, nil

		case hEND, hCONST, hTYPE, hVAR, hPROC, hPROCEDURE, hBEGIN, hEOF:
			return hdr, nil // no import section

		default:
			lex.next() // skip meta-params, semicolons, etc.
		}
	}
}

// parseImportList parses the comma-separated import list that follows the
// IMPORT keyword.
//
//	ImportList = import { [','] import } [';']
//	import     = [ ident ':=' ] ident { '.' ident }
func parseImportList(lex *hLexer, file string) ([]Import, error) {
	var list []Import

	for {
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
