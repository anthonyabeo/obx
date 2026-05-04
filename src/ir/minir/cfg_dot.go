package minir

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
	"unicode"
)

// dotEscape sanitises s for safe embedding inside a Graphviz HTML-like label
// (the << ... >> form).  Graphviz's internal HTML parser only accepts the
// standard named entities &amp; &lt; &gt; &quot; — numeric references such as
// &#34; or &#39; produced by Go's html.EscapeString are NOT supported and
// cause a "Render error: in label of node N" crash in viz.js.
//
// Rules applied:
//   - &  → &amp;
//   - <  → &lt;
//   - >  → &gt;
//   - "  → &quot;  (safe in both text content and attribute values)
//   - \n → literal backslash-n  (raw newlines break the DOT string parser)
//   - \r → backslash-r
//   - \t → backslash-t
//   - other C0/C1 control chars → \xNN
//   - strings longer than 120 runes are truncated with "…"
//
// Single quotes are left verbatim — they are safe in HTML text content.
func dotEscape(s string) string {
	const maxLen = 120
	var b strings.Builder
	b.Grow(len(s))
	for i, r := range s {
		if i >= maxLen {
			b.WriteString("…")
			break
		}
		switch r {
		case '&':
			b.WriteString("&amp;")
		case '<':
			b.WriteString("&lt;")
		case '>':
			b.WriteString("&gt;")
		case '"':
			b.WriteString("&quot;")
		case '\n':
			b.WriteString(`\n`)
		case '\r':
			b.WriteString(`\r`)
		case '\t':
			b.WriteString(`\t`)
		default:
			if unicode.IsControl(r) {
				fmt.Fprintf(&b, `\x%02x`, r)
			} else {
				b.WriteRune(r)
			}
		}
	}
	return b.String()
}

// DotOptions controls DOT output styling.
type DotOptions struct {
	Title      string // optional graph title
	EntryColor string
	ExitColor  string
	MergeColor string
	Compact    bool // omit instruction bodies when true
}

// WriteDOT writes a DOT digraph representation of fn to w.
func WriteDOT(w io.Writer, fn *Function, opts DotOptions) error {
	if fn == nil {
		return errors.New("nil function")
	}
	if opts.EntryColor == "" {
		opts.EntryColor = "#e6ffed" // pale green
	}
	if opts.ExitColor == "" {
		opts.ExitColor = "#eeeeee" // light gray
	}
	if opts.MergeColor == "" {
		opts.MergeColor = "#ffffff"
	}

	// build label -> block map and ordered ids
	ids := make([]int, 0, len(fn.Blocks))
	labelMap := make(map[string]*Block)
	for id, b := range fn.Blocks {
		ids = append(ids, id)
		labelMap[b.Label] = b
	}
	// deterministic ordering
	// sort ids
	for i := 0; i < len(ids)-1; i++ {
		for j := i + 1; j < len(ids); j++ {
			if ids[i] > ids[j] {
				ids[i], ids[j] = ids[j], ids[i]
			}
		}
	}

	var buf bytes.Buffer
	buf.WriteString("digraph G {\n")
	buf.WriteString("  graph [rankdir=LR];\n")
	if opts.Title != "" {
		buf.WriteString(fmt.Sprintf("  label=%q; labelloc=top; fontsize=18;\n", opts.Title))
	}

	// emit nodes
	for _, id := range ids {
		b := fn.Blocks[id]
		// compact mode: omit dead blocks
		if opts.Compact && strings.Contains(b.Label, "dead") {
			continue
		}
		node := fmt.Sprintf("n%d", b.ID)
		// build HTML-like label as a table
		var rows []string
		// header: block label
		headerBg := opts.MergeColor
		if b == fn.Entry {
			headerBg = opts.EntryColor
		}
		if b == fn.Exit {
			headerBg = opts.ExitColor
		}
		rows = append(rows, fmt.Sprintf("<TR><TD BGCOLOR=\"%s\"><B>%s</B></TD></TR>", headerBg, dotEscape(b.Label)))

		if opts.Compact {
			// show succ count
			rows = append(rows, fmt.Sprintf("<TR><TD>succ=%d</TD></TR>", len(b.Succs)))
		} else {
			for _, ins := range b.Instrs {
				s := dotEscape(FormatInstr(ins))
				rows = append(rows, fmt.Sprintf("<TR><TD ALIGN=\"LEFT\"><FONT FACE=\"monospace\">%s</FONT></TD></TR>", s))
			}
			// ensure terminator printed (if not already last)
			if b.Term != nil {
				lastIdx := -1
				if len(b.Instrs) > 0 {
					lastIdx = len(b.Instrs) - 1
				}
				if lastIdx < 0 || b.Instrs[lastIdx] != b.Term {
					s := dotEscape(FormatInstr(b.Term))
					rows = append(rows, fmt.Sprintf("<TR><TD ALIGN=\"LEFT\"><I><FONT FACE=\"monospace\">%s</FONT></I></TD></TR>", s))
				}
			}
		}

		label := fmt.Sprintf("<<TABLE BORDER=0 CELLBORDER=1 CELLSPACING=0>%s</TABLE>>", strings.Join(rows, ""))
		buf.WriteString(fmt.Sprintf("  %s [shape=none, label=%s];\n", node, label))
	}

	// emit edges
	for _, id := range ids {
		b := fn.Blocks[id]
		if opts.Compact && strings.Contains(b.Label, "dead") {
			continue
		}
		src := fmt.Sprintf("n%d", b.ID)
		if b.Term == nil {
			continue
		}
		switch t := b.Term.(type) {
		case *JumpInst:
			if tgt := fn.GetBlock(t.Target); tgt != nil {
				if opts.Compact && strings.Contains(t.Target, "dead") {
					continue
				}
				attrs := []string{"color=black"}
				// back-edge?
				if tgt.ID <= b.ID {
					attrs = append(attrs, "color=orange", "constraint=false")
				}
				buf.WriteString(fmt.Sprintf("  %s -> n%d [%s];\n", src, tgt.ID, strings.Join(attrs, ",")))
			}
		case *CondBrInst:
			if tTrue := fn.GetBlock(t.TrueLabel); tTrue != nil {
				if !(opts.Compact && strings.Contains(t.TrueLabel, "dead")) {
					attrs := []string{"color=green", "label=\"T\""}
					if tTrue.ID <= b.ID {
						attrs = append(attrs, "constraint=false")
					}
					buf.WriteString(fmt.Sprintf("  %s -> n%d [%s];\n", src, tTrue.ID, strings.Join(attrs, ",")))
				}
			}
			if tFalse := fn.GetBlock(t.FalseLabel); tFalse != nil {
				if !(opts.Compact && strings.Contains(t.FalseLabel, "dead")) {
					attrs := []string{"color=red", "style=dashed", "label=\"F\""}
					if tFalse.ID <= b.ID {
						attrs = append(attrs, "constraint=false")
					}
					buf.WriteString(fmt.Sprintf("  %s -> n%d [%s];\n", src, tFalse.ID, strings.Join(attrs, ",")))
				}
			}
		case *SwitchInst:
			// default target
			if d := fn.GetBlock(t.Default); d != nil {
				if !(opts.Compact && strings.Contains(t.Default, "dead")) {
					attrs := []string{"color=blue", "label=\"default\""}
					if d.ID <= b.ID {
						attrs = append(attrs, "constraint=false")
					}
					buf.WriteString(fmt.Sprintf("  %s -> n%d [%s];\n", src, d.ID, strings.Join(attrs, ",")))
				}
			}
			for _, arm := range t.Arms {
				if a := fn.GetBlock(arm.Label); a != nil {
					if opts.Compact && strings.Contains(arm.Label, "dead") {
						continue
					}
					attrs := []string{`color=blue`, fmt.Sprintf(`label="%d"`, arm.Val)}
					if a.ID <= b.ID {
						attrs = append(attrs, "constraint=false")
					}
					buf.WriteString(fmt.Sprintf("  %s -> n%d [%s];\n", src, a.ID, strings.Join(attrs, ",")))
				}
			}
		}
	}

	buf.WriteString("}\n")
	_, err := w.Write(buf.Bytes())
	return err
}

// FormatDOT is a convenience wrapper that returns DOT as a string with default options.
func FormatDOT(fn *Function) string {
	var b bytes.Buffer
	_ = WriteDOT(&b, fn, DotOptions{Title: fn.FnName})
	return b.String()
}

// RenderSVG runs `dot -Tsvg` on the DOT output and returns SVG bytes.
func RenderSVG(fn *Function, opts DotOptions) ([]byte, error) {
	// check dot exists
	if _, err := exec.LookPath("dot"); err != nil {
		return nil, errors.New("graphviz `dot` not found in PATH")
	}
	var in bytes.Buffer
	if err := WriteDOT(&in, fn, opts); err != nil {
		return nil, err
	}
	cmd := exec.Command("dot", "-Tsvg")
	cmd.Stdin = &in
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("dot failed: %w", err)
	}
	return out, nil
}

// RenderImage renders the graph in the requested format (e.g. "png", "svg").
// It returns the raw bytes produced by dot.
func RenderImage(fn *Function, opts DotOptions, format string) ([]byte, error) {
	if _, err := exec.LookPath("dot"); err != nil {
		return nil, errors.New("graphviz `dot` not found in PATH")
	}
	var in bytes.Buffer
	if err := WriteDOT(&in, fn, opts); err != nil {
		return nil, err
	}
	cmd := exec.Command("dot", "-T"+format)
	cmd.Stdin = &in
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("dot failed: %w", err)
	}
	return out, nil
}

// SaveImage writes the rendered image (format e.g. "png" or "svg") to the
// provided filesystem path. Overwrites existing file.
func SaveImage(fn *Function, opts DotOptions, format, path string) error {
	data, err := RenderImage(fn, opts, format)
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0644)
}

// SavePNG is a convenience wrapper that renders the function as PNG and saves it.
func SavePNG(fn *Function, opts DotOptions, path string) error {
	return SaveImage(fn, opts, "png", path)
}
