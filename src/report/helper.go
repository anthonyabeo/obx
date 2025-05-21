package report

import (
	"fmt"
	"io"
	"strings"
	"unicode/utf8"
)

func underlineRange(line string, lineNo int, r Range) string {
	if lineNo != r.Start.Line {
		return ""
	}

	start := r.Start.Column - 1
	end := r.End.Column - 1
	if end < start || start >= len(line) {
		return ""
	}
	width := end - start
	if width <= 0 {
		width = 1
	}

	return fmt.Sprintf("%s%s", strings.Repeat(" ", start), strings.Repeat("~", width))
}

func printDiagnosticTo(d Diagnostic, source *SourceManager, out io.Writer) {
	// 1. Header: Severity, File, Line, Column
	fmt.Fprintf(out, "%s: %s:%d:%d: %s\n",
		d.Severity.String(),
		d.Pos.File,
		d.Pos.Line,
		d.Pos.Column,
		d.Message,
	)

	// 2. Get relevant source line(s)
	var lines []string
	if d.LineText != "" {
		lines = []string{d.LineText}
	} else if d.Range != nil && source != nil {
		l, err := source.LinesInRange(*d.Range)
		if err == nil {
			lines = l
		}
	} else if source != nil {
		if line, err := source.Line(d.Pos); err == nil {
			lines = []string{line}
		}
	}

	// 3. Print source snippet and underline
	for i, line := range lines {
		lineNo := d.Pos.Line + i
		fmt.Fprintf(out, "%4d | %s\n", lineNo, line)

		if i == 0 && d.Range == nil {
			// Single-line position only (no range)
			fmt.Fprintf(out, "     | %s^\n", spaces(d.Pos.Column-1))
		} else if d.Range != nil {
			underline := underlineRange(line, lineNo, *d.Range)
			if underline != "" {
				fmt.Fprintf(out, "     | %s\n", underline)
			}
		}
	}
}

func spaces(n int) string {
	if n <= 0 {
		return ""
	}
	return strings.Repeat(" ", n)
}

func offsetToLineCol(src []byte, offset int) (line, col int) {
	if offset > len(src) {
		offset = len(src)
	}

	line = 1
	col = 1

	for i := 0; i < offset; {
		r, size := utf8.DecodeRune(src[i:])
		if r == '\n' {
			line++
			col = 1
		} else {
			col++
		}
		i += size
	}

	return line, col
}
