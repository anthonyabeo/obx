// Package render formats Diagnostic values as human-readable, ANSI-coloured
// text written to an io.Writer.
package render

import (
	"fmt"
	"io"
	"strings"

	"github.com/anthonyabeo/obx/src/diag"
	"github.com/anthonyabeo/obx/src/source"
)

const (
	colorReset  = "\033[0m"
	colorBold   = "\033[1m"
	colorRed    = "\033[31m"
	colorYellow = "\033[33m"
	colorBlue   = "\033[34m"
	colorGray   = "\033[90m"
)

func severityColor(s diag.Severity) string {
	switch s {
	case diag.Error:
		return colorRed
	case diag.Warning:
		return colorYellow
	case diag.Info:
		return colorBlue
	default:
		return colorGray
	}
}

// Diagnostic writes d to out with source context and ANSI colour highlighting.
func Diagnostic(out io.Writer, sm *source.Manager, d diag.Diagnostic) {
	if d.Range == nil {
		fmt.Fprintf(out, "%s%s:%s %s\n", colorBold, d.Severity, colorReset, d.Message)
		return
	}

	color := severityColor(d.Severity)
	sf := sm.GetSourceFile(d.Range.Start.File)

	header := fmt.Sprintf("%s%s:%d:%d: %s%s:%s %s",
		colorBold, d.Range.Start.File, d.Range.Start.Line, d.Range.Start.Column,
		color, d.Severity, colorReset, d.Message)
	fmt.Fprintln(out, header)

	if sf == nil {
		return
	}

	lineNumWidth := len(fmt.Sprintf("%d", d.Range.End.Line))

	for line := d.Range.Start.Line; line <= d.Range.End.Line; line++ {
		lineStart := sf.LineOffsets[line-1]
		var lineEnd int
		if line < len(sf.LineOffsets) {
			lineEnd = sf.LineOffsets[line]
		} else {
			lineEnd = len(sf.Content)
		}

		lineContent := string(sf.Content[lineStart:lineEnd])
		trimmedLine := strings.ReplaceAll(strings.TrimRight(lineContent, "\n"), "\t", strings.Repeat(" ", sf.TabWidth))
		fmt.Fprintf(out, "%*d | %s\n", lineNumWidth, line, trimmedLine)

		// Compute underline bounds
		var startCol, endCol int
		switch {
		case line == d.Range.Start.Line && line == d.Range.End.Line:
			startCol = d.Range.Start.Column - 1
			endCol = d.Range.End.Column - 1
		case line == d.Range.Start.Line:
			startCol = d.Range.Start.Column - 1
			endCol = len(trimmedLine)
		case line == d.Range.End.Line:
			startCol = 0
			endCol = d.Range.End.Column - 1
		default:
			startCol = 0
			endCol = len(trimmedLine)
		}

		if endCol < startCol {
			endCol = startCol + 1
		}

		marker := strings.Repeat(" ", startCol) + color + strings.Repeat("^", endCol-startCol) + colorReset
		fmt.Fprintf(out, "%s | %s", strings.Repeat(" ", lineNumWidth), marker)

		if line == d.Range.Start.Line {
			fmt.Fprintf(out, " [%s]", d.Severity)
		}
		fmt.Fprintln(out)
	}
}
