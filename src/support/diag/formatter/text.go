package formatter

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/support/diag"
	"github.com/anthonyabeo/obx/src/support/source"
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

// TextFormatter produces ANSI-coloured, human-readable diagnostic text with
// source-snippet context and caret underlines.
type TextFormatter struct {
	Source   *source.Manager
	TabWidth int // spaces per tab in source snippets; 0 → default 4
}

// NewTextFormatter creates a TextFormatter. tabWidth <= 0 uses the default of 4.
func NewTextFormatter(sm *source.Manager, tabWidth int) *TextFormatter {
	return &TextFormatter{Source: sm, TabWidth: tabWidth}
}

func (f *TextFormatter) ContentType() string { return "text/plain; charset=utf-8" }

// byteColToVisual converts a 0-based byte column within rawLine to the
// corresponding 0-based visual column after tab expansion (each tab expands
// to exactly tabWidth spaces, matching the strings.ReplaceAll display).
func byteColToVisual(rawLine []byte, byteCol int, tabWidth int) int {
	visual := 0
	for i := 0; i < byteCol && i < len(rawLine); i++ {
		if rawLine[i] == '\t' {
			visual += tabWidth
		} else {
			visual++
		}
	}
	return visual
}

func (f *TextFormatter) Format(d diag.Diagnostic) []byte {
	tabWidth := f.TabWidth
	if tabWidth <= 0 {
		tabWidth = 4
	}

	var buf bytes.Buffer

	if d.Range == nil {
		fmt.Fprintf(&buf, "%s%s:%s %s\n", colorBold, d.Severity, colorReset, d.Message)
		return buf.Bytes()
	}

	color := severityColor(d.Severity)
	sf := f.Source.GetSourceFile(d.Range.Start.File)

	fmt.Fprintf(&buf, "%s%s:%d:%d: %s%s:%s %s\n",
		colorBold, d.Range.Start.File, d.Range.Start.Line, d.Range.Start.Column,
		color, d.Severity, colorReset, d.Message)

	if sf == nil {
		return buf.Bytes()
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

		rawLine := sf.Content[lineStart:lineEnd]
		lineContent := string(rawLine)
		trimmedLine := strings.ReplaceAll(strings.TrimRight(lineContent, "\n"), "\t", strings.Repeat(" ", tabWidth))
		fmt.Fprintf(&buf, "%*d | %s\n", lineNumWidth, line, trimmedLine)

		// Column values in the diagnostic are byte-based (tabs count as 1).
		// Convert to visual columns to align with the tab-expanded display.
		var startCol, endCol int
		switch {
		case line == d.Range.Start.Line && line == d.Range.End.Line:
			startCol = byteColToVisual(rawLine, d.Range.Start.Column-1, tabWidth)
			endCol = byteColToVisual(rawLine, d.Range.End.Column-1, tabWidth)
		case line == d.Range.Start.Line:
			startCol = byteColToVisual(rawLine, d.Range.Start.Column-1, tabWidth)
			endCol = len(trimmedLine)
		case line == d.Range.End.Line:
			startCol = 0
			endCol = byteColToVisual(rawLine, d.Range.End.Column-1, tabWidth)
		default:
			startCol = 0
			endCol = len(trimmedLine)
		}

		if endCol < startCol {
			endCol = startCol + 1
		}

		marker := strings.Repeat(" ", startCol) + color + strings.Repeat("^", endCol-startCol) + colorReset
		fmt.Fprintf(&buf, "%s | %s", strings.Repeat(" ", lineNumWidth), marker)

		if line == d.Range.Start.Line {
			fmt.Fprintf(&buf, " [%s]", d.Severity)
		}
		fmt.Fprintln(&buf)
	}

	return buf.Bytes()
}
