package report

import (
	"fmt"
	"io"
	"strings"
)

const (
	colorReset  = "\033[0m"
	colorBold   = "\033[1m"
	colorRed    = "\033[31m"
	colorYellow = "\033[33m"
	colorBlue   = "\033[34m"
	colorGray   = "\033[90m"
)

func PrintDiagnostic(out io.Writer, sm *SourceManager, diag Diagnostic) {
	var color string
	switch diag.Severity {
	case Error:
		color = colorRed
	case Warning:
		color = colorYellow
	case Info:
		color = colorBlue
	default:
		color = colorGray
	}

	sf := sm.files[diag.Range.Start.File]
	header := fmt.Sprintf("%s%s:%d:%d: %s%s:%s %s",
		colorBold, diag.Range.Start.File, diag.Range.Start.Line, diag.Range.Start.Column,
		color, diag.Severity, colorReset, diag.Message)
	fmt.Fprintln(out, header)

	lineNumWidth := len(fmt.Sprintf("%d", diag.Range.End.Line))

	for line := diag.Range.Start.Line; line <= diag.Range.End.Line; line++ {
		lineStart := sf.LineOffsets[line-1]
		var lineEnd int
		if line < len(sf.LineOffsets) {
			lineEnd = sf.LineOffsets[line]
		} else {
			lineEnd = len(sf.Content)
		}
		lineContent := string(sf.Content[lineStart:lineEnd])
		trimmedLine := strings.ReplaceAll(strings.TrimRight(lineContent, "\n"), "\t", strings.Repeat(" ", sf.TabWidth))

		// Print source line with line number
		fmt.Fprintf(out, "%*d | %s\n", lineNumWidth, line, trimmedLine)

		// Compute underline range
		var startCol, endCol int
		if line == diag.Range.Start.Line {
			startCol = diag.Range.Start.Column - 1
		} else {
			startCol = 0
		}
		if line == diag.Range.End.Line {
			endCol = diag.Range.End.Column - 1
		} else {
			endCol = len(trimmedLine)
		}
		if endCol < startCol {
			endCol = startCol + 1
		}
		marker := strings.Repeat(" ", startCol) + color + strings.Repeat("^", endCol-startCol) + colorReset
		labelText := fmt.Sprintf(" [%s]", diag.Severity)
		fmt.Fprintf(out, "%s | %s%s\n", strings.Repeat(" ", lineNumWidth), marker, labelText)
	}
}
