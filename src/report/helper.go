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

func underlineRange(line string, startCol, endCol int) string {
	runes := []rune(line)
	var builder strings.Builder

	for i := 1; i < startCol; i++ {
		if i-1 < len(runes) && runes[i-1] == '\t' {
			builder.WriteRune('\t') // maintain tab alignment
		} else {
			builder.WriteRune(' ')
		}
	}

	marker := "~"
	if endCol-startCol <= 1 {
		marker = "^"
	}
	underlineLen := endCol - startCol
	if underlineLen <= 0 {
		underlineLen = 1
	}
	builder.WriteString(strings.Repeat(marker, underlineLen))

	return builder.String()
}

func printDiagnosticTo(w io.Writer, sm *SourceManager, d Diagnostic) {
	pos := d.Range.Start
	end := d.Range.End

	// Choose color based on severity
	var sevColor string
	switch d.Severity {
	case Error:
		sevColor = colorRed
	case Warning:
		sevColor = colorYellow
	case Info:
		sevColor = colorBlue
	default:
		sevColor = colorGray
	}

	// Header: filename:line:col: severity: message
	fmt.Fprintf(w, "%s%s:%d:%d:%s %s%s%s: %s\n",
		colorBold, pos.File, pos.Line, pos.Column, colorReset,
		sevColor, d.Severity.String(), colorReset,
		d.Message,
	)

	// Get source lines
	src := sm.GetSourceFile(pos.File)
	if src == nil || pos.Line-1 >= len(src.Lines) {
		fmt.Fprintln(w, "  [source not available]")
		return
	}
	lines := src.Lines

	startLine := pos.Line
	endLine := end.Line

	for lineNum := startLine; lineNum <= endLine; lineNum++ {
		lineIdx := lineNum - 1
		if lineIdx < 0 || lineIdx >= len(lines) {
			continue
		}
		line := lines[lineIdx]
		lineText := string(line)

		// Print source line
		fmt.Fprintf(w, "%s%4d%s | %s\n", colorGray, lineNum, colorReset, lineText)

		// Calculate column range
		var startCol, endCol int
		if lineNum == startLine {
			startCol = pos.Column
		} else {
			startCol = 1
		}
		if lineNum == endLine {
			endCol = end.Column
		} else {
			endCol = len([]rune(lineText)) + 1
		}

		// Print underline
		fmt.Fprintf(w, "     | %s%s%s\n",
			sevColor,
			underlineRange(lineText, startCol, endCol),
			colorReset,
		)
	}
}
