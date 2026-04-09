package source

import (
	"sort"
	"strings"
)

// -----------------------------------------------------------------------
// File
// -----------------------------------------------------------------------

// File holds the raw content and pre-computed line table for a single
// source file.
type File struct {
	Name        string
	Content     []byte
	Lines       []string // each line includes '\n' if present
	LineOffsets []int    // byte offsets at the start of each line
	TabWidth    int
}

// NewSourceFile creates a File and pre-computes the line table.
func NewSourceFile(name string, content []byte, tabWidth int) *File {
	lines := strings.SplitAfter(string(content), "\n")
	offsets := make([]int, len(lines))

	offset := 0
	for i, line := range lines {
		offsets[i] = offset
		offset += len(line)
	}

	return &File{
		Name:        name,
		Content:     content,
		Lines:       lines,
		LineOffsets: offsets,
		TabWidth:    tabWidth,
	}
}

// OffsetToLineCol converts a byte offset to a (1-based) line and column number.
func (sf *File) OffsetToLineCol(offset int, tabWidth int) (line, col int) {
	line = 1
	col = 1
	for i := 0; i < offset && i < len(sf.Content); i++ {
		b := sf.Content[i]
		switch b {
		case '\n':
			line++
			col = 1
		case '\t':
			spaces := tabWidth - ((col - 1) % tabWidth)
			col += spaces
		default:
			col++
		}
	}
	return
}

// LineColToOffset converts a (1-based) line/column pair back to a byte offset.
func (sf *File) LineColToOffset(line, col int) int {
	if line < 1 || line > len(sf.LineOffsets) {
		return -1
	}
	return sf.LineOffsets[line-1] + (col - 1)
}

// OffsetToPosition converts a byte offset to a *Position with file, line and
// column fields filled in.
func (sf *File) OffsetToPosition(offset int) *Position {
	i := sort.Search(len(sf.LineOffsets), func(i int) bool {
		return sf.LineOffsets[i] > offset
	}) - 1
	lineStart := sf.LineOffsets[i]
	column := 0
	for j := lineStart; j < offset && j < len(sf.Content); j++ {
		if sf.Content[j] == '\t' {
			column += sf.TabWidth
		} else {
			column++
		}
	}
	return &Position{
		File:   sf.Name,
		Line:   i + 1,
		Column: column + 1,
		Offset: offset,
	}
}
