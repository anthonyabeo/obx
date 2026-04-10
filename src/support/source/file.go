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
}

// NewSourceFile creates a File and pre-computes the line table.
func NewSourceFile(name string, content []byte) *File {
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
	}
}


// LineColToOffset converts a (1-based) line/column pair back to a byte offset.
func (sf *File) LineColToOffset(line, col int) int {
	if line < 1 || line > len(sf.LineOffsets) {
		return -1
	}
	return sf.LineOffsets[line-1] + (col - 1)
}

// OffsetToPosition converts a byte offset to a *Position.
// Columns are counted as characters (1-based); tab characters count as one.
func (sf *File) OffsetToPosition(offset int) *Position {
	i := sort.Search(len(sf.LineOffsets), func(i int) bool {
		return sf.LineOffsets[i] > offset
	}) - 1
	lineStart := sf.LineOffsets[i]
	column := 0
	for j := lineStart; j < offset && j < len(sf.Content); j++ {
		column++
	}
	return &Position{
		File:   sf.Name,
		Line:   i + 1,
		Column: column + 1,
		Offset: offset,
	}
}
