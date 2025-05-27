package report

import (
	"fmt"
	"sort"
	"strings"
)

type SourceFile struct {
	Name        string
	Content     []byte
	Lines       []string // each line includes '\n' if present
	LineOffsets []int    // byte offsets at the start of each line
	TabWidth    int
}

func NewSourceFile(name string, content []byte, tabWidth int) *SourceFile {
	lines := strings.SplitAfter(string(content), "\n")
	offsets := make([]int, len(lines))

	offset := 0
	for i, line := range lines {
		offsets[i] = offset
		offset += len(line)
	}

	return &SourceFile{
		Name:        name,
		Content:     content,
		Lines:       lines,
		LineOffsets: offsets,
		TabWidth:    tabWidth,
	}
}

func (sf *SourceFile) OffsetToLineCol(offset int, tabWidth int) (line, col int) {
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

func (sf *SourceFile) LineColToOffset(line, col int) int {
	if line < 1 || line > len(sf.LineOffsets) {
		return -1
	}
	return sf.LineOffsets[line-1] + (col - 1)
}

func (sf *SourceFile) OffsetToPosition(offset int) *Position {
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

type SourceManager struct {
	files    map[string]*SourceFile
	tabWidth int
}

func NewSourceManager() *SourceManager {
	return &SourceManager{files: make(map[string]*SourceFile), tabWidth: 4}
}

func (sm *SourceManager) Load(name string, content []byte, tabWidth int) {
	sm.files[name] = NewSourceFile(name, content, tabWidth)
}

func (sm *SourceManager) GetSourceFile(name string) *SourceFile {
	if sf, ok := sm.files[name]; ok {
		return sf
	}
	return nil
}

// Line returns the full line for a given position
func (sm *SourceManager) Line(pos Position) (string, error) {
	src, ok := sm.files[pos.File]
	if !ok {
		return "", fmt.Errorf("file not loaded: %s", pos.File)
	}
	if pos.Line < 1 || pos.Line > len(src.Lines) {
		return "", fmt.Errorf("invalid line number: %d", pos.Line)
	}
	return src.Lines[pos.Line-1], nil
}

func (sm *SourceManager) LinesInRange(rng Range) ([]string, error) {
	src, ok := sm.files[rng.Start.File]
	if !ok {
		return nil, fmt.Errorf("file not loaded: %s", rng.Start.File)
	}

	if rng.Start.Line < 1 || rng.End.Line > len(src.Lines) {
		return nil, fmt.Errorf("range out of bounds")
	}

	return src.Lines[rng.Start.Line-1 : rng.End.Line], nil
}

//func (sm *SourceManager) Pos(file string, offset int) (*Position, error) {
//	sf, ok := sm.files[file]
//	if !ok {
//		return &Position{}, fmt.Errorf("file not found: %s", file)
//	}
//
//	line, col := sf.OffsetToLineCol(offset, sm.tabWidth)
//	return &Position{File: file, Offset: offset, Line: line, Column: col}, nil
//}

//func (sm *SourceManager) Range(file string, start, end int) (*Range, error) {
//	p1, err := sm.Pos(file, start)
//	if err != nil {
//		return &Range{}, err
//	}
//	p2, err := sm.Pos(file, end)
//	if err != nil {
//		return &Range{}, err
//	}
//	return &Range{Start: p1, End: p2}, nil
//}

func (sm *SourceManager) Span(file string, start, end int) *Range {
	f := sm.files[file]
	return &Range{
		Start: f.OffsetToPosition(start),
		End:   f.OffsetToPosition(end),
	}
}
