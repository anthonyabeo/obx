package token

import (
	"fmt"
	"sync"
)

type Position struct {
	Filename string // filename, if any
	Line     int    // line number, starting at 1
	Column   int    // column number, starting at 1 (byte count)
}

func (pos *Position) String() string {
	s := pos.Filename
	if pos.Filename != "" {
		s += fmt.Sprintf(":%d:%d", pos.Line, pos.Column)
	} else {
		s = "-"
	}

	return s
}

// -----------------------------------------------------------------------------
// File

type File struct {
	name      string // file name as provided to AddFile
	base      int    // Pos value range for this file is [base...base+size]
	size      int    // file size as provided to AddFile
	curLineNo int

	mutex sync.Mutex
	lines []int // lines contains the offset of the first character for each line (the first entry is always 0)
}

func NewFile(filename string, size int) *File {
	return &File{
		name:      filename,
		size:      size,
		lines:     []int{0},
		curLineNo: 1,
	}
}

// Name returns the file name of file f as registered with AddFile.
func (f *File) Name() string {
	return f.name
}

// Base returns the base offset of file f as registered with AddFile.
func (f *File) Base() int {
	return f.base
}

// Size returns the size of file f as registered with AddFile.
func (f *File) Size() int {
	return f.size
}

func (f *File) LastLineOffset() int {
	return f.lines[len(f.lines)-1]
}

func (f *File) CurLineNo() int {
	return f.curLineNo
}

func (f *File) UpdateLineNo() {
	f.curLineNo += 1
}

// AddLine adds the line offset for a new line.
// The line offset must be larger than the offset for the previous line
// and smaller than the file size; otherwise the line offset is ignored.
func (f *File) AddLine(offset int) {
	f.mutex.Lock()
	if i := len(f.lines); (i == 0 || f.lines[i-1] < offset) && offset < f.size {
		f.lines = append(f.lines, offset)
	}
	f.mutex.Unlock()
}
