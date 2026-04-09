package source

// -----------------------------------------------------------------------
// Source locations
// -----------------------------------------------------------------------

// Position is a byte-offset based location inside a named source file.
type Position struct {
	File   string
	Offset int // byte offset in file
	Line   int // 1-based
	Column int
}

// Range is a half-open [Start, End) span inside a source file.
type Range struct {
	Start *Position
	End   *Position
}
