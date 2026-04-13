package source

import "fmt"

// Manager is a registry of loaded source files. It also converts raw
// byte offsets to Positions and Ranges.
type Manager struct {
	files map[string]*File
}

// NewSourceManager creates an empty Manager.
func NewSourceManager() *Manager {
	return &Manager{files: make(map[string]*File)}
}

// Load registers a source file under name.
func (sm *Manager) Load(name string, content []byte) {
	sm.files[name] = NewSourceFile(name, content)
}

// GetSourceFile returns the named File, or nil if not loaded.
func (sm *Manager) GetSourceFile(name string) *File {
	if sf, ok := sm.files[name]; ok {
		return sf
	}
	return nil
}

// Line returns the full text of the line that contains pos.
func (sm *Manager) Line(pos Position) (string, error) {
	src, ok := sm.files[pos.File]
	if !ok {
		return "", fmt.Errorf("file not loaded: %s", pos.File)
	}
	if pos.Line < 1 || pos.Line > len(src.Lines) {
		return "", fmt.Errorf("invalid line number: %d", pos.Line)
	}
	return src.Lines[pos.Line-1], nil
}

// LinesInRange returns the source lines covered by rng.
func (sm *Manager) LinesInRange(rng Range) ([]string, error) {
	src, ok := sm.files[rng.Start.File]
	if !ok {
		return nil, fmt.Errorf("file not loaded: %s", rng.Start.File)
	}
	if rng.Start.Line < 1 || rng.End.Line > len(src.Lines) {
		return nil, fmt.Errorf("range out of bounds")
	}
	return src.Lines[rng.Start.Line-1 : rng.End.Line], nil
}

// Span converts a pair of byte offsets in file into a *Range.
// Returns nil if the named file has not been loaded.
func (sm *Manager) Span(file string, start, end int) *Range {
	f := sm.files[file]
	if f == nil {
		return nil
	}
	return &Range{
		Start: f.OffsetToPosition(start),
		End:   f.OffsetToPosition(end),
	}
}
