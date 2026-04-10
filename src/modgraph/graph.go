package modgraph

// Header holds the metadata extracted from a single .obx file before it is
// fully parsed.  One file → one Header.
type Header struct {
	Key      ModuleKey // derived from the file's path relative to a root
	File     string    // absolute path to the source file
	StartPos int       // byte offset of the MODULE/DEFINITION keyword
	EndPos   int       // byte offset past the end of the unit (len(content))
	Imports  []Import  // import list in source order
}

func (h Header) String() string { return h.Key.String() }

// Import represents one entry in a module's import list.
type Import struct {
	Alias string    // local alias, e.g. "D" in "D := Collections.Drawing"
	Key   ModuleKey // canonical key built from all dot-separated segments
}

// ImportGraph is a directed graph of module dependencies keyed by the string
// form of each module's ModuleKey.
type ImportGraph struct {
	Headers map[string]Header   // key → Header
	Adj     map[string][]string // key → keys of direct dependencies
}
