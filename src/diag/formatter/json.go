package formatter

import (
	"encoding/json"

	"github.com/anthonyabeo/obx/src/diag"
	"github.com/anthonyabeo/obx/src/source"
)

// jsonDiagnostic is the wire representation written by JSONFormatter.
// It mirrors Diagnostic but adds an optional source snippet.
type jsonDiagnostic struct {
	Severity string        `json:"severity"`
	Message  string        `json:"message"`
	Location *jsonLocation `json:"location,omitempty"`
	Snippet  []string      `json:"snippet,omitempty"`
}

type jsonLocation struct {
	File      string `json:"file"`
	StartLine int    `json:"startLine"`
	StartCol  int    `json:"startCol"`
	EndLine   int    `json:"endLine"`
	EndCol    int    `json:"endCol"`
}

// JSONFormatter serialises diagnostics as JSON objects (one per line).
// When Source is set, the affected source lines are embedded in the
// "snippet" field for richer tooling output.
type JSONFormatter struct {
	Source *source.Manager
}

// NewJSONFormatter creates a JSONFormatter. sm may be nil if source snippets
// are not required.
func NewJSONFormatter(sm *source.Manager) *JSONFormatter {
	return &JSONFormatter{Source: sm}
}

func (JSONFormatter) ContentType() string { return "application/json" }

func (f JSONFormatter) Format(d diag.Diagnostic) []byte {
	out := jsonDiagnostic{
		Severity: d.Severity.String(),
		Message:  d.Message,
	}

	if d.Range != nil {
		out.Location = &jsonLocation{
			File:      d.Range.Start.File,
			StartLine: d.Range.Start.Line,
			StartCol:  d.Range.Start.Column,
			EndLine:   d.Range.End.Line,
			EndCol:    d.Range.End.Column,
		}

		if f.Source != nil {
			if lines, err := f.Source.LinesInRange(*d.Range); err == nil {
				out.Snippet = lines
			}
		}
	}

	b, _ := json.Marshal(out)
	return append(b, '\n')
}
