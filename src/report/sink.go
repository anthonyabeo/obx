package report

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"os"
)

type DiagnosticSink interface {
	Emit(d Diagnostic)
}

type StdoutSink struct {
	Source *SourceManager
	Writer io.Writer // typically os.Stderr
}

func (s StdoutSink) Emit(d Diagnostic) {
	printDiagnosticTo(s.Writer, s.Source, d)
}

type FileSink struct {
	Source *SourceManager
	File   *os.File
}

func (s *FileSink) Emit(d Diagnostic) {
	printDiagnosticTo(s.File, s.Source, d)
}

type JSONNetworkSink struct {
	Endpoint string
	Client   *http.Client
}

func (s *JSONNetworkSink) Emit(d Diagnostic) {
	payload, _ := json.Marshal(d)
	req, _ := http.NewRequest("POST", s.Endpoint, bytes.NewReader(payload))
	req.Header.Set("Content-Type", "application/json")
	s.Client.Do(req)
}
