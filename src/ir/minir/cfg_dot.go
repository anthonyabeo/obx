package minir

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

// RenderSVG runs `dot -Tsvg` on the DOT output and returns SVG bytes.
func RenderSVG(fn *Function) ([]byte, error) {
	// check dot exists
	if _, err := exec.LookPath("dot"); err != nil {
		return nil, errors.New("graphviz `dot` not found in PATH")
	}
	return RenderImage(fn, "svg")
}

// FormatDOT returns the DOT source for fn's graph. Kept for compatibility with
// older callsites and tests that expect a package-level helper.
func FormatDOT(fn *Function) string {
	if fn == nil {
		return ""
	}
	return fn.OutputDOT()
}

// RenderImage renders the graph in the requested format (e.g. "png", "svg").
// It returns the raw bytes produced by dot.
func RenderImage(fn *Function, format string) ([]byte, error) {
	if _, err := exec.LookPath("dot"); err != nil {
		return nil, errors.New("graphviz `dot` not found in PATH")
	}
	var in = fn.OutputDOT()

	cmd := exec.Command("dot", "-T"+format)
	cmd.Stdin = strings.NewReader(in)
	var out bytes.Buffer
	cmd.Stdout = &out
	var stderr bytes.Buffer
	cmd.Stderr = &stderr

	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("dot command failed: %v, stderr: %s", err, stderr.String())
	}
	return out.Bytes(), nil
}

// SaveImage writes the rendered image (format e.g. "png" or "svg") to the
// provided filesystem path. Overwrites existing file.
func SaveImage(fn *Function, format, path string) error {
	data, err := RenderImage(fn, format)
	if err != nil {
		return err
	}
	return os.WriteFile(path, data, 0644)
}

// SavePNG is a convenience wrapper that renders the function as PNG and saves it.
func SavePNG(fn *Function, path string) error {
	return SaveImage(fn, "png", path)
}
