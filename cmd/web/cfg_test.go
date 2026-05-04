package web

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func newTestServer() *Server {
	return &Server{cfg: Config{MaxErrors: 50}}
}

func TestHandleCFG_SimpleModule(t *testing.T) {
	body := `{"source":"MODULE Main;\nVAR x: INTEGER;\nBEGIN\n  x := 42\nEND Main.","filename":"Main.obx", "entry": "Main"}`
	req := httptest.NewRequest(http.MethodPost, "/api/cfg", bytes.NewBufferString(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleCFG(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("want 200, got %d: %s", rr.Code, rr.Body.String())
	}

	var resp map[string]any
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	t.Logf("response ok=%v", resp["ok"])

	if ok, _ := resp["ok"].(bool); !ok {
		t.Fatalf("expected ok=true, got error: %v", resp["error"])
	}

	graphs, _ := resp["graphs"].([]any)
	if len(graphs) == 0 {
		t.Fatal("expected at least one graph entry, got none")
	}
	t.Logf("got %d graph(s)", len(graphs))

	for i, g := range graphs {
		entry := g.(map[string]any)
		fn := entry["function"].(string)
		dot := entry["dot"].(string)
		t.Logf("  [%d] module=%v function=%v  DOT length=%d", i, entry["module"], fn, len(dot))
		if !strings.HasPrefix(strings.TrimSpace(dot), "digraph") {
			t.Errorf("  graph[%d] DOT does not start with 'digraph': %.80s", i, dot)
		}
	}
}

func TestHandleCFG_WithProcedure(t *testing.T) {
	src := `MODULE Test;
VAR res: INTEGER;
PROCEDURE Fib(n: INTEGER): INTEGER;
VAR a, b: INTEGER;
BEGIN
  IF n <= 1 THEN
    RETURN n
  ELSE
    a := Fib(n - 1);
    b := Fib(n - 2);
    RETURN a + b
  END
END Fib
BEGIN
  res := Fib(10)
END Test.`

	body, _ := json.Marshal(map[string]string{"source": src, "filename": "Test.obx", "entry": "Test"})
	req := httptest.NewRequest(http.MethodPost, "/api/cfg", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleCFG(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("want 200, got %d: %s", rr.Code, rr.Body.String())
	}

	var resp map[string]any
	json.NewDecoder(rr.Body).Decode(&resp)

	if ok, _ := resp["ok"].(bool); !ok {
		t.Fatalf("expected ok=true, got error: %v", resp["error"])
	}

	graphs, _ := resp["graphs"].([]any)
	t.Logf("got %d graph(s)", len(graphs))
	for i, g := range graphs {
		entry := g.(map[string]any)
		dot := entry["dot"].(string)
		// Print full DOT for each graph
		t.Logf("  [%d] fn=%v  DOT=%d bytes\nFULL DOT:\n%s", i, entry["function"], len(dot), dot)
		// Check for known bad entities
		for _, bad := range []string{"&#34;", "&#39;", "&#0;"} {
			if strings.Contains(dot, bad) {
				t.Errorf("  graph[%d] DOT contains forbidden entity %q", i, bad)
			}
		}
	}

	if len(graphs) < 2 {
		t.Errorf("expected at least 2 functions (Fib + init), got %d", len(graphs))
	}
}

func TestHandleCFG_LoopWithExit(t *testing.T) {
	src := `MODULE LoopTest;
VAR n, s: INTEGER;
BEGIN
  n := 0;
  s := 0;
  LOOP
    IF n > 9 THEN EXIT END;
    s := s + n;
    n := n + 1
  END
END LoopTest.`

	body, _ := json.Marshal(map[string]string{"source": src, "filename": "LoopTest.obx", "entry": "LoopTest"})
	req := httptest.NewRequest(http.MethodPost, "/api/cfg", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleCFG(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("want 200, got %d: %s", rr.Code, rr.Body.String())
	}

	var resp map[string]any
	json.NewDecoder(rr.Body).Decode(&resp)

	if ok, _ := resp["ok"].(bool); !ok {
		t.Fatalf("expected ok=true, got error: %v", resp["error"])
	}

	graphs, _ := resp["graphs"].([]any)
	t.Logf("got %d graph(s)", len(graphs))
	for i, g := range graphs {
		entry := g.(map[string]any)
		dot := entry["dot"].(string)
		t.Logf("  [%d] fn=%v  DOT=%d bytes\nFULL DOT:\n%s", i, entry["function"], len(dot), dot)
		for _, bad := range []string{"&#34;", "&#39;", "&#0;"} {
			if strings.Contains(dot, bad) {
				t.Errorf("  graph[%d] DOT contains forbidden entity %q", i, bad)
			}
		}
	}
}

func TestHandleCFG_WhileWithPrintf(t *testing.T) {
	src := `MODULE LoopTest;
IMPORT IO
VAR x, y: INTEGER;
BEGIN
  x := 0;
  y := 20;
  WHILE x < y DO
    x := x + 1
  END;
  IO.PrintLn(x)
END LoopTest.`

	body, _ := json.Marshal(map[string]string{"source": src, "filename": "LoopTest.obx", "entry": "LoopTest"})
	req := httptest.NewRequest(http.MethodPost, "/api/cfg", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleCFG(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("want 200, got %d: %s", rr.Code, rr.Body.String())
	}

	var resp map[string]any
	json.NewDecoder(rr.Body).Decode(&resp)

	t.Logf("response ok=%v error=%v", resp["ok"], resp["error"])

	graphs, _ := resp["graphs"].([]any)
	for i, g := range graphs {
		entry := g.(map[string]any)
		dot := entry["dot"].(string)
		t.Logf("  [%d] fn=%v  DOT=%d bytes\nFULL DOT:\n%s", i, entry["function"], len(dot), dot)
		for _, bad := range []string{"&#34;", "&#39;", "&#0;"} {
			if strings.Contains(dot, bad) {
				t.Errorf("  graph[%d] DOT contains forbidden entity %q", i, bad)
			}
		}
	}
}

func TestHandleCFG_ParseError(t *testing.T) {
	body := `{"source":"MODULE Bad;\nVAR x: INTEGER\nEND Bad.","filename":"Bad.obx", "entry": "Bad"}`
	req := httptest.NewRequest(http.MethodPost, "/api/cfg", bytes.NewBufferString(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleCFG(rr, req)

	var resp map[string]any
	json.NewDecoder(rr.Body).Decode(&resp)
	t.Logf("parse-error response: ok=%v error=%v", resp["ok"], resp["error"])
}
