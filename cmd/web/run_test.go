package web

import (
	"bytes"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

func TestHandleRun_ExecutesProgram(t *testing.T) {
	if _, _, err := hostRunBackendConfig(); err != nil {
		t.Skipf("run toolchain unavailable on this host: %v", err)
	}

	src := `MODULE Main;
IMPORT IO;
BEGIN
		  IO.WriteLn("42")
END Main.`

	body, _ := json.Marshal(map[string]any{"source": src, "filename": "Main.obx"})
	req := httptest.NewRequest(http.MethodPost, "/api/run", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	rr := httptest.NewRecorder()

	newTestServer().HandleRun(rr, req)

	if rr.Code != http.StatusOK {
		t.Fatalf("want 200, got %d: %s", rr.Code, rr.Body.String())
	}

	var resp map[string]any
	if err := json.NewDecoder(rr.Body).Decode(&resp); err != nil {
		t.Fatalf("decode response: %v", err)
	}
	if ok, _ := resp["ok"].(bool); !ok {
		t.Fatalf("expected ok=true, got error: %v; diagnostics=%v; output=%v", resp["error"], resp["diagnostics"], resp["output"])
	}
	output, _ := resp["output"].(string)
	if !strings.Contains(output, "42") {
		t.Fatalf("expected output to contain 42, got: %q", output)
	}
}


