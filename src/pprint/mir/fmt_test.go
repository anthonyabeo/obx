package mir

import (
	"os"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func TestFormatSampleProgram(t *testing.T) {
	input := []byte(`
		MODULE Test;
		VAR x: INTEGER;
		BEGIN x := 42
		END Test.
	`)

	filename := "test.obx"
	mgr := report.NewSourceManager()
	ctx := &report.Context{
		FileName: filename,
		Content:  input,
		Env:      ast.NewEnv(),
		Source:   mgr,
		Reporter: report.NewBufferedReporter(mgr, 25, report.StdoutSink{
			Source: mgr,
			Writer: os.Stdout,
		}),
		TabWidth:  4,
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	program := parseSourceAndLowerToMIR(t, ctx)

	if err := EmitMIR(os.Stdout, program); err != nil {
		panic(err)
	}
}
