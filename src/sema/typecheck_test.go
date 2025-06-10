package sema

import (
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"log"
	"os"
	"testing"
)

func TestTypeCheckerPrograms(t *testing.T) {
	tests := []struct {
		name     string
		filename string
		source   string
		wantErr  bool
	}{
		{
			name:     "Valid basic assignments",
			filename: "test.obx",
			source: `
				MODULE Test;
				VAR i: INTEGER; bl: BOOL; ch: CHAR;
					  b: BYTE;
					  i8: INT8; i16: INT16; i32: INT32; i64: INT64;
					  r: REAL; lr: LONGREAL;
					  c: CHAR; wc: WCHAR;
					  s: SET;
				BEGIN
					i := 5;
					r := 3.14;
					bl := TRUE;
					ch := "A";
					b := 100;
					i8 := -120;
					i16 := i8;
					i32 := i16;
					i64 := i32;
					r := i16;
					lr := i32;
					c := "A";
					wc := "A";
					s := {1, 3, 5};
					s := i32
				END Test.
			`,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			obx := ast.NewOberonX()
			srcMgr := report.NewSourceManager()
			reporter := report.NewBufferedReporter(srcMgr, 32, report.StdoutSink{
				Source: srcMgr,
				Writer: os.Stdout,
			})

			ctx := &report.Context{
				FileName: tt.filename,
				FilePath: tt.filename,
				Content:  []byte(tt.source),
				Source:   srcMgr,
				Reporter: reporter,
				TabWidth: 4,
				Envs:     make(map[string]*ast.Environment),
			}

			p := parser.NewParser(ctx)
			unit := p.Parse()
			if ctx.Reporter.ErrorCount() > 0 {
				ctx.Reporter.Flush()
				log.Fatalf("%d parse errors found", ctx.Reporter.ErrorCount())
			}

			obx.AddUnit(unit)
			ctx.Envs[unit.Name()] = unit.Environ()

			s := NewSema(ctx, obx)
			s.Validate()

			if tt.wantErr {
				if ctx.Reporter.ErrorCount() == 0 {
					t.Errorf("expected error but got none")
				}
			} else {
				if ctx.Reporter.ErrorCount() > 0 {
					t.Error("unexpected type checking errors:\n")
					ctx.Reporter.Flush()
				}
			}
		})
	}
}
