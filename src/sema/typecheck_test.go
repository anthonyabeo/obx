package sema

import (
	"log"
	"os"
	"testing"

	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
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
				VAR i: INTEGER; si: SHORTINT; li: LONGINT;  
                    ch: CHAR;b: BYTE; bl: BOOL;
					  i8: INT8; i16: INT16; i32: INT32; i64: INT64;
					  r: REAL; lr: LONGREAL;
					  c: CHAR; wc: WCHAR;
					  s: SET;

				CONST
				   x = 5;
				   y = x + 10;
				   z = {1, 2, 3};
				   cat = "A";
				   str = "Hello";

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
					s := i32;
					i := x * y
					si := i16
					li := i64
				END Test.
			`,
		},
		{
			name:     "Valid basic assignments with complex expressions",
			filename: "test.obx",
			source: `
		MODULE Test;
		VAR
			x: INTEGER;
			y: INTEGER;
			b: BOOL;
			a: ARRAY 3 OF INTEGER;
			p: POINTER TO RECORD f: INTEGER END;

		BEGIN
			x := 42;                      (* int literal *)
			x := y;                       (* variable *)
			x := x + y;                   (* binary op *)
			x := -x;                      (* unary op *)
			x := a[1];                    (* indexed designator *)
			x := p^.f;                    (* dereference and field select *)
			a[0] := 123;                  (* array assignment *)
			b := x = y;                   (* comparison op *)
			b := ~(x = y);                (* unary not *)
			(* x := ORD(TRUE);               built-in function *)
			b := x < y + 1              (* combined binary ops *)
			(* x := ABS(-y)                 built-in call with unary *)
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

func TestTypeCheckInvalidPrograms(t *testing.T) {
	tests := []struct {
		name     string
		filename string
		source   string
		wantErr  bool
	}{
		{
			name:     "Invalid assignments - incompatible types",
			filename: "test.obx",
			source: `
		MODULE Test;
		VAR
			x: INTEGER;
			r: REAL;
			b: BOOLEAN;
			s: SET;
			c: CHAR;
			a: ARRAY 3 OF INTEGER;

		BEGIN
			x := TRUE;           (* BOOLEAN to INTEGER *)
			r := x;              (* INTEGER to REAL without coercion *)
			b := 1;              (* INTEGER to BOOLEAN *)
			s := {0, TRUE};      (* BOOLEAN in set *)
			c := 300;            (* out-of-range for CHAR *)
			a := x;              (* ARRAY := INTEGER *)
			x := s;              (* SET to INTEGER *)
		END Test.
	`,
			wantErr: true,
		},
		{
			name:     "Invalid expressions in assignment",
			filename: "test.obx",
			source: `
		MODULE Test;
		VAR
			x: INTEGER;
			b: BOOLEAN;
			a: ARRAY 3 OF INTEGER;
			p: POINTER TO RECORD f: INTEGER END;

		BEGIN
			x := x + b;          (* incompatible binary op: INTEGER + BOOLEAN *)
			b := -b;             (* invalid unary minus on BOOLEAN *)
			x := a[TRUE];        (* BOOLEAN index into array *)
			x := p.f;            (* missing dereference *)
			p^.f := b;           (* assigning BOOLEAN to INTEGER field *)
		END Test.
	`,
			wantErr: true,
		},
		{
			name:     "Invalid identifiers in assignment",
			filename: "test.obx",
			source: `
		MODULE Test;
		VAR
			x: INTEGER;

		BEGIN
			y := 1;              (* y not declared *)
			x := z + 1;          (* z not declared *)
		END Test.
	`,
			wantErr: false,
		},
		{
			name:     "Non-lvalues on LHS",
			filename: "test.obx",
			source: `
		MODULE Test;
		VAR
			x, y: INTEGER;

		BEGIN
			(x + y) := 42;       (* expression is not assignable *)
			1 := x;              (* literal is not assignable *)
		END Test.
	`,
			wantErr: false,
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
