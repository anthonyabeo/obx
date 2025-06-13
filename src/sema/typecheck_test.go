package sema

import (
	"log"
	"os"
	"path/filepath"
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
                    ch: CHAR; b: BYTE; bl: BOOL;
					  i8: INT8; i16: INT16; i32: INT32; i64: INT64;
					  r: REAL; lr: LONGREAL;
					   wc: WCHAR;
					  s: SET;

				CONST
				   x = 5;
				   y = x + 10;
				   z = {1, 2, 3};
				   cat = "A";
				   str = "Hello";

				BEGIN
					i8 := -128
					i := -2147483648
					si := -32768
					i := 5;
					li := 9223372036854775807
					r := 3.14;
					bl := TRUE;
					ch := "A";
					ch := "Z";
					ch := "^"
					b := 100;
					i8 := -120;
					i16 := i8;
					i32 := i16;
					i64 := i32;
					r := i16;
					lr := i32;
					lr := 2.8657E3432
					ch := "A";
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
			x := s              (* SET to INTEGER *)
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
			p^.f := b           (* assigning BOOLEAN to INTEGER field *)
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
			x := z + 1          (* z not declared *)
		END Test.
	`,
			wantErr: true,
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

func TestExprCompatibilityValid(t *testing.T) {
	content := []byte(`
MODULE ExprCompatibilityTest;

VAR
  b1, b2: BOOL;
  c1, c2: CHAR;
  i1: INT8;
  i2: INT16;
  i3: INT32;
  i4: INT64;
  r1: REAL;
  r2: LONGREAL;
  s1, s2: SET;
  str1, str2: ARRAY 5 OF CHAR;

//PROCEDURE DummyProc;  
//END DummyProc

BEGIN
  (* Arithmetic on numerics *)
  i2 := i1 + i2;       (* INT16 *)
  i3 := i3 - i1;       (* INT32 *)
  r1 := r1 * r1;       (* REAL *)
  r2 := r1 / r2;       (* LONGREAL *)

  (* SET operations *)
  s1 := s1 + s2;
  s1 := s1 - s2;
  s1 := s1 / s2;

  (* DIV, MOD *)
  i2 := i1 DIV i2;
  i2 := i1 MOD i2;

  (* Boolean operations *)
  b1 := b1 OR b2;
  b1 := b1 & b2;
  b1 := ~b1;

  (* Comparisons on numerics *)
  b1 := i1 = i2;
  b1 := i1 # i2;
  b1 := i3 < i4;

  (* CHAR comparisons *)
  b1 := c1 <= c2;
  b1 := c1 > c2;

  (* String/array equality *)
  str1 := "test";
  str2 := "test";
  b1 := str1 = str2;
  b1 := str1 # str2;

  (* BOOLEAN comparisons *)
  b1 := b1 = b2;
  b1 := b1 # b2;

  (* SET comparisons *)
  b1 := s1 = s2;
  b1 := s1 # s2;

  (* NIL and pointer equality *)
  IF NIL = NIL THEN 
	b1 := true
  END;

  //IF DummyProc = NIL THEN
	//b1 := true
  //END

  (* IN operator *)
  b1 := 1 IN s1

  (* IS operator – simulated as a no-op if runtime types not supported *)
  //IF DummyProc IS p 
  //THEN 
    // b1 := true
  //END
END ExprCompatibilityTest.`)

	tmp := t.TempDir()
	file := filepath.Join(tmp, "test.obx")
	if err := os.WriteFile(file, []byte(content), 0644); err != nil {
		t.Fatalf("write failed: %v", err)
	}

	obx := ast.NewOberonX()
	srcMgr := report.NewSourceManager()
	reporter := report.NewBufferedReporter(srcMgr, 32, report.StdoutSink{
		Source: srcMgr,
		Writer: os.Stdout,
	})

	ctx := &report.Context{
		FileName: file,
		FilePath: file,
		Content:  content,
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

	if ctx.Reporter.ErrorCount() > 0 {
		t.Errorf("got %d unexpected errors:\n", ctx.Reporter.ErrorCount())
		ctx.Reporter.Flush()
	}
}

func TestExprCompatibilityInvalid(t *testing.T) {
	content := []byte(`
MODULE InvalidExprCompatibilityTest;
VAR
  b: BOOLEAN;
  c: CHAR;
  i: INT8;
  r: REAL;
  s: SET;
  str: ARRAY 5 OF CHAR;
  p: POINTER TO RECORD END;
  pr: PROCEDURE;

BEGIN
  (* Arithmetic on incompatible types *)
  i := i + b;        (* INVALID: BOOLEAN + INT8 *)
  i := i + s;        (* INVALID: SET + INT8 *)
  r := c * r;        (* INVALID: CHAR * REAL *)

  (* Division on non-numeric types *)
  r := s / s;        (* INVALID: SET / SET with / meaning real division *)

  (* SET operations with non-set *)
  s := s + i;        (* INVALID: SET + INT8 *)
  s := i - s;        (* INVALID: INT8 - SET *)

  (* DIV and MOD with non-integers *)
  i := i DIV r;      (* INVALID: REAL in DIV *)
  i := r MOD i;      (* INVALID: REAL in MOD *)

  (* Boolean operations with non-BOOLEAN *)
  b := b OR i;       (* INVALID: INT8 in OR *)
  b := r & b;        (* INVALID: REAL in AND *)
  b := ~i;           (* INVALID: ~INT8 *)

  (* Comparisons with incompatible types *)
  b := i < b;        (* INVALID: BOOLEAN in numeric comparison *)
  b := s = b;        (* INVALID: SET = BOOLEAN *)
  b := c > i;        (* INVALID: CHAR > INT8 *)

  (* String vs numeric *)
  b := str = i;      (* INVALID: ARRAY = INT8 *)

  (* Set membership with non-integer *)
  b := c IN s;       (* INVALID: CHAR IN SET *)

  (* IS operator with incompatible types *)
  IF b IS s THEN b := 1 END; (* INVALID: BOOLEAN IS SET *)

  (* Equality with incompatible pointer and scalar types *)
  b := p = r        (* INVALID: POINTER = REAL *)

  (* Procedure comparison with incompatible type *)
  (* b := proc = i;      INVALID: PROCEDURE = INT8 *)
END InvalidExprCompatibilityTest.

`)

	tmp := t.TempDir()
	file := filepath.Join(tmp, "test.obx")
	if err := os.WriteFile(file, []byte(content), 0644); err != nil {
		t.Fatalf("write failed: %v", err)
	}

	obx := ast.NewOberonX()
	srcMgr := report.NewSourceManager()
	reporter := report.NewBufferedReporter(srcMgr, 32, report.StdoutSink{
		Source: srcMgr,
		Writer: os.Stdout,
	})

	ctx := &report.Context{
		FileName: file,
		FilePath: file,
		Content:  content,
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

	if ctx.Reporter.ErrorCount() == 0 {
		t.Errorf("got %d unexpected errors:\n", ctx.Reporter.ErrorCount())
		ctx.Reporter.Flush()
	}
}
