package sema

import (
	"log"
	"os"
	"path/filepath"
	"strings"
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
		TYPE User = RECORD 
				username: array 32 of char
  				age: integer
  				salary: real
		END
		TYPE Table = ARRAY 10 OF real;

		VAR
			x: INTEGER;
			y: INTEGER;
			b: BOOL;
			a: ARRAY 3 OF INTEGER;
			p: POINTER TO RECORD f: INTEGER END;
			usr: User
			c: CHAR;
			ptr: POINTER TO User;
			t: Table;
			r: REAL;

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
			x := ORD(TRUE);              (* built-in function *)
			b := x < y + 1              (* combined binary ops *)
			x := ABS(-y)                (* built-in call with unary *)
			usr.username := "foobar"
			usr.age := x
			usr.salary := 346.32
			c := usr.username[0] (* record field access *)
			ptr^.username[3] := "A" (* pointer dereference and field access *)
			t[0] := 3.14                (* array assignment *)
			c := CHR(65)            (* built-in function for char *)
			(*y := MIN(int32)*)
			r := MIN(3.14, 2) (* built-in function with multiple args *)
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

type procTestCase struct {
	name        string
	code        string
	shouldPass  bool
	expectError string
}

type procTestEntry struct {
	procName string
	cases    []procTestCase
}

var predeclaredProcTests = []procTestEntry{
	{
		procName: "ABS",
		cases: []procTestCase{
			{
				name: "Valid ABS INT8",
				code: `
					MODULE Test;
					VAR x: INT8;
					BEGIN x := ABS(-5)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "Valid ABS INT16",
				code: `
					MODULE Test;
					VAR x: INT16;
					BEGIN x := ABS(-12345)
					END Test.`,
				shouldPass: true,
			},
			{
				name:       "Valid ABS INT32",
				code:       `MODULE Test; VAR x, y: INT32; BEGIN y := ABS(x) END Test.`,
				shouldPass: true,
			},
			{
				name: "ABS with REAL",
				code: `MODULE Test;
						VAR x: REAL;
						BEGIN x := ABS(-3.14)
						END Test.`,
				shouldPass: true,
			},
			{
				name: "ABS with non-numeric",
				code: `MODULE Test;
						VAR x: BOOL;
						BEGIN x := ABS(TRUE)
						END Test.`,
				shouldPass:  false,
				expectError: "'ABS' expects a numeric argument",
			},
			{
				name: "ABS with no arguments",
				code: `MODULE Test;
					VAR x: INT8;
					BEGIN x := ABS()
					END Test.`,
				shouldPass:  false,
				expectError: "'ABS' expects exactly one argument",
			},
			{
				name: "ABS with too many arguments",
				code: `MODULE Test;
					VAR x: INT8;
					BEGIN x := ABS(1, 2)
					END Test.`,
				shouldPass:  false,
				expectError: "'ABS' expects exactly one argument",
			},
			{
				name:        "Invalid ABS on BOOLEAN",
				code:        `MODULE Test; VAR x, b: BOOL; BEGIN x := ABS(b) END Test.`,
				shouldPass:  false,
				expectError: "'ABS' expects a numeric argument",
			},
			{
				name:        "Too many arguments",
				code:        `MODULE Test; VAR x, y: INT32; BEGIN y := ABS(x, x)  END Test.`,
				shouldPass:  false,
				expectError: "expects exactly one argument",
			},
		},
	},
	{
		procName: "CHR",
		cases: []procTestCase{
			{
				name: "CAP with lowercase CHAR",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP('a')
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with CHAR literal",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP(0ffx)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with CHAR literal",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP(0x)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with WCHAR literal",
				code: `MODULE Test;
					VAR c: WCHAR;
					BEGIN c := CAP(4F60x)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with WCHAR literal",
				code: `MODULE Test;
					VAR c: WCHAR;
					BEGIN c := CAP(0d7ffx)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with WCHAR literal",
				code: `MODULE Test;
					VAR c: WCHAR;
					BEGIN c := CAP(0f900x)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with WCHAR literal",
				code: `MODULE Test;
					VAR c: WCHAR;
					BEGIN c := CAP(0ffffx)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with uppercase CHAR",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP('Z')
					END Test.`,
				shouldPass: true,
			},
			{
				name: "CAP with numeric argument",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP(65)
					END Test.`,
				shouldPass:  false,
				expectError: "'CAP' expects a char argument",
			},
			{
				name: "CAP with boolean argument",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP(TRUE)
					END Test.`,
				shouldPass:  false,
				expectError: "'CAP' expects a char argument",
			},
			{
				name: "CAP with no arguments",
				code: `MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP()
					END Test.`,
				shouldPass:  false,
				expectError: "'CAP' expects exactly one argument",
			},
			{
				name: "CAP with too many arguments",
				code: `
					MODULE Test;
					VAR c: CHAR;
					BEGIN c := CAP('a', 'b')
					END Test.`,
				shouldPass:  false,
				expectError: "'CAP' expects exactly one argument",
			},
		},
	},
	{
		procName: "DEC",
		cases: []procTestCase{
			{
				name: "DEC with one argument: integer variable",
				code: `MODULE Test;
					VAR x: INTEGER;
					BEGIN DEC(x)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "DEC with one argument: enum variable",
				code: `MODULE Test;
					TYPE Color = (Red, Green, Blue);
					VAR c: Color;
					BEGIN DEC(c)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "DEC with one argument: non-integer, non-enum",
				code: `MODULE Test;
					VAR r: REAL;
					BEGIN DEC(r)
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects first argument to be an integer or enum type",
			},
			{
				name: "DEC with two arguments: valid integer decrement",
				code: `MODULE Test;
					VAR x: INTEGER;
					BEGIN DEC(x, 3)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "DEC with two arguments: enum variable, valid integer",
				code: `MODULE Test;
					TYPE State = (Start, Middle, Last);
					VAR s: State;
					BEGIN DEC(s, 1)
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects the first argument to be an assignable integer",
			},
			{
				name: "DEC with two arguments: non-integer decrement",
				code: `
					MODULE Test;
					VAR x: INTEGER;
					BEGIN DEC(x, 1.5)
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects the second argument to be an integer",
			},
			{
				name: "DEC with two arguments: first argument not a variable",
				code: `MODULE Test;
					CONST x = 5;
					BEGIN DEC(x, 1)
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects the first argument to be an assignable integer",
			},
			{
				name: "DEC with too many arguments",
				code: `MODULE Test;
					VAR x: INTEGER;
					BEGIN DEC(x, 1, 2)
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects one or two arguments",
			},
			{
				name: "DEC with zero arguments",
				code: `MODULE Test;
					BEGIN DEC()
					END Test.`,
				shouldPass:  false,
				expectError: "'DEC' expects one or two arguments",
			},
		},
	},
	{
		procName: "INC",
		cases: []procTestCase{
			{
				name: "INC with one argument: integer variable",
				code: `MODULE Test;
					VAR x: INTEGER;
					BEGIN INC(x)
					END Test.`,
				shouldPass: true,
			},
			{
				name: "INC with one argument: enum variable",
				code: `
			MODULE Test;
			TYPE Color = (Red, Green, Blue);
			VAR c: Color;
			BEGIN INC(c)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INC with one argument: real variable (invalid)",
				code: `
			MODULE Test;
			VAR r: REAL;
			BEGIN INC(r)
			END Test.`,
				expectError: "'INC' expects an integer or enum type",
			},
			{
				name: "INC with two arguments: integer variable and constant",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN INC(x, 5)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INC with two arguments: enum variable and constant",
				code: `
			MODULE Test;
			TYPE State = (Init, Running, Done);
			VAR s: State;
			BEGIN INC(s, 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'INC' expects the first argument to be an assignable integer",
			},
			{
				name: "INC with two arguments: second argument not integer",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN INC(x, 1.0)
			END Test.`,
				expectError: "'INC' expects the second argument to be an integer",
			},
			{
				name: "INC with two arguments: first argument not a variable",
				code: `
			MODULE Test;
			CONST c = 5;
			BEGIN INC(c, 1)
			END Test.`,
				expectError: "'INC' expects the first argument to be an assignable integer",
			},
			{
				name: "INC with too many arguments",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN INC(x, 1, 2)
			END Test.`,
				expectError: "'INC' expects one or two arguments",
			},
			{
				name: "INC with zero arguments",
				code: `
			MODULE Test;
			BEGIN INC()
			END Test.`,
				expectError: "'INC' expects one or two arguments",
			},
		},
	},
	{
		procName: "INCL",
		cases: []procTestCase{
			{
				name: "INCL with valid SET variable and integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 5)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with constant expression index",
				code: `
			MODULE Test;
			VAR s: SET;
			CONST i = 10;
			BEGIN INCL(s, i)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with large integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 999999999)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with MAX(SET) integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 4294967295)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with out-of-bounds integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 4294967296)
			END Test.`,
				shouldPass:  false,
				expectError: "'INCL' expects the second argument to be an integer in the range [0, MAX(SET)]",
			},
			{
				name: "INCL with non-integer index",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 1.5)
			END Test.`,
				expectError: "'INCL' expects the second argument to be an integer",
			},
			{
				name: "INCL with non-SET variable",
				code: `
			MODULE Test;
			VAR s: INTEGER;
			BEGIN INCL(s, 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'INCL' expects the first argument to be a set",
			},
			{
				name: "INCL with set union as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s + s, 1)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s / s, 4294967295)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s - s, 0)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s * s, 0)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "INCL with missing arguments",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s)
			END Test.`,
				expectError: "'INCL' expects exactly two arguments",
			},
			{
				name: "INCL with too many arguments",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN INCL(s, 1, 2)
			END Test.`,
				expectError: "'INCL' expects exactly two arguments",
			},
		},
	},
	{
		procName: "EXCL",
		cases: []procTestCase{
			{
				name: "EXCL with valid SET variable and integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 5)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with constant expression index",
				code: `
			MODULE Test;
			VAR s: SET;
			CONST i = 7;
			BEGIN EXCL(s, i)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with non-integer index",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 3.14)
			END Test.`,
				shouldPass:  false,
				expectError: "'EXCL' expects the second argument to be an integer",
			},
			{
				name: "EXCL with valid SET variable and integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 5)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with constant expression index",
				code: `
			MODULE Test;
			VAR s: SET;
			CONST i = 10;
			BEGIN EXCL(s, i)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with large integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 999999999)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with MAX(SET) integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 4294967295)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with out-of-bounds integer",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 4294967296)
			END Test.`,
				shouldPass:  false,
				expectError: "'EXCL' expects the second argument to be an integer in the range [0, MAX(SET)]",
			},
			{
				name: "EXCL with non-integer index",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 'a')
			END Test.`,
				expectError: "'EXCL' expects the second argument to be an integer",
			},
			{
				name: "EXCL with non-SET variable",
				code: `
			MODULE Test;
			VAR s: INTEGER;
			BEGIN EXCL(s, 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'EXCL' expects the first argument to be a set",
			},
			{
				name: "EXCL with set union as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s + s, 1)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s / s, 4294967295)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s - s, 0)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with set difference as first argument",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s * s, 0)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "EXCL with missing arguments",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s)
			END Test.`,
				expectError: "'EXCL' expects exactly two arguments",
			},
			{
				name: "EXCL with too many arguments",
				code: `
			MODULE Test;
			VAR s: SET;
			BEGIN EXCL(s, 1, 2)
			END Test.`,
				expectError: "'EXCL' expects exactly two arguments",
			},
		},
	},
	{
		procName: "ASSERT",
		cases: []procTestCase{
			{
				name: "ASSERT with valid BOOLEAN expression",
				code: `
			MODULE Test;
			VAR x: BOOL;
			BEGIN ASSERT(x)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "ASSERT with TRUE literal",
				code: `
			MODULE Test;
			BEGIN ASSERT(TRUE)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "ASSERT with valid second INTEGER constant argument",
				code: `
			MODULE Test;
			BEGIN ASSERT(TRUE, 42)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "ASSERT with second argument that is not constant",
				code: `
			MODULE Test;
			VAR code: INTEGER;
			BEGIN ASSERT(TRUE, code)
			END Test.`,
				shouldPass:  false,
				expectError: "'ASSERT' expects the second argument to be an integer constant",
			},
			{
				name: "ASSERT with non-BOOLEAN first argument",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN ASSERT(x)
			END Test.`,
				expectError: "'ASSERT' expects a boolean type",
			},
			{
				name: "ASSERT with negative constant code",
				code: `
			MODULE Test;
			BEGIN ASSERT(TRUE, -1)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "ASSERT with too many arguments",
				code: `
			MODULE Test;
			BEGIN ASSERT(TRUE, 1, 2)
			END Test.`,
				expectError: "'ASSERT' expects one or two arguments",
			},
			{
				name: "ASSERT with no arguments",
				code: `
			MODULE Test;
			BEGIN ASSERT()
			END Test.`,
				expectError: "'ASSERT' expects one or two arguments",
			},
		},
	},
	{
		procName: "BYTES",
		cases: []procTestCase{
			{
				name: "BYTES(array, n) with BYTE array and INT32",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF BYTE;
			VAR x: INT32;
			VAR n: INTEGER;
			BEGIN BYTES(a, x)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "BYTES(array, n) with CHAR array and SET",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			VAR x: SET;
			VAR n: INTEGER;
			BEGIN BYTES(a, x)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "BYTES(array, n) with array of INTEGER",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF INTEGER;
			VAR x: INT32;
			VAR n: INTEGER;
			BEGIN BYTES(a, x)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects the first argument to be an array of byte or char",
			},
			{
				name: "BYTES(array, n) with BOOLEAN",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF BYTE;
			VAR b: BOOL;
			BEGIN BYTES(a, b)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects the second argument to be numeric or set",
			},
			{
				name: "BYTES() with no arguments",
				code: `
			MODULE Test;
			BEGIN BYTES()
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects exactly two arguments",
			},
			{
				name: "BYTES() with too many arguments",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF BYTE;
			VAR n: INTEGER;
			BEGIN BYTES(a, n, n)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects exactly two arguments",
			},
			{
				name: "BYTES() with non-array first argument",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			VAR n: INTEGER;
			BEGIN BYTES(x, n)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects the first argument to be an array of byte or char",
			},
			{
				name: "BYTES() with non-numeric second argument",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF BYTE;
			VAR b: BOOL;
			BEGIN BYTES(a, b)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects the second argument to be numeric or set",
			},
			{
				name: "BYTES() with array of CHAR",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF CHAR;
			VAR n: INTEGER;
			BEGIN BYTES(a, n)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "BYTES() with array of BYTE",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF BYTE;
			VAR n: INTEGER;
			BEGIN BYTES(a, n)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "BYTES() with array of WCHAR",
				code: `
			MODULE Test;
			VAR a: ARRAY 8 OF WCHAR;
			VAR n: INTEGER;
			BEGIN BYTES(a, n)
			END Test.`,
				shouldPass:  false,
				expectError: "'BYTES' expects the first argument to be an array of byte or char",
			},
		},
	},
	{
		procName: "HALT",
		cases: []procTestCase{
			{
				name: "HALT with integer constant",
				code: `
			MODULE Test;
			BEGIN HALT(1)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "HALT with zero",
				code: `
			MODULE Test;
			BEGIN HALT(0)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "HALT with large int constant",
				code: `
			MODULE Test;
			BEGIN HALT(32767)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "HALT with variable",
				code: `
			MODULE Test;
			VAR n: INTEGER;
			BEGIN HALT(n)
			END Test.`,
				shouldPass:  false,
				expectError: "'HALT' expects an integer constant",
			},
			{
				name: "HALT with REAL constant",
				code: `
			MODULE Test;
			BEGIN HALT(1.0)
			END Test.`,
				shouldPass:  false,
				expectError: "'HALT' expects an integer constant",
			},
			{
				name: "HALT with no arguments",
				code: `
			MODULE Test;
			BEGIN HALT()
			END Test.`,
				shouldPass:  false,
				expectError: "'HALT' expects exactly one argument",
			},
			{
				name: "HALT with two arguments",
				code: `
			MODULE Test;
			BEGIN HALT(1, 2)
			END Test.`,
				shouldPass:  false,
				expectError: "'HALT' expects exactly one argument",
			},
			{
				name: "HALT with negative constant",
				code: `
			MODULE Test;
			BEGIN HALT(-1)
			END Test.`,
				shouldPass: true,
			},
		},
	},
	{
		procName: "NEW",
		cases: []procTestCase{
			{
				name: "NEW fixed array",
				code: `
			MODULE Test;
			TYPE A = ARRAY 10 OF INTEGER;
			VAR arr: A;
			BEGIN NEW(arr)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NEW pointer to record",
				code: `
			MODULE Test;
			TYPE Rec = RECORD
				field: INTEGER
			END;
			VAR p: POINTER TO Rec;
			BEGIN NEW(p)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NEW pointer to open array 1D",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 42)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NEW pointer to open array 2D",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF ARRAY OF CHAR;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 3, 10)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NEW pointer to 2D open array - too few args",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 5)
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW': expected 2 dimension lengths for open array type",
			},
			{
				name: "NEW with too many args",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 1, 2)
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW': expected 1 dimension lengths for open array type",
			},
			{
				name: "NEW with non-integer dimension",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, TRUE)
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW': dimension length must be integer",
			},
			{
				name: "NEW with missing open array dimensions",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p)
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW' expects a pointer to a record or a fixed array",
			},
			{
				name: "NEW with arg on fixed array",
				code: `
			MODULE Test;
			TYPE A = ARRAY 10 OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 1)
			END Test.`,
				expectError: "'NEW' expects a pointer to an open array",
			},
			{
				name: "NEW with no args",
				code: `
			MODULE Test;
			BEGIN NEW()
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW' expects at least one argument",
			},
			{
				name: "NEW with too many args",
				code: `
			MODULE Test;
			TYPE A = ARRAY OF INTEGER;
			     P = ^A;
			VAR p: P;
			BEGIN NEW(p, 1, 2, 3)
			END Test.`,
				shouldPass:  false,
				expectError: "'NEW': expected 1 dimension lengths for open array type",
			},
		},
	},
	{
		procName: "NUMBER",
		cases: []procTestCase{
			{
				name: "NUMBER to BYTE",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    n: BYTE;
			BEGIN NUMBER(n, a)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NUMBER to CHAR",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF CHAR;
			    n: CHAR;
			BEGIN NUMBER(n, a)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects the first argument to be numeric or set",
			},
			{
				name: "NUMBER to INT32",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    n: INT32;
			BEGIN NUMBER(n, a)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NUMBER to SET",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    s: SET;
			BEGIN NUMBER(s, a)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "NUMBER with non-numeric target",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    s: BOOL;
			BEGIN NUMBER(s, a)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects the first argument to be numeric or set",
			},
			{
				name: "NUMBER with wrong array element type",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF INTEGER;
			    n: INT32;
			BEGIN NUMBER(n, a)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects the second argument to be an array of byte or char",
			},
			{
				name: "NUMBER with second argument not array",
				code: `
			MODULE Test;
			VAR x: BYTE;
			    n: INT32;
			BEGIN NUMBER(n, x)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects the second argument to be an array of byte or char",
			},
			{
				name: "NUMBER with no arguments",
				code: `
			MODULE Test;
			BEGIN NUMBER()
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects exactly two arguments",
			},
			{
				name: "NUMBER with too many arguments",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    n: INT32;
			BEGIN NUMBER(n, a, 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects exactly two arguments",
			},
			{
				name: "NUMBER with non-assignable first argument",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    n: INT32;
			BEGIN NUMBER(1, a)
			END Test.`,
				shouldPass:  false,
				expectError: "'NUMBER' expects the first argument to be an assignable numeric or set type",
			},
			{
				name: "NUMBER with real first argument",
				code: `
			MODULE Test;
			VAR a: ARRAY 4 OF BYTE;
			    n: REAL;
			BEGIN NUMBER(n, a)
			END Test.`,
				shouldPass: true,
			},
		},
	},
	{
		procName: "COPY",
		cases: []procTestCase{
			{
				name: "COPY with string and array",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			BEGIN COPY("Hello", a)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "COPY with array and array",
				code: `
			MODULE Test;
			VAR src, dst: ARRAY 16 OF CHAR;
			BEGIN COPY(src, dst)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "COPY with non-string first argument",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			    x: INTEGER;
			BEGIN COPY(x, a)
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects the first argument to be a char array or string",
			},
			{
				name: "COPY with wrong destination type",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			    x: SET;
			BEGIN COPY(a, x)
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects the second argument to be an assignable array of char",
			},
			{
				name: "COPY to non-array of char",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			BEGIN COPY(a, a[1])  (* not array of char *)
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects the second argument to be an assignable array of char",
			},
			{
				name: "COPY wrong arity",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			BEGIN COPY(a)
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects exactly two arguments",
			},
			{
				name: "COPY with too many arguments",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			BEGIN COPY(a, a, 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects exactly two arguments",
			},
			{
				name: "COPY with no arguments",
				code: `
			MODULE Test;
			BEGIN COPY()
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects exactly two arguments",
			},
			{
				name: "COPY with non-assignable second argument",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			BEGIN COPY(a, "Hello")
			END Test.`,
				shouldPass:  false,
				expectError: "'COPY' expects the second argument to be an assignable array of char",
			},
			{
				name: "COPY with array of CHAR",
				code: `
			MODULE Test;
			VAR a: ARRAY 16 OF CHAR;
			VAR b: ARRAY 16 OF CHAR;
			BEGIN COPY(a, b)
			END Test.`,
				shouldPass: true,
			},
		},
	},
	{
		procName: "PACK",
		cases: []procTestCase{
			{
				name: "PACK with REAL and INT32",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN PACK(x, 5i)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "PACK with LONGREAL and INT32",
				code: `
			MODULE Test;
			VAR x: LONGREAL;
			BEGIN PACK(x, 0i)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "PACK with wrong x type",
				code: `
			MODULE Test;
			VAR x: SET;
			BEGIN PACK(x, 0)
			END Test.`,
				expectError: "'PACK' expects the first argument to be an assignable REAL/LONGREAL",
			},
			{
				name: "PACK with wrong n type",
				code: `
			MODULE Test;
			VAR x: REAL; b: BYTE;
			BEGIN PACK(x, b)
			END Test.`,
				shouldPass:  false,
				expectError: "'PACK' expects the second argument to be INT32",
			},
			{
				name: "PACK first argument not l-value",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN PACK(x + 1.0, 0i)
			END Test.`,
				shouldPass:  false,
				expectError: "'PACK' expects the first argument to be an assignable REAL/LONGREAL",
			},
			{
				name: "PACK with one argument",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN PACK(x)
			END Test.`,
				shouldPass:  false,
				expectError: "'PACK' expects exactly two arguments",
			},
		},
	},
	{
		procName: "UNPK",
		cases: []procTestCase{
			{
				name: "UNPK with REAL and INT32",
				code: `
			MODULE Test;
			VAR x: REAL; n: INT32;
			BEGIN UNPK(x, n)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "UNPK with LONGREAL and INT32",
				code: `
			MODULE Test;
			VAR x: LONGREAL; n: INT32;
			BEGIN UNPK(x, n)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "UNPK with invalid x type",
				code: `
			MODULE Test;
			VAR x: SET; n: INT32;
			BEGIN UNPK(x, n)
			END Test.`,
				shouldPass:  false,
				expectError: "'UNPK' expects the first argument to be an assignable REAL/LONGREAL",
			},
			{
				name: "UNPK with invalid n type",
				code: `
			MODULE Test;
			VAR x: REAL; n: BYTE;
			BEGIN UNPK(x, n)
			END Test.`,
				expectError: "'UNPK' expects the second argument to be an assignable INT32",
			},
			{
				name: "UNPK x not l-value",
				code: `
			MODULE Test;
			VAR x: REAL; n: INT32;
			BEGIN UNPK(x + 1.0, n)
			END Test.`,
				shouldPass:  false,
				expectError: "'UNPK' expects the first argument to be an assignable REAL/LONGREAL",
			},
			{
				name: "UNPK n not l-value",
				code: `
			MODULE Test;
			VAR x: REAL; n: INT32;
			BEGIN UNPK(x, n + 1)
			END Test.`,
				shouldPass:  false,
				expectError: "'UNPK' expects the second argument to be an assignable INT32",
			},
			{
				name: "UNPK arity mismatch",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN UNPK(x)
			END Test.`,
				expectError: "'UNPK' expects exactly two arguments",
			},
			{
				name: "UNPK arity mismatch",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN UNPK(x, 0i, 5)
			END Test.`,
				expectError: "'UNPK' expects exactly two arguments",
			},
			{
				name: "UNPK no arguments",
				code: `
			MODULE Test;
			VAR x: REAL;
			BEGIN UNPK()
			END Test.`,
				expectError: "'UNPK' expects exactly two arguments",
			},
		},
	},
	{
		procName: "RAISE",
		cases: []procTestCase{
			{
				name: "RAISE pointer to record",
				code: `
			MODULE Test;
			VAR e: POINTER TO ANYREC;
			BEGIN RAISE(e)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "RAISE with non-pointer",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN RAISE(x)
			END Test.`,
				shouldPass:  false,
				expectError: "'RAISE' expects a pointer to ANYREC",
			},
			{
				name: "RAISE pointer to array",
				code: `
			MODULE Test;
			TYPE A = ARRAY 4 OF CHAR;
			VAR p: POINTER TO A;
			BEGIN RAISE(p)
			END Test.`,
				shouldPass:  false,
				expectError: "'RAISE' expects a pointer to ANYREC",
			},
			{
				name: "RAISE too many args",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN RAISE(x, x)
			END Test.`,
				expectError: "'RAISE' expects exactly one argument",
			},
			{
				name: "RAISE too few args",
				code: `
			MODULE Test;
			VAR x: INTEGER;
			BEGIN RAISE()
			END Test.`,
				expectError: "'RAISE' expects exactly one argument",
			},
		},
	},
	{
		procName: "PCALL",
		cases: []procTestCase{
			{
				name: "Valid PCALL",
				code: `
			MODULE Test;
			//TYPE P = PROCEDURE(x: INTEGER);
	
			VAR e: POINTER TO anyrec;
			PROCEDURE p(x: INTEGER);  END p;
	
			BEGIN PCALL(e, p, 42)
			END Test.`,
				shouldPass: true,
			},
			{
				name: "PCALL non-pointer exception",
				code: `
			MODULE Test;
			TYPE P = PROCEDURE;
			VAR x: INTEGER;
			PROCEDURE Act; END Act;
			BEGIN PCALL(x, Act)
			END Test.`,
				shouldPass:  false,
				expectError: "'PCALL' expects the first argument to be a pointer to ANYREC",
			},
			{
				name: "PCALL pointer to non-record",
				code: `
			MODULE Test;
			TYPE A = ARRAY 4 OF INTEGER;
			     P = PROCEDURE;
			VAR p: POINTER TO A;
			PROCEDURE Act; END Act;
			BEGIN PCALL(p, Act)
			END Test.`,
				shouldPass:  false,
				expectError: "'PCALL' expects the first argument to be a pointer to ANYREC",
			},
			{
				name: "PCALL second argument not procedure",
				code: `
			MODULE Test;
			VAR e: POINTER TO ANYREC;
			    x: INTEGER;
			BEGIN PCALL(e, x)
			END Test.`,
				shouldPass:  false,
				expectError: "'PCALL' expects the second argument to be a proper procedure",
			},
			{
				name: "PCALL argument mismatch",
				code: `
			MODULE Test;
			VAR e: POINTER TO ANYREC;
			PROCEDURE F(x: INTEGER); END F;
			BEGIN PCALL(e, F, TRUE)
			END Test.`,
				shouldPass:  false,
				expectError: "PCALL: argument 1 is incompatible with parameter of type 'integer'",
			},
		},
	},
}

func TestPredeclaredProcedureCalls(t *testing.T) {
	for _, entry := range predeclaredProcTests {
		for _, tc := range entry.cases {
			t.Run(entry.procName+"_"+tc.name, func(t *testing.T) {
				ctx := typeCheckSnippet(t, tc.code)
				if tc.shouldPass {
					if ctx.Reporter.ErrorCount() > 0 {
						t.Errorf("expected success, got %d errors", ctx.Reporter.ErrorCount())
						ctx.Reporter.Flush()
					}
				} else {
					if ctx.Reporter.ErrorCount() == 0 {
						t.Errorf("expected error but got success")
					} else {
						err := ctx.Reporter.Diagnostics()[0]
						if !strings.Contains(err.Message, tc.expectError) {
							t.Errorf("expected error to contain %q, got %q", tc.expectError, err.Message)
						}
					}
				}
			})
		}
	}
}

func typeCheckSnippet(t *testing.T, code string) *report.Context {
	tmp := t.TempDir()
	file := filepath.Join(tmp, "test.obx")
	if err := os.WriteFile(file, []byte(code), 0644); err != nil {
		panic(err)
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
		Content:  []byte(code),
		Source:   srcMgr,
		Reporter: reporter,
		TabWidth: 4,
		Envs:     make(map[string]*ast.Environment),
	}
	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		return ctx
	}

	obx.AddUnit(unit)
	ctx.Envs[unit.Name()] = unit.Environ()
	s := NewSema(ctx, obx)
	s.Validate()

	return ctx
}
