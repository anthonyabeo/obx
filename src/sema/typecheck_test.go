package sema

import (
	"log"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/anthonyabeo/obx/adt"
	"github.com/anthonyabeo/obx/modgraph"
	"github.com/anthonyabeo/obx/src/report"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/parser"
	"github.com/anthonyabeo/obx/src/types"
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
					i64 := -2147483648
					i32 := -32768
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
			y := MIN(int32)
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
				FileName:  tt.filename,
				FilePath:  tt.filename,
				Content:   []byte(tt.source),
				Source:    srcMgr,
				Reporter:  reporter,
				TabWidth:  4,
				Env:       ast.NewEnv(),
				Names:     adt.NewStack[string](),
				ExprLists: adt.NewStack[[]ast.Expression](),
			}

			p := parser.NewParser(ctx)
			unit := p.Parse()
			if ctx.Reporter.ErrorCount() > 0 {
				ctx.Reporter.Flush()
				log.Fatalf("%d parse errors found", ctx.Reporter.ErrorCount())
			}

			obx.AddUnit(unit)

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
				FileName:  tt.filename,
				FilePath:  tt.filename,
				Content:   []byte(tt.source),
				Source:    srcMgr,
				Reporter:  reporter,
				TabWidth:  4,
				Env:       ast.NewEnv(),
				Names:     adt.NewStack[string](),
				ExprLists: adt.NewStack[[]ast.Expression](),
			}

			p := parser.NewParser(ctx)
			unit := p.Parse()
			if ctx.Reporter.ErrorCount() > 0 {
				ctx.Reporter.Flush()
				log.Fatalf("%d parse errors found", ctx.Reporter.ErrorCount())
			}

			obx.AddUnit(unit)

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
		FileName:  file,
		FilePath:  file,
		Content:   content,
		Source:    srcMgr,
		Reporter:  reporter,
		TabWidth:  4,
		Env:       ast.NewEnv(),
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		log.Fatalf("%d parse errors found", ctx.Reporter.ErrorCount())
	}

	obx.AddUnit(unit)

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
		FileName:  file,
		FilePath:  file,
		Content:   content,
		Source:    srcMgr,
		Reporter:  reporter,
		TabWidth:  4,
		Env:       ast.NewEnv(),
		Names:     adt.NewStack[string](),
		ExprLists: adt.NewStack[[]ast.Expression](),
	}

	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		ctx.Reporter.Flush()
		log.Fatalf("%d parse errors found", ctx.Reporter.ErrorCount())
	}

	obx.AddUnit(unit)

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
		procName: "CAP",
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
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name: "CAP with boolean argument",
				code: `MODULE Test;
						VAR c: CHAR;
						BEGIN c := CAP(TRUE)
						END Test.`,
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
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
			{
				name:       "Valid_CAP_on_char",
				code:       "MODULE Test; VAR i, c: CHAR; ch: CHAR; BEGIN i := CAP(c) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_CAP_on_integer",
				code:        "MODULE Test; VAR i: INTEGER; BEGIN i := CAP(i) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name:        "Invalid_CAP_wrong_arg_count",
				code:        "MODULE Test; VAR c: CHAR; BEGIN c := CAP() END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects exactly one argument",
			},
			{
				name:        "Invalid_CAP_too_many_args",
				code:        "MODULE Test; VAR c: CHAR; BEGIN c := CAP(c, c) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects exactly one argument",
			},
			{
				name:       "Valid_CAP_on_char_literal",
				code:       "MODULE Test; VAR i, c: WCHAR; BEGIN i := CAP('a') END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_CAP_on_boolean",
				code:        "MODULE Test; VAR i, b: BOOL; BEGIN i := CAP(b) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name:        "Invalid_CAP_on_array_of_char",
				code:        "MODULE Test; VAR i, arr: ARRAY 3 OF CHAR; BEGIN i := CAP(arr) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name:        "Invalid_CAP_on_set",
				code:        "MODULE Test; VAR i, s: SET; BEGIN i := CAP(s) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name:        "Invalid_CAP_on_real",
				code:        "MODULE Test; VAR i, r: REAL; BEGIN i := CAP(r) END Test.",
				shouldPass:  false,
				expectError: "'CAP' expects a CHAR argument",
			},
			{
				name:       "Invalid_CAP_on_string_literal",
				code:       "MODULE Test; VAR i: WCHAR; BEGIN i := CAP('A') END Test.",
				shouldPass: true,
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
	{
		procName: "CHR",
		cases: []procTestCase{
			{
				name:       "Valid_CHR_on_integer",
				code:       "MODULE Test; VAR c: CHAR; BEGIN c := CHR(65) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_variable",
				code:       "MODULE Test; VAR c: CHAR; i: INTEGER; BEGIN i := 99; c := CHR(i) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_CHR_on_real",
				code:        "MODULE Test; VAR c: CHAR; r: REAL; BEGIN c := CHR(r) END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects an integer argument",
			},
			{
				name:        "Invalid_CHR_on_char",
				code:        "MODULE Test; VAR c1, c2: CHAR; BEGIN c1 := CHR(c2) END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects an integer argument",
			},
			{
				name:        "Invalid_CHR_on_boolean",
				code:        "MODULE Test; VAR c: CHAR; b: BOOL; BEGIN c := CHR(b) END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects an integer argument",
			},
			{
				name:        "Invalid_CHR_on_set",
				code:        "MODULE Test; VAR c: CHAR; s: SET; BEGIN c := CHR(s) END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects an integer argument",
			},
			{
				name:        "Invalid_CHR_too_few_args",
				code:        "MODULE Test; VAR c: CHAR; BEGIN c := CHR() END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects exactly one argument",
			},
			{
				name:        "Invalid_CHR_too_many_args",
				code:        "MODULE Test; VAR c: CHAR; BEGIN c := CHR(1, 2) END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects exactly one argument",
			},
			{
				name:       "Invalid_CHR_out_of_range",
				code:       "MODULE Test; VAR c: CHAR; BEGIN c := CHR(300) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_byte",
				code:       "MODULE Test; VAR c: CHAR; b: BYTE; BEGIN b := 255; c := CHR(b) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_int8",
				code:       "MODULE Test; VAR c: CHAR; i: INT8; BEGIN c := CHR(i) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_int16",
				code:       "MODULE Test; VAR c: CHAR; i: INT16; BEGIN c := CHR(i) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_int32",
				code:       "MODULE Test; VAR c: CHAR; i: INT32; BEGIN i := 34; c := CHR(i) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CHR_on_int64",
				code:       "MODULE Test; VAR c: CHAR; i: INT64; BEGIN i := 254; c := CHR(i) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_CHR_on_string_literal",
				code:        "MODULE Test; VAR c: CHAR; BEGIN c := CHR(\"A\") END Test.",
				shouldPass:  false,
				expectError: "'CHR' expects an integer argument",
			},
		},
	},
	{
		procName: "BITAND",
		cases: []procTestCase{
			{
				name:       "Valid_BITAND_INT32_INT32",
				code:       "MODULE Test; VAR x, y: INT32; z: INT32; BEGIN z := BITAND(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITAND_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; y: INT32; z: INT64; BEGIN z := BITAND(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITAND_INT32_INT64",
				code:       "MODULE Test; VAR x: INT32; y: INT64; z: INT64; BEGIN z := BITAND(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITAND_INT64_INT64",
				code:       "MODULE Test; VAR x, y: INT64; z: INT64; BEGIN z := BITAND(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITAND_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; y: REAL; z: INT32; BEGIN z := BITAND(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITAND_CHAR_INT32",
				code:        "MODULE Test; VAR x: CHAR; y: INT32; z: INT32; BEGIN z := BITAND(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITAND_too_few_args",
				code:        "MODULE Test; VAR x: INT32; z: INT32; BEGIN z := BITAND(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITAND_too_many_args",
				code:        "MODULE Test; VAR x, y, z: INT32; BEGIN z := BITAND(x, y, z) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITAND_on_boolean",
				code:        "MODULE Test; VAR x: BOOL; y: INT32; z: INT32; BEGIN z := BITAND(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITAND_on_set",
				code:        "MODULE Test; VAR x: SET; y: INT32; z: INT32; BEGIN z := BITAND(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITAND' expects INT32 or INT64 arguments",
			},
		},
	},
	{
		procName: "BITASR",
		cases: []procTestCase{
			{
				name:       "Valid_BITASR_INT32_INT32",
				code:       "MODULE Test; VAR x, n: INT32; z: INT32; BEGIN z := BITASR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITASR_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; n: INT32; z: INT64; BEGIN z := BITASR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITASR_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; z: INT32; BEGIN z := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITASR_REAL_INT32",
				code:        "MODULE Test; VAR x: REAL; n: INT32; z: INT32; BEGIN z := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITASR_INT64_INT64",
				code:        "MODULE Test; VAR x, n: INT64; z: INT64; BEGIN z := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITASR_too_few_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITASR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITASR_too_many_args",
				code:        "MODULE Test; VAR x, n: INT32; BEGIN x := BITASR(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITASR_on_boolean",
				code:        "MODULE Test; VAR x: BOOL; n: INT32; BEGIN x := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITASR_on_set",
				code:        "MODULE Test; VAR x: SET; n: INT32; BEGIN x := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITASR_second_arg_char",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; BEGIN x := BITASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITASR' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "BITNOT",
		cases: []procTestCase{
			{
				name:       "Valid_BITNOT_INT32",
				code:       "MODULE Test; VAR x, z: INT32; BEGIN z := BITNOT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITNOT_INT64",
				code:       "MODULE Test; VAR x, z: INT64; BEGIN z := BITNOT(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITNOT_REAL",
				code:        "MODULE Test; VAR x: REAL; BEGIN x := BITNOT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_BITNOT_CHAR",
				code:        "MODULE Test; VAR x: CHAR; BEGIN x := BITNOT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_BITNOT_BOOLEAN",
				code:        "MODULE Test; VAR x: BOOL; BEGIN x := BITNOT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_BITNOT_SET",
				code:        "MODULE Test; VAR x: SET; BEGIN x := BITNOT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_BITNOT_too_few_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITNOT() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects exactly one argument",
			},
			{
				name:        "Invalid_BITNOT_too_many_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITNOT(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITNOT' expects exactly one argument",
			},
		},
	},
	{
		procName: "BITOR",
		cases: []procTestCase{
			{
				name:       "Valid_BITOR_INT32_INT32",
				code:       "MODULE Test; VAR x, y: INT32; z: INT32; BEGIN z := BITOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITOR_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; y: INT32; z: INT64; BEGIN z := BITOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITOR_INT32_INT64",
				code:       "MODULE Test; VAR x: INT32; y: INT64; z: INT64; BEGIN z := BITOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITOR_INT64_INT64",
				code:       "MODULE Test; VAR x, y: INT64; z: INT64; BEGIN z := BITOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITOR_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; y: REAL; z: INT32; BEGIN z := BITOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITOR_CHAR_INT32",
				code:        "MODULE Test; VAR x: CHAR; y: INT32; z: INT32; BEGIN z := BITOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITOR_too_few_args",
				code:        "MODULE Test; VAR x: INT32; z: INT32; BEGIN z := BITOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITOR_too_many_args",
				code:        "MODULE Test; VAR x, y, z: INT32; BEGIN z := BITOR(x, y, z) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITOR_on_bool",
				code:        "MODULE Test; VAR x: BOOL; y: INT32; z: INT32; BEGIN z := BITOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITOR_on_set",
				code:        "MODULE Test; VAR x: SET; y: INT32; z: INT32; BEGIN z := BITOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITOR' expects INT32 or INT64 arguments",
			},
		},
	},
	{
		procName: "BITS",
		cases: []procTestCase{
			{
				name:       "Valid_BITS_INT32",
				code:       "MODULE Test; VAR x: INT32; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITS_INT64",
				code:        "MODULE Test; VAR x: INT64; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects an INT32 argument",
			},
			{
				name:        "Invalid_BITS_REAL",
				code:        "MODULE Test; VAR x: REAL; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects an INT32 argument",
			},
			{
				name:        "Invalid_BITS_CHAR",
				code:        "MODULE Test; VAR x: CHAR; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects an INT32 argument",
			},
			{
				name:        "Invalid_BITS_BOOL",
				code:        "MODULE Test; VAR x: BOOL; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects an INT32 argument",
			},
			{
				name:        "Invalid_BITS_SET",
				code:        "MODULE Test; VAR x: SET; s: SET; BEGIN s := BITS(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects an INT32 argument",
			},
			{
				name:        "Invalid_BITS_too_few_args",
				code:        "MODULE Test; VAR s: SET; BEGIN s := BITS() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects exactly one argument",
			},
			{
				name:        "Invalid_BITS_too_many_args",
				code:        "MODULE Test; VAR x: INT32; s: SET; BEGIN s := BITS(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITS' expects exactly one argument",
			},
		},
	},
	{
		procName: "BITSHL",
		cases: []procTestCase{
			{
				name:       "Valid_BITSHL_INT32_INT32",
				code:       "MODULE Test; VAR x, n: INT32; z: INT32; BEGIN z := BITSHL(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITSHL_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; n: INT32; z: INT64; BEGIN z := BITSHL(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITSHL_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; z: INT32; BEGIN z := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITSHL_REAL_INT32",
				code:        "MODULE Test; VAR x: REAL; n: INT32; z: INT32; BEGIN z := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHL_INT64_INT64",
				code:        "MODULE Test; VAR x, n: INT64; z: INT64; BEGIN z := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITSHL_too_few_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITSHL(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITSHL_too_many_args",
				code:        "MODULE Test; VAR x, n: INT32; BEGIN x := BITSHL(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITSHL_on_bool",
				code:        "MODULE Test; VAR x: BOOL; n: INT32; BEGIN x := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHL_on_set",
				code:        "MODULE Test; VAR x: SET; n: INT32; BEGIN x := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHL_second_arg_char",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; BEGIN x := BITSHL(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHL' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "BITSHR",
		cases: []procTestCase{
			{
				name:       "Valid_BITSHR_INT32_INT32",
				code:       "MODULE Test; VAR x, n: INT32; z: INT32; BEGIN z := BITSHR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITSHR_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; n: INT32; z: INT64; BEGIN z := BITSHR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITSHR_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; z: INT32; BEGIN z := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITSHR_REAL_INT32",
				code:        "MODULE Test; VAR x: REAL; n: INT32; z: INT32; BEGIN z := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHR_INT64_INT64",
				code:        "MODULE Test; VAR x, n: INT64; z: INT64; BEGIN z := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_BITSHR_too_few_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITSHR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITSHR_too_many_args",
				code:        "MODULE Test; VAR x, n: INT32; BEGIN x := BITSHR(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITSHR_on_bool",
				code:        "MODULE Test; VAR x: BOOL; n: INT32; BEGIN x := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHR_on_set",
				code:        "MODULE Test; VAR x: SET; n: INT32; BEGIN x := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_BITSHR_second_arg_char",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; BEGIN x := BITSHR(x, n) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITSHR' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "BITXOR",
		cases: []procTestCase{
			{
				name:       "Valid_BITXOR_INT32_INT32",
				code:       "MODULE Test; VAR x, y: INT32; z: INT32; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITXOR_INT64_INT32",
				code:       "MODULE Test; VAR x: INT64; y: INT32; z: INT64; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITXOR_INT32_INT64",
				code:       "MODULE Test; VAR x: INT32; y: INT64; z: INT64; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_BITXOR_INT64_INT64",
				code:       "MODULE Test; VAR x, y: INT64; z: INT64; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_BITXOR_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; y: REAL; z: INT32; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITXOR_REAL_INT32",
				code:        "MODULE Test; VAR x: REAL; y: INT32; z: INT32; BEGIN z := BITXOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITXOR_on_bool",
				code:        "MODULE Test; VAR x: BOOL; y: INT32; BEGIN x := BITXOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITXOR_on_set",
				code:        "MODULE Test; VAR x: SET; y: INT32; BEGIN x := BITXOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects INT32 or INT64 arguments",
			},
			{
				name:        "Invalid_BITXOR_too_few_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := BITXOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITXOR_too_many_args",
				code:        "MODULE Test; VAR x, y: INT32; BEGIN x := BITXOR(x, y, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects exactly two arguments",
			},
			{
				name:        "Invalid_BITXOR_second_arg_char",
				code:        "MODULE Test; VAR x: INT32; y: CHAR; BEGIN x := BITXOR(x, y) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'BITXOR' expects INT32 or INT64 arguments",
			},
		},
	},
	{
		procName: "FLOOR",
		cases: []procTestCase{
			{
				name:       "Valid_FLOOR_REAL",
				code:       "MODULE Test; VAR x: REAL; z: INT32; BEGIN z := FLOOR(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_FLOOR_LONGREAL",
				code:       "MODULE Test; VAR x: LONGREAL; z: INT64; BEGIN z := FLOOR(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_FLOOR_INT32",
				code:        "MODULE Test; VAR x: INT32; z: INT32; BEGIN z := FLOOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects a REAL or LONGREAL argument",
			},
			{
				name:        "Invalid_FLOOR_SET",
				code:        "MODULE Test; VAR x: SET; z: INT32; BEGIN z := FLOOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects a REAL or LONGREAL argument",
			},
			{
				name:        "Invalid_FLOOR_CHAR",
				code:        "MODULE Test; VAR x: CHAR; z: INT32; BEGIN z := FLOOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects a REAL or LONGREAL argument",
			},
			{
				name:        "Invalid_FLOOR_BOOL",
				code:        "MODULE Test; VAR x: BOOL; z: INT32; BEGIN z := FLOOR(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects a REAL or LONGREAL argument",
			},
			{
				name:        "Invalid_FLOOR_too_few_args",
				code:        "MODULE Test; VAR z: INT32; BEGIN z := FLOOR() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects exactly one argument",
			},
			{
				name:        "Invalid_FLOOR_too_many_args",
				code:        "MODULE Test; VAR x: REAL; z: INT32; BEGIN z := FLOOR(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLOOR' expects exactly one argument",
			},
		},
	},
	{
		procName: "FLT",
		cases: []procTestCase{
			{
				name:       "Valid_FLT_INT32",
				code:       "MODULE Test; VAR x: INT32; r: REAL; BEGIN r := FLT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_FLT_INT64",
				code:       "MODULE Test; VAR x: INT64; r: LONGREAL; BEGIN r := FLT(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_FLT_REAL",
				code:        "MODULE Test; VAR x: REAL; r: REAL; BEGIN r := FLT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_FLT_SET",
				code:        "MODULE Test; VAR x: SET; r: REAL; BEGIN r := FLT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_FLT_CHAR",
				code:        "MODULE Test; VAR x: CHAR; r: REAL; BEGIN r := FLT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_FLT_BOOL",
				code:        "MODULE Test; VAR x: BOOL; r: REAL; BEGIN r := FLT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects an INT32 or INT64 argument",
			},
			{
				name:        "Invalid_FLT_too_few_args",
				code:        "MODULE Test; VAR r: REAL; BEGIN r := FLT() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects exactly one argument",
			},
			{
				name:        "Invalid_FLT_too_many_args",
				code:        "MODULE Test; VAR x: INT32; r: REAL; BEGIN r := FLT(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'FLT' expects exactly one argument",
			},
		},
	},
	{
		procName: "LEN",
		cases: []procTestCase{
			{
				name:       "Valid_LEN_array",
				code:       "MODULE Test; VAR a: ARRAY 10 OF INTEGER; n: INT32; BEGIN n := LEN(a) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LEN_string",
				code:       "MODULE Test; VAR s: ARRAY 16 OF CHAR; n: INT32; BEGIN n := LEN(s) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LEN_array_dim",
				code:       "MODULE Test; VAR a: ARRAY 5, 7 OF INTEGER; n: INT32; BEGIN n := LEN(a, 1i) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LEN_string_dim0",
				code:       "MODULE Test; VAR s: ARRAY 8 OF CHAR; n: INT32; BEGIN n := LEN(s, 0i) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_LEN_on_int",
				code:        "MODULE Test; VAR x: INT32; n: INT32; BEGIN n := LEN(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects an array or string argument",
			},
			{
				name:        "Invalid_LEN_on_set",
				code:        "MODULE Test; VAR s: SET; n: INT32; BEGIN n := LEN(s) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects an array or string argument",
			},
			{
				name:        "Invalid_LEN_too_few_args",
				code:        "MODULE Test; VAR a: ARRAY 3 OF INTEGER; n: INT32; BEGIN n := LEN() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects one or two arguments",
			},
			{
				name:        "Invalid_LEN_too_many_args",
				code:        "MODULE Test; VAR a: ARRAY 3 OF INTEGER; n: INT32; BEGIN n := LEN(a, 0, 1) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects one or two arguments",
			},
			{
				name:        "Invalid_LEN_dim_not_int",
				code:        "MODULE Test; VAR a: ARRAY 3 OF INTEGER; n: INT32; BEGIN n := LEN(a, TRUE) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_LEN_dim_on_scalar",
				code:        "MODULE Test; VAR x: INT32; n: INT32; BEGIN n := LEN(x, 0) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LEN' expects the first argument to be an array",
			},
			{
				name:        "Invalid_LEN_dim_too_large",
				code:        "MODULE Test; VAR a: ARRAY 3, 4 OF INTEGER; n: INT32; BEGIN n := LEN(a, 2i) END Test.",
				shouldPass:  false,
				expectError: "ARRAY has fewer than 3 dimensions",
			},
			{
				name:        "Invalid_LEN_dim_on_string_too_large",
				code:        "MODULE Test; VAR s: ARRAY 8 OF CHAR; n: INT32; BEGIN n := LEN(s, 1i) END Test.",
				shouldPass:  false,
				expectError: "ARRAY has fewer than 2 dimensions",
			},
			{
				name:        "Invalid_LEN_dim_negative",
				code:        "MODULE Test; VAR a: ARRAY 3, 4 OF INTEGER; n: INT32; BEGIN n := LEN(a, -1i) END Test.",
				shouldPass:  false,
				expectError: "dimension value -1 should be integer constant >= 0",
			},
		},
	},
	{
		procName: "LONG",
		cases: []procTestCase{
			{
				name:       "Valid_LONG_INT8",
				code:       "MODULE Test; VAR x: INT8; y: INT16; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LONG_BYTE",
				code:       "MODULE Test; VAR x: BYTE; y: INT16; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LONG_INT16",
				code:       "MODULE Test; VAR x: INT16; y: INT32; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LONG_INT32",
				code:       "MODULE Test; VAR x: INT32; y: INT64; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LONG_REAL",
				code:       "MODULE Test; VAR x: REAL; y: LONGREAL; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LONG_CHAR",
				code:       "MODULE Test; VAR x: CHAR; y: WCHAR; BEGIN y := LONG(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_LONG_INT64",
				code:        "MODULE Test; VAR x: INT64; y: INT64; BEGIN y := LONG(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LONG' expects INT8, BYTE, INT16, INT32, REAL, or CHAR argument",
			},
			{
				name:        "Invalid_LONG_WCHAR",
				code:        "MODULE Test; VAR x: WCHAR; y: WCHAR; BEGIN y := LONG(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LONG' expects INT8, BYTE, INT16, INT32, REAL, or CHAR argument",
			},
			{
				name:        "Invalid_LONG_too_many_args",
				code:        "MODULE Test; VAR x: INT8; y: INT16; BEGIN y := LONG(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LONG' expects exactly one argument",
			},
			{
				name:        "Invalid_LONG_no_args",
				code:        "MODULE Test; VAR y: INT16; BEGIN y := LONG() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'LONG' expects exactly one argument",
			},
		},
	},
	{
		procName: "SHORT",
		cases: []procTestCase{
			{
				name:       "Valid_SHORT_INT64",
				code:       "MODULE Test; VAR x: INT64; y: INT32; BEGIN y := SHORT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SHORT_INT32",
				code:       "MODULE Test; VAR x: INT32; y: INT16; BEGIN y := SHORT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SHORT_INT16",
				code:       "MODULE Test; VAR x: INT16; y: INT8; BEGIN y := SHORT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SHORT_LONGREAL",
				code:       "MODULE Test; VAR x: LONGREAL; y: REAL; BEGIN y := SHORT(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SHORT_WCHAR",
				code:       "MODULE Test; VAR x: WCHAR; y: CHAR; BEGIN y := SHORT(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_SHORT_INT8",
				code:        "MODULE Test; VAR x: INT8; y: INT8; BEGIN y := SHORT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'SHORT' expects an INT16, INT32, INT64, LONGREAL, or WCHAR argument",
			},
			{
				name:        "Invalid_SHORT_REAL",
				code:        "MODULE Test; VAR x: REAL; y: REAL; BEGIN y := SHORT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'SHORT' expects an INT16, INT32, INT64, LONGREAL, or WCHAR argument",
			},
			{
				name:        "Invalid_SHORT_CHAR",
				code:        "MODULE Test; VAR x: CHAR; y: CHAR; BEGIN y := SHORT(x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'SHORT' expects an INT16, INT32, INT64, LONGREAL, or WCHAR argument",
			},
			{
				name:        "Invalid_SHORT_too_many_args",
				code:        "MODULE Test; VAR x: INT64; y: INT32; BEGIN y := SHORT(x, x) END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'SHORT' expects exactly one argument",
			},
			{
				name:        "Invalid_SHORT_no_args",
				code:        "MODULE Test; VAR y: INT32; BEGIN y := SHORT() END Test.",
				shouldPass:  false,
				expectError: "predeclared procedure 'SHORT' expects exactly one argument",
			},
		},
	},
	{
		procName: "ORD",
		cases: []procTestCase{
			{
				name:       "Valid_ORD_CHAR",
				code:       "MODULE Test; VAR c: CHAR; n: BYTE; BEGIN n := ORD(c) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ORD_WCHAR",
				code:       "MODULE Test; VAR w: WCHAR; n: SHORTINT; BEGIN n := ORD(w) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ORD_ENUM",
				code:       "MODULE Test; TYPE Color = (Red, Green, Blue); VAR c: Color; n: INT32; BEGIN n := ORD(c) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ORD_BOOLEAN",
				code:       "MODULE Test; VAR b: BOOL; n: BYTE; BEGIN n := ORD(b) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ORD_SET",
				code:       "MODULE Test; VAR s: SET; n: INT32; BEGIN n := ORD(s) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ORD_INT32",
				code:        "MODULE Test; VAR i: INT32; n: INT32; BEGIN n := ORD(i) END Test.",
				shouldPass:  false,
				expectError: "'ORD' cannot be applied to type",
			},
			{
				name:        "Invalid_ORD_REAL",
				code:        "MODULE Test; VAR r: REAL; n: INT32; BEGIN n := ORD(r) END Test.",
				shouldPass:  false,
				expectError: "'ORD' cannot be applied to type",
			},
			{
				name:        "Invalid_ORD_too_many_args",
				code:        "MODULE Test; VAR c: CHAR; n: BYTE; BEGIN n := ORD(c, c) END Test.",
				shouldPass:  false,
				expectError: "'ORD' expects exactly one argument",
			},
			{
				name:        "Invalid_ORD_no_args",
				code:        "MODULE Test; VAR n: BYTE; BEGIN n := ORD() END Test.",
				shouldPass:  false,
				expectError: "'ORD' expects exactly one argument",
			},
		},
	},
	{
		procName: "ODD",
		cases: []procTestCase{
			{
				name:       "Valid_ODD_INT8",
				code:       "MODULE Test; VAR x: INT8; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ODD_INT16",
				code:       "MODULE Test; VAR x: INT16; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ODD_INT32",
				code:       "MODULE Test; VAR x: INT32; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ODD_INT64",
				code:       "MODULE Test; VAR x: INT64; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ODD_REAL",
				code:        "MODULE Test; VAR x: REAL; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass:  false,
				expectError: "'ODD' expects an integer argument",
			},
			{
				name:        "Invalid_ODD_BOOLEAN",
				code:        "MODULE Test; VAR x: BOOL; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass:  false,
				expectError: "'ODD' expects an integer argument",
			},
			{
				name:        "Invalid_ODD_CHAR",
				code:        "MODULE Test; VAR x: CHAR; b: BOOL; BEGIN b := ODD(x) END Test.",
				shouldPass:  false,
				expectError: "'ODD' expects an integer argument",
			},
			{
				name:        "Invalid_ODD_no_args",
				code:        "MODULE Test; VAR b: BOOL; BEGIN b := ODD() END Test.",
				shouldPass:  false,
				expectError: "'ODD' expects exactly one argument",
			},
			{
				name:        "Invalid_ODD_too_many_args",
				code:        "MODULE Test; VAR x: INT32; b: BOOL; BEGIN b := ODD(x, x) END Test.",
				shouldPass:  false,
				expectError: "'ODD' expects exactly one argument",
			},
		},
	},
	{
		procName: "ASH",
		cases: []procTestCase{
			{
				name:       "Valid_ASH_INT32",
				code:       "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ASH(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ASH_INT64",
				code:       "MODULE Test; VAR x: INT64; n: INT32; y: INT64; BEGIN y := ASH(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ASH_x_REAL",
				code:        "MODULE Test; VAR x: REAL; n: INT32; y: INT32; BEGIN y := ASH(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_ASH_n_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; y: INT32; BEGIN y := ASH(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_ASH_no_args",
				code:        "MODULE Test; VAR y: INT32; BEGIN y := ASH() END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects exactly two arguments",
			},
			{
				name:        "Invalid_ASH_too_many_args",
				code:        "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ASH(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects exactly two arguments",
			},
			{
				name:        "Invalid_ASH_x_CHAR",
				code:        "MODULE Test; VAR x: CHAR; n: INT32; y: INT32; BEGIN y := ASH(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_ASH_n_CHAR",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; y: INT32; BEGIN y := ASH(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASH' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "ASR",
		cases: []procTestCase{
			{
				name:       "Valid_ASR_INT32",
				code:       "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ASR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ASR_INT64",
				code:       "MODULE Test; VAR x: INT64; n: INT32; y: INT64; BEGIN y := ASR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ASR_x_REAL",
				code:        "MODULE Test; VAR x: REAL; n: INT32; y: INT32; BEGIN y := ASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_ASR_n_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; y: INT32; BEGIN y := ASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_ASR_no_args",
				code:        "MODULE Test; VAR y: INT32; BEGIN y := ASR() END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects exactly two arguments",
			},
			{
				name:        "Invalid_ASR_too_many_args",
				code:        "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ASR(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects exactly two arguments",
			},
			{
				name:        "Invalid_ASR_x_CHAR",
				code:        "MODULE Test; VAR x: CHAR; n: INT32; y: INT32; BEGIN y := ASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_ASR_n_CHAR",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; y: INT32; BEGIN y := ASR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ASR' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "ENTIER",
		cases: []procTestCase{
			{
				name:       "Valid_ENTIER_REAL",
				code:       "MODULE Test; VAR x: REAL; y: INT64; BEGIN y := ENTIER(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_ENTIER_LONGREAL",
				code:       "MODULE Test; VAR x: LONGREAL; y: INT64; BEGIN y := ENTIER(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ENTIER_INT32",
				code:        "MODULE Test; VAR x: INT32; y: INT64; BEGIN y := ENTIER(x) END Test.",
				shouldPass:  false,
				expectError: "'ENTIER' expects a real type as argument",
			},
			{
				name:        "Invalid_ENTIER_no_args",
				code:        "MODULE Test; VAR y: INT64; BEGIN y := ENTIER() END Test.",
				shouldPass:  false,
				expectError: "'ENTIER' expects exactly one argument",
			},
			{
				name:        "Invalid_ENTIER_too_many_args",
				code:        "MODULE Test; VAR x: REAL; y: INT64; BEGIN y := ENTIER(x, x) END Test.",
				shouldPass:  false,
				expectError: "'ENTIER' expects exactly one argument",
			},
		},
	},
	{
		procName: "LSL",
		cases: []procTestCase{
			{
				name:       "Valid_LSL_INT32",
				code:       "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := LSL(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_LSL_INT64",
				code:       "MODULE Test; VAR x: INT64; n: INT32; y: INT64; BEGIN y := LSL(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_LSL_x_REAL",
				code:        "MODULE Test; VAR x: REAL; n: INT32; y: INT32; BEGIN y := LSL(x, n) END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_LSL_n_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; y: INT32; BEGIN y := LSL(x, n) END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects the second argument to be INT32",
			},
			{
				name:        "Invalid_LSL_no_args",
				code:        "MODULE Test; VAR y: INT32; BEGIN y := LSL() END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects exactly two arguments",
			},
			{
				name:        "Invalid_LSL_too_many_args",
				code:        "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := LSL(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects exactly two arguments",
			},
			{
				name:        "Invalid_LSL_x_CHAR",
				code:        "MODULE Test; VAR x: CHAR; n: INT32; y: INT32; BEGIN y := LSL(x, n) END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects the first argument to be INT32 or INT64",
			},
			{
				name:        "Invalid_LSL_n_CHAR",
				code:        "MODULE Test; VAR x: INT32; n: CHAR; y: INT32; BEGIN y := LSL(x, n) END Test.",
				shouldPass:  false,
				expectError: "'LSL' expects the second argument to be INT32",
			},
		},
	},
	{
		procName: "ROR",
		cases: []procTestCase{
			{
				name:       "Valid_ROR_INT32",
				code:       "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ROR(x, n) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_ROR_x_REAL",
				code:        "MODULE Test; VAR x: REAL; n: INT32; y: INT32; BEGIN y := ROR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects both arguments to be INT32",
			},
			{
				name:        "Invalid_ROR_n_REAL",
				code:        "MODULE Test; VAR x: INT32; n: REAL; y: INT32; BEGIN y := ROR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects both arguments to be INT32",
			},
			{
				name:        "Invalid_ROR_x_INT64",
				code:        "MODULE Test; VAR x: INT64; n: INT32; y: INT32; BEGIN y := ROR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects both arguments to be INT32",
			},
			{
				name:        "Invalid_ROR_n_INT64",
				code:        "MODULE Test; VAR x: INT32; n: INT64; y: INT32; BEGIN y := ROR(x, n) END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects both arguments to be INT32",
			},
			{
				name:        "Invalid_ROR_no_args",
				code:        "MODULE Test; VAR y: INT32; BEGIN y := ROR() END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects exactly two arguments",
			},
			{
				name:        "Invalid_ROR_too_many_args",
				code:        "MODULE Test; VAR x: INT32; n: INT32; y: INT32; BEGIN y := ROR(x, n, n) END Test.",
				shouldPass:  false,
				expectError: "'ROR' expects exactly two arguments",
			},
		},
	},
	{
		procName: "WCHR",
		cases: []procTestCase{
			{
				name:       "Valid_WCHR_INT32",
				code:       "MODULE Test; VAR x: INT32; w: WCHAR; BEGIN w := WCHR(x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_WCHR_INT64",
				code:       "MODULE Test; VAR x: INT64; w: WCHAR; BEGIN w := WCHR(x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_WCHR_REAL",
				code:        "MODULE Test; VAR x: REAL; w: WCHAR; BEGIN w := WCHR(x) END Test.",
				shouldPass:  false,
				expectError: "'WCHR' expects an integer type as argument",
			},
			{
				name:        "Invalid_WCHR_CHAR",
				code:        "MODULE Test; VAR x: CHAR; w: WCHAR; BEGIN w := WCHR(x) END Test.",
				shouldPass:  false,
				expectError: "'WCHR' expects an integer type as argument",
			},
			{
				name:        "Invalid_WCHR_no_args",
				code:        "MODULE Test; VAR w: WCHAR; BEGIN w := WCHR() END Test.",
				shouldPass:  false,
				expectError: "'WCHR' expects exactly one argument",
			},
			{
				name:        "Invalid_WCHR_too_many_args",
				code:        "MODULE Test; VAR x: INT32; w: WCHAR; BEGIN w := WCHR(x, x) END Test.",
				shouldPass:  false,
				expectError: "'WCHR' expects exactly one argument",
			},
		},
	},
	{
		procName: "STRLEN",
		cases: []procTestCase{
			{
				name:       "Valid_STRLEN_array_CHAR",
				code:       "MODULE Test; VAR s: ARRAY 10 OF CHAR; n: INT32; BEGIN n := STRLEN(s) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_STRLEN_array_WCHAR",
				code:       "MODULE Test; VAR s: ARRAY 10 OF WCHAR; n: INT32; BEGIN n := STRLEN(s) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_STRLEN_string_literal",
				code:       "MODULE Test; VAR n: INT32; BEGIN n := STRLEN(\"hello\") END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_STRLEN_INT32",
				code:        "MODULE Test; VAR s: INT32; n: INT32; BEGIN n := STRLEN(s) END Test.",
				shouldPass:  false,
				expectError: "'STRLEN' expects an array of CHAR, array of WCHAR, or string literal as argument",
			},
			{
				name:        "Invalid_STRLEN_REAL",
				code:        "MODULE Test; VAR s: REAL; n: INT32; BEGIN n := STRLEN(s) END Test.",
				shouldPass:  false,
				expectError: "'STRLEN' expects an array of CHAR, array of WCHAR, or string literal as argument",
			},
			{
				name:        "Invalid_STRLEN_no_args",
				code:        "MODULE Test; VAR n: INT32; BEGIN n := STRLEN() END Test.",
				shouldPass:  false,
				expectError: "'STRLEN' expects exactly one argument",
			},
			{
				name:        "Invalid_STRLEN_too_many_args",
				code:        "MODULE Test; VAR s: ARRAY 10 OF CHAR; n: INT32; BEGIN n := STRLEN(s, s) END Test.",
				shouldPass:  false,
				expectError: "'STRLEN' expects exactly one argument",
			},
		},
	},
	{
		procName: "MIN",
		cases: []procTestCase{
			{
				name:       "Valid_MIN_INT32_INT32",
				code:       "MODULE Test; VAR x, y, z: INT32; BEGIN z := MIN(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_INT32_INT64",
				code:       "MODULE Test; VAR x: INT32; y: INT64; z: INT64; BEGIN z := MIN(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_REAL_LONGREAL",
				code:       "MODULE Test; VAR x: REAL; y: LONGREAL; z: LONGREAL; BEGIN z := MIN(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_CHAR_CHAR",
				code:       "MODULE Test; VAR x, y, z: CHAR; BEGIN z := MIN(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_CHAR_WCHAR",
				code:       "MODULE Test; VAR x: CHAR; y: WCHAR; z: WCHAR; BEGIN z := MIN(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_MIN_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; y: REAL; z: INT32; BEGIN z := MIN(x, y) END Test.",
				shouldPass:  false,
				expectError: "expression 'MIN(x, y)' is not assignment compatible with variable 'z'",
			},
			{
				name:        "Invalid_MIN_CHAR_INT32",
				code:        "MODULE Test; VAR x: CHAR; y: INT32; z: CHAR; BEGIN z := MIN(x, y) END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects both arguments to be numeric or char types",
			},
			{
				name:        "Invalid_MIN_no_args",
				code:        "MODULE Test; VAR z: INT32; BEGIN z := MIN() END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects one or two arguments",
			},
			{
				name:        "Invalid_MIN_one_arg",
				code:        "MODULE Test; VAR x: INT32; z: INT32; BEGIN z := MIN(x) END Test.",
				shouldPass:  false,
				expectError: "'MIN': single argument must be a type",
			},
			{
				name:        "Invalid_MIN_too_many_args",
				code:        "MODULE Test; VAR x, y, z: INT32; BEGIN z := MIN(x, y, z) END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects one or two arguments",
			},
		},
	},
	{
		procName: "MAX",
		cases: []procTestCase{
			{
				name:       "Valid_MAX_INT32_INT32",
				code:       "MODULE Test; VAR x, y, z: INT32; BEGIN z := MAX(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_INT32_INT64",
				code:       "MODULE Test; VAR x: INT32; y: INT64; z: INT64; BEGIN z := MAX(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_REAL_LONGREAL",
				code:       "MODULE Test; VAR x: REAL; y: LONGREAL; z: LONGREAL; BEGIN z := MAX(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_CHAR_CHAR",
				code:       "MODULE Test; VAR x, y, z: CHAR; BEGIN z := MAX(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_CHAR_WCHAR",
				code:       "MODULE Test; VAR x: CHAR; y: WCHAR; z: WCHAR; BEGIN z := MAX(x, y) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_MAX_INT32_REAL",
				code:        "MODULE Test; VAR x: INT32; y: REAL; z: INT32; BEGIN z := MAX(x, y) END Test.",
				shouldPass:  false,
				expectError: "expression 'MAX(x, y)' is not assignment compatible with variable 'z'",
			},
			{
				name:        "Invalid_MAX_CHAR_INT32",
				code:        "MODULE Test; VAR x: CHAR; y: INT32; z: CHAR; BEGIN z := MAX(x, y) END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects both arguments to be numeric or char types",
			},
			{
				name:        "Invalid_MAX_no_args",
				code:        "MODULE Test; VAR z: INT32; BEGIN z := MAX() END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects one or two arguments",
			},
			{
				name:        "Invalid_MAX_one_arg",
				code:        "MODULE Test; VAR x: INT32; z: INT32; BEGIN z := MAX(x) END Test.",
				shouldPass:  false,
				expectError: "'MAX': single argument must be a type",
			},
			{
				name:        "Invalid_MAX_too_many_args",
				code:        "MODULE Test; VAR x, y, z: INT32; BEGIN z := MAX(x, y, z) END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects one or two arguments",
			},
		},
	},

	{
		procName: "SIZE",
		cases: []procTestCase{
			{
				name:       "Valid_SIZE_INT32",
				code:       "MODULE Test; VAR n: INT32; BEGIN n := SIZE(INT32) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SIZE_ARRAY",
				code:       "MODULE Test; VAR n: INT32; BEGIN n := SIZE(ARRAY 10 OF CHAR) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_SIZE_RECORD",
				code:       "MODULE Test; TYPE R = RECORD x: INT8; y: INT16 END; VAR n: INT32; BEGIN n := SIZE(R) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_SIZE_no_args",
				code:        "MODULE Test; VAR n: INT32; BEGIN n := SIZE() END Test.",
				shouldPass:  false,
				expectError: "'SIZE' expects exactly one argument",
			},
			{
				name:        "Invalid_SIZE_too_many_args",
				code:        "MODULE Test; VAR n: INT32; BEGIN n := SIZE(INT32, INT16) END Test.",
				shouldPass:  false,
				expectError: "'SIZE' expects exactly one argument",
			},
		},
	},
	{
		procName: "MAX",
		cases: []procTestCase{
			{
				name:       "Valid_MAX_INT32_type",
				code:       "MODULE Test; VAR m : INT32;BEGIN m := MAX(INT32) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_SET_type",
				code:       "MODULE Test; VAR m :INT32; BEGIN m := MAX(SET) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MAX_enum_type",
				code:       "MODULE Test; TYPE Color = (Red, Green, Blue); VAR m: BYTE; BEGIN m := MAX(Color) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_MAX_no_args",
				code:        "MODULE Test; VAR m :INT32; BEGIN  m := MAX() END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects one or two arguments",
			},
			{
				name:        "Invalid_MAX_too_many_args",
				code:        "MODULE Test; VAR m :INT32; BEGIN m := MAX(INT32, INT32, INT32) END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects one or two arguments",
			},
			{
				name:        "Invalid_MAX_non_type_arg",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := MAX(x) END Test.",
				shouldPass:  false,
				expectError: "'MAX': single argument must be a type",
			},
			{
				name:        "Invalid_MAX_unsupported_type",
				code:        "MODULE Test; TYPE Rec = RECORD END; VAR m: BYTE; BEGIN m := MAX(Rec) END Test.",
				shouldPass:  false,
				expectError: "'MAX' expects a basic type or enum",
			},
		},
	},
	{
		procName: "MIN",
		cases: []procTestCase{
			{
				name:       "Valid_MIN_INT32_type",
				code:       "MODULE Test; VAR m: INT32; BEGIN m := MIN(INT32) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_SET_type",
				code:       "MODULE Test; VAR m: INT32; BEGIN m := MIN(SET) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_MIN_enum_type",
				code:       "MODULE Test; TYPE Color = (Red, Green, Blue); VAR m: BYTE; BEGIN m := MIN(Color) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_MIN_no_args",
				code:        "MODULE Test; VAR m: BYTE; BEGIN m := MIN() END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects one or two arguments",
			},
			{
				name:        "Invalid_MIN_too_many_args",
				code:        "MODULE Test; VAR m: BYTE; BEGIN m := MIN(INT32, INT32, INT32) END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects one or two arguments",
			},
			{
				name:        "Invalid_MIN_non_type_arg",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := MIN(x) END Test.",
				shouldPass:  false,
				expectError: "'MIN': single argument must be a type",
			},
			{
				name:        "Invalid_MIN_unsupported_type",
				code:        "MODULE Test; TYPE Rec = RECORD END; VAR m: BYTE; BEGIN m := MIN(Rec) END Test.",
				shouldPass:  false,
				expectError: "'MIN' expects a basic type or enum",
			},
		},
	},
	{
		procName: "DEFAULT",
		cases: []procTestCase{
			{
				name:       "Valid_DEFAULT_INT32",
				code:       "MODULE Test; VAR x: INT32; BEGIN x := DEFAULT(INT32) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_CHAR",
				code:       "MODULE Test; VAR c: CHAR; BEGIN c := DEFAULT(CHAR) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_BOOLEAN",
				code:       "MODULE Test; VAR b: BOOL; BEGIN b := DEFAULT(BOOL) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_SET",
				code:       "MODULE Test; VAR s: SET; BEGIN s := DEFAULT(SET) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_enum",
				code:       "MODULE Test; TYPE Color = (Red, Green, Blue); VAR c: Color; BEGIN c := DEFAULT(Color) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_pointer",
				code:       "MODULE Test; TYPE P = POINTER TO RECORD END; VAR p: P; BEGIN p := DEFAULT(P) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_proc",
				code:       "MODULE Test; VAR p: PROCEDURE; BEGIN p := DEFAULT(PROCEDURE) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_record",
				code:       "MODULE Test; TYPE R = RECORD x: INT32 END; VAR r: R; BEGIN r := DEFAULT(R) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_DEFAULT_array",
				code:       "MODULE Test; TYPE A = ARRAY 3 OF INT32; VAR a: A; BEGIN a := DEFAULT(A) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_DEFAULT_no_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := DEFAULT() END Test.",
				shouldPass:  false,
				expectError: "'DEFAULT' expects exactly one argument",
			},
			{
				name:        "Invalid_DEFAULT_too_many_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := DEFAULT(INT32, INT32) END Test.",
				shouldPass:  false,
				expectError: "'DEFAULT' expects exactly one argument",
			},
			{
				name:        "Invalid_DEFAULT_non_type_arg",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := DEFAULT(x) END Test.",
				shouldPass:  false,
				expectError: "'DEFAULT' expects a type as argument",
			},
			{
				name:       "Invalid_DEFAULT_unsupported_type",
				code:       "MODULE Test; TYPE F = PROCEDURE (_: INT32): INT32; VAR f: F; BEGIN f := DEFAULT(F) END Test.",
				shouldPass: true,
			},
		},
	},
	{
		procName: "LDMOD",
		cases: []procTestCase{
			{
				name:       "Valid_LDMOD_string",
				code:       "MODULE Test; VAR b: BOOL; BEGIN b := LDMOD(\"MyModule\") END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_LDMOD_no_args",
				code:        "MODULE Test; VAR b: BOOL; BEGIN b := LDMOD() END Test.",
				shouldPass:  false,
				expectError: "'LDMOD' expects exactly one argument",
			},
			{
				name:        "Invalid_LDMOD_too_many_args",
				code:        "MODULE Test; VAR b: BOOL; BEGIN b := LDMOD(\"A\", \"B\") END Test.",
				shouldPass:  false,
				expectError: "'LDMOD' expects exactly one argument",
			},
			{
				name:        "Invalid_LDMOD_non_string_arg",
				code:        "MODULE Test; VAR b: BOOL; BEGIN b := LDMOD(123) END Test.",
				shouldPass:  false,
				expectError: "'LDMOD' expects a string argument",
			},
		},
	},
	{
		procName: "LDCMD",
		cases: []procTestCase{
			{
				name:       "Valid_LDCMD_two_strings",
				code:       "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD(\"Mod\", \"Cmd\") END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_LDCMD_no_args",
				code:        "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD() END Test.",
				shouldPass:  false,
				expectError: "'LDCMD' expects exactly two arguments",
			},
			{
				name:        "Invalid_LDCMD_one_arg",
				code:        "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD(\"Mod\") END Test.",
				shouldPass:  false,
				expectError: "'LDCMD' expects exactly two arguments",
			},
			{
				name:        "Invalid_LDCMD_three_args",
				code:        "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD(\"A\", \"B\", \"C\") END Test.",
				shouldPass:  false,
				expectError: "'LDCMD' expects exactly two arguments",
			},
			{
				name:        "Invalid_LDCMD_non_string_first_arg",
				code:        "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD(123, \"Cmd\") END Test.",
				shouldPass:  false,
				expectError: "'LDCMD' expects string arguments",
			},
			{
				name:        "Invalid_LDCMD_non_string_second_arg",
				code:        "MODULE Test; VAR p: PROCEDURE; BEGIN p := LDCMD(\"Mod\", 456) END Test.",
				shouldPass:  false,
				expectError: "'LDCMD' expects string arguments",
			},
		},
	},
	{
		procName: "CAST",
		cases: []procTestCase{
			{
				name:       "Valid_CAST_enum_from_ordinal",
				code:       "MODULE Test; TYPE Color = (Red, Green, Blue); VAR c: Color; BEGIN c := CAST(Color, 1) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CAST_INT32_from_INT8",
				code:       "MODULE Test; VAR x: INT8; VAR y: INT32; BEGIN y := CAST(INT32, x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CAST_INT8_from_INT32",
				code:       "MODULE Test; VAR x: INT32; VAR y: INT8; BEGIN y := CAST(INT8, x) END Test.",
				shouldPass: true,
			},
			{
				name:       "Valid_CAST_INT64_from_INT32",
				code:       "MODULE Test; VAR x: INT32; VAR y: INT64; BEGIN y := CAST(INT64, x) END Test.",
				shouldPass: true,
			},
			{
				name:        "Invalid_CAST_no_args",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := CAST() END Test.",
				shouldPass:  false,
				expectError: "'CAST' expects exactly two arguments",
			},
			{
				name:        "Invalid_CAST_one_arg",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := CAST(INT32) END Test.",
				shouldPass:  false,
				expectError: "'CAST' expects exactly two arguments",
			},
			{
				name:        "Invalid_CAST_non_type_first_arg",
				code:        "MODULE Test; VAR x: INT32; BEGIN x := CAST(x, 1) END Test.",
				shouldPass:  false,
				expectError: "'CAST' expects a type as first argument",
			},
			{
				name:        "Invalid_CAST_enum_from_non_ordinal",
				code:        "MODULE Test; TYPE Color = (Red, Green, Blue); VAR c: Color; BEGIN c := CAST(Color, TRUE) END Test.",
				shouldPass:  false,
				expectError: "'CAST' expected second argument to be ordinal number or integer",
			},
			{
				name:        "Invalid_CAST_integer_from_non_integer",
				code:        "MODULE Test; VAR x: REAL; VAR y: INT32; BEGIN y := CAST(INT32, x) END Test.",
				shouldPass:  false,
				expectError: "'CAST' expected second argument to be ordinal number or integer",
			},
			{
				name:        "Invalid_CAST_unsupported_type",
				code:        "MODULE Test; TYPE P = POINTER TO RECORD END; VAR p: P; BEGIN p := CAST(P, 1) END Test.",
				shouldPass:  false,
				expectError: "'CAST' expects first argument to be integer or enum",
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
		FileName:        file,
		FilePath:        file,
		Content:         []byte(code),
		Source:          srcMgr,
		Reporter:        reporter,
		TabWidth:        4,
		Env:             ast.NewEnv(),
		Names:           adt.NewStack[string](),
		ExprLists:       adt.NewStack[[]ast.Expression](),
		SymbolOverrides: map[string]ast.Symbol{},
		TypeOverrides:   map[string]types.Type{},
	}
	p := parser.NewParser(ctx)
	unit := p.Parse()
	if ctx.Reporter.ErrorCount() > 0 {
		return ctx
	}

	obx.AddUnit(unit)
	s := NewSema(ctx, obx)
	s.Validate()

	return ctx
}

type typeCheckTestCase struct {
	Name     string
	Source   string
	WantErrs []string
}

func TestTypeCheckPrograms(t *testing.T) {
	tests := []typeCheckTestCase{
		{
			Name: "Valid function call with integer",
			Source: `
				MODULE M;
				PROCEDURE Act(x: INTEGER); END
				BEGIN Act(5) END M.
			`,
			WantErrs: nil,
		},
		{
			Name: "Call with incorrect type",
			Source: `
				MODULE M;
				PROCEDURE Act(x: INTEGER); END Act;
				BEGIN Act(TRUE) END M.
			`,
			WantErrs: []string{"argument 1: of type, 'bool' is incompatible with parameter of type 'integer'"},
		},
		{
			Name: "Procedure with return type mismatch",
			Source: `
				MODULE M;
				PROCEDURE Add(): INTEGER;
				BEGIN 
					RETURN TRUE 
				END Add

				END M.
			`,
			WantErrs: nil,
		},
		{
			Name: "Call procedure with wrong number of args",
			Source: `
				MODULE Test;
				  PROCEDURE Foo(x: INTEGER);
				  END Foo;
				
				BEGIN
				  Foo()
				END Test.
				`,
			WantErrs: []string{"expected 1 arguments, got 0"},
		},
		{
			Name: "Invalid type-bound procedure receiver",
			Source: `
				MODULE Test;
				  TYPE
					P = POINTER TO ARRAY OF INTEGER;
				
				  PROCEDURE (p: P) DoSomething;
				  END DoSomething;
				
				END Test.
				`,
			WantErrs: []string{"value receiver type must be a pointer to record type"},
		},
		{
			Name: "Valid type-bound procedure",
			Source: `
				MODULE Test;
				  TYPE
					T = POINTER TO RECORD END;

				  PROCEDURE (t: T) Print;
				  END Print
				END Test.
				`,
			WantErrs: nil,
		},
		{
			Name: "Valid Proper and Function Procedures",
			Source: `
				MODULE M;
				  VAR x: INTEGER;
				
				  PROCEDURE Incr(VAR a: INTEGER);
				  BEGIN a := a + 1
				  END Incr;
				
				  PROCEDURE Square(x: INTEGER): INTEGER;
				  BEGIN RETURN x * x
				  END Square;
				
				  PROCEDURE Main;
					VAR y: INTEGER;
				  BEGIN
					Incr(y);
					y := Square(y)
				  END Main;
				END M.`,
			WantErrs: nil,
		},
		{
			Name: "Valid Type-bound Procedure and Redefinition ",
			Source: `
				MODULE M;
				  TYPE R = RECORD END;
					   S = RECORD (R) END;
				  VAR x: S;
				
				  PROCEDURE (VAR r: R) P;
				  END P;
				
				  PROCEDURE (VAR s: S) P;
				  BEGIN
					s.P^() (* call super P *)
				  END P;
				END M.`,
			WantErrs: nil,
		},
		{
			Name: "Type-bound Redefinition (Invalid: parameter mismatch)",
			Source: `
				MODULE M;
				  TYPE T = RECORD END;
					   U = RECORD (T) END;
				
				  PROCEDURE (VAR t: T) Foo(x: INTEGER);
				  END Foo;
				
				  PROCEDURE (IN u: U) Foo(x: CHAR);  (* ERROR *)
				  END Foo;
				END M.`,
			WantErrs: []string{"parameter mismatch between of super and redefinition of 'Foo'"},
		},
		{
			Name: "Type-bound Procedure Call and Dispatch",
			Source: `MODULE M;
				  TYPE T = RECORD END;
					   U = RECORD (T) END;
				
				  PROCEDURE (VAR t: T) Print;
				  END Print;
				
				  VAR u: U;
				
				  PROCEDURE Main;
				  BEGIN
					u.Print()  (* dispatch to U.Print or fallback to T.Print *)
				  END Main;
				END M.
				`,
			WantErrs: nil,
		},
		{
			Name: "Procedure Types and Assignment Compatibility",
			Source: `MODULE M;
				  TYPE P = PROCEDURE(_: INTEGER);
				  VAR f: P;
				
				  PROCEDURE Impl(x: INTEGER);
				  END Impl;
				
				  PROCEDURE Main;
				  BEGIN
					f := Impl;  (* compatible procedure type *)
					f(42)
				  END Main;
				END M.
				`,
			WantErrs: nil,
		},
		{
			Name: "Predeclared Procedure Call (ABS)",
			Source: `
				MODULE M;
				  VAR x, y: INTEGER;
				
				  PROCEDURE Main;
				  BEGIN
					y := ABS(x)
				  END Main;
				END M.`,
			WantErrs: nil,
		},
		{
			Name: "Call Procedure via Procedure Variable",
			Source: `MODULE M;
				  TYPE Handler = PROCEDURE(VAR x: INTEGER);
				  VAR doSomething: Handler;
				
				  PROCEDURE Impl(VAR x: INTEGER);
				  BEGIN x := x + 1
				  END Impl;
				
				  PROCEDURE Main;
					VAR n: INTEGER;
				  BEGIN
					doSomething := Impl;
					doSomething(n)
				  END Main;
				END M.
				`,
			WantErrs: nil,
		},
		{
			Name: "Calling a non-procedure type ",
			Source: `MODULE M;
				  VAR x: INTEGER;
				
				  PROCEDURE Main;
				  BEGIN
					x()  (* ERROR: x is not a procedure *)
				  END Main;
				END M.
				`,
			WantErrs: []string{"name 'x' could not be resolved to a procedure"},
		},
		{
			Name: "Using Procedure as Expression",
			Source: `MODULE M;
				  PROCEDURE F(x: INTEGER): INTEGER;
				  BEGIN RETURN x * 2
				  END F;
				
				  VAR g: PROCEDURE(x: INTEGER): INTEGER;
				
				  PROCEDURE Main;
					VAR y: INTEGER;
				  BEGIN
					g := F;
					y := g(5)
				  END Main;
				END M.
				`,
			WantErrs: nil,
		},
		{
			Name: "Type-bound Procedure with POINTER Receiver",
			Source: `MODULE M;
			  TYPE R = RECORD END;
				   P = POINTER TO R;
			
			  PROCEDURE (r: P) DoSomething;
			  END DoSomething;
			
			  VAR x: P;
			
			  PROCEDURE Main;
			  BEGIN
				NEW(x);
				x.DoSomething()
			  END Main;
			END M.
			`,
			WantErrs: nil,
		},
		{
			Name: "Valid proper and function procedures, with type-bound methods and calls",
			Source: `
				MODULE Test;
				  TYPE
					T = RECORD x: INTEGER END;
				  VAR
					t: T;
				
				  PROCEDURE Inc(VAR x: INTEGER);
				  BEGIN x := x + 1
				  END Inc;
				
				  PROCEDURE Add(a, b: INTEGER): INTEGER;
				  BEGIN RETURN a + b
				  END Add;
				
				  PROCEDURE (VAR t: T) Update(y: INTEGER);
				  BEGIN t.x := y
				  END Update;
				
				BEGIN
				  Inc(t.x);
				  Add(1, 2);
				  t.Update(42)
				END Test.
				`,
			WantErrs: nil,
		},
		{
			Name: "Type-bound method override with mismatched parameters",
			Source: `
				MODULE Test;
				  TYPE
					T0 = RECORD END;
					T1 = RECORD (T0) END;
				
				  PROCEDURE (VAR t: T0) P(a: INTEGER);
				  END P;
				
				  PROCEDURE (VAR t: T1) P(a, b: INTEGER);
				  END P;
				
				END Test.
				`,
			WantErrs: []string{"parameter mismatch between of super and redefinition of 'P'"},
		},
		{
			Name: "Type-bound method called with invalid receiver",
			Source: `
				MODULE Test;
				  TYPE
					T = RECORD END;
				
				  PROCEDURE (VAR t: T) M();
				  END M;
				
				  VAR x: INTEGER;
				
				BEGIN
				  x.M() (* invalid call, x is not of type T *)
				END Test.
				`,
			WantErrs: []string{
				"cannot select field 'M' of non-record 'x",
				"name 'x.M' could not be resolved to a procedure",
			},
		},
		{
			Name: "Call to undefined procedure",
			Source: `
				MODULE Test;
				BEGIN
				  Foo() (* undefined procedure *)
				END Test.
				`,
			WantErrs: []string{
				"undeclared identifier: 'Foo'",
				"name 'Foo' could not be resolved to a procedure",
			},
		},
		{
			Name: "Valid procedure type declaration and variable",
			Source: `
				MODULE Test;
				  TYPE
					Handler = PROCEDURE (_: INTEGER): INTEGER;
				
				  VAR
					f: Handler;
				
				  PROCEDURE Square(x: INTEGER): INTEGER;
				  BEGIN RETURN x * x
				  END Square;
				
				BEGIN
				  f := Square;
				  f(5)
				END Test.
				`,
			WantErrs: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			ctx := typeCheckSnippet(t, tt.Source)

			diags := ctx.Reporter.Diagnostics()
			if len(diags) != len(tt.WantErrs) {
				t.Errorf("got %d diagnostics, want %d", len(diags), len(tt.WantErrs))
				for _, d := range diags {
					t.Logf("diag: %s", d.Message)
				}
				return
			}
			for i, want := range tt.WantErrs {
				if !strings.Contains(diags[i].Message, want) {
					t.Errorf("diagnostic %d = %q, want substring %q", i, diags[i].Message, want)
				}
			}
		})
	}
}

func TestTypeCheckCaseStatementsPrograms(t *testing.T) {
	tests := []typeCheckTestCase{
		{
			Name: "Valid Integer Case Ranges",
			Source: `
				MODULE valid_case_includes;
				VAR x: INT16;
				BEGIN
					CASE x OF
                      | 1: ASSERT(TRUE)
					  | 5 .. 10: ASSERT(TRUE)
					END
				END valid_case_includes.
			`,
			WantErrs: nil,
		},
		{
			Name: "Case Expression Type Does Not Include Label Type",
			Source: `
				MODULE incompatible_case_type;
				VAR x: INT8;
				BEGIN
					CASE x OF
						300: ASSERT(TRUE)     (* too large for INT8 *)
					END
				END incompatible_case_type.
			`,
			WantErrs: []string{
				"CASE label '300' is invalid for the range of CASE expression ('x': 'int8')",
			},
		},
		{
			Name: "Duplicate Labels in Range",
			Source: `
				MODULE duplicate_case_label;
				VAR x: INT32;
				BEGIN
					CASE x OF
						| 1 .. 3: ASSERT(TRUE)
						| 3 .. 5: ASSERT(TRUE)
					END
				END duplicate_case_label.
			`,
			WantErrs: []string{
				"duplicate case label 3",
			},
		},
		{
			Name: "Label Not Constant",
			Source: `
				MODULE non_constant_label;
				CONST A = 5;
				VAR x, y: INT32;
				BEGIN
					CASE x OF
						| y: ASSERT(TRUE)
						| A: ASSERT(TRUE)
					END
				END non_constant_label.
			`,
			WantErrs: []string{
				"CASE label must be a constant",
				"'y' is not a valid INTEGER",
			},
		},
		{
			Name: "CHAR Label on INT32 Case Expression",
			Source: `
				MODULE char_label_on_int;
				VAR x: INT32;
				BEGIN
					CASE x OF
						"A": ASSERT(TRUE)
					END
				END char_label_on_int.
			`,
			WantErrs: []string{
				"CASE label 'A' is invalid for the range of CASE expression ('x': 'int32')",
				"'A' is not a valid INTEGER",
			},
		},
		{
			Name: "Valid CHAR Case Labels",
			Source: `
		MODULE char_valid;
		VAR ch: CHAR;
		BEGIN
			CASE ch OF
				| "A": ASSERT(TRUE)
				| "Z": ASSERT(TRUE)
				| "0" .. "9": ASSERT(TRUE)
			END
		END char_valid.`,
			WantErrs: nil,
		},
		{
			Name: "Mixed Label Type (INT and CHAR)",
			Source: `
		MODULE char_mixed_label_type;
		VAR ch: CHAR;
		BEGIN
			CASE ch OF
				| "A": ASSERT(TRUE)
				| 65: ASSERT(TRUE) (* Invalid: 65 is INT, not CHAR *)
			END
		END char_mixed_label_type.
	`,
			WantErrs: []string{
				"CASE label must be CHAR type",
				"'65' is not a valid LATIN-1 CHAR",
			},
		},
		{
			Name: "Duplicate CHAR Label",
			Source: `
		MODULE char_duplicate_label;
		VAR ch: CHAR;
		BEGIN
			CASE ch OF
				| "A" .. "C": ASSERT(TRUE)
				| "C" .. "F": ASSERT(TRUE) (* Overlap at "C" *)
			END
		END char_duplicate_label.
	`,
			WantErrs: []string{
				"duplicate case label 'C'",
			},
		},
		{
			Name: "CHAR Label Out of Range",
			Source: `
		MODULE char_label_out_of_range;
		VAR ch: CHAR;
		BEGIN
			CASE ch OF
				"Ā": ASSERT(TRUE)  (* Invalid: outside Latin-1 CHAR range *)
			END
		END char_label_out_of_range.
	`,
			WantErrs: []string{
				"CASE label must be CHAR type",
				"'Ā' is not a valid LATIN-1 CHAR",
			},
		},
		{
			Name: "Invalid Range (Low > High)",
			Source: `
		MODULE char_invalid_range;
		VAR ch: CHAR;
		BEGIN
			CASE ch OF
				"Z" .. "A": ASSERT(TRUE)
			END
		END char_invalid_range.
	`,
			WantErrs: []string{
				"Invalid case label range",
			},
		},
		{
			Name: "Valid Enumeration Case Labels",
			Source: `
		MODULE enum_valid;
			TYPE Color = (Red, Green, Blue);
			VAR c: Color;
		BEGIN
			CASE c OF
				| Red: ASSERT(TRUE)
				| Green: ASSERT(TRUE)
				| Blue: ASSERT(TRUE)
			END
		END enum_valid.`,
			WantErrs: nil,
		},
		{
			Name: "Invalid Case Label (Not in Enum)",
			Source: `
				MODULE enum_invalid_label;
					TYPE Color = (Red, Green);
					VAR c: Color;
				BEGIN
					CASE c OF
						| Red: ASSERT(TRUE)
						| Blue: ASSERT(TRUE)  (* Error: Blue is not in Color *)
					END
				END enum_invalid_label.`,
			WantErrs: []string{
				"undeclared identifier: 'Blue'",
				"CASE label must be a constant",
				"CASE label must be of ENUM type, 'Color' got 'unknown'",
				"CASE label, 'Blue' is not a member of ENUM 'Color'",
			},
		},
		{
			Name: "Duplicate Enum Case Label",
			Source: `
				MODULE enum_duplicate_label;
					TYPE Direction = (North, East, South, West);
					VAR d: Direction;
				BEGIN
					CASE d OF
						| North .. East: ASSERT(TRUE)
						| East .. South: ASSERT(FALSE)  (* Error: duplicate 'East' *)
					END
				END enum_duplicate_label.`,
			WantErrs: []string{
				"duplicate case label 'East'",
			},
		},
		{
			Name: "Mixed Type Label",
			Source: `
				MODULE enum_mixed_label;
					TYPE Mode = (Auto, Manual);
					VAR m: Mode;
				BEGIN
					CASE m OF
						| Auto: ASSERT(FALSE)
						| 1: ASSERT(TRUE)  (* Error: INT used as label on ENUM case *)
					END
				END enum_mixed_label.`,
			WantErrs: []string{
				"CASE label must be of ENUM type",
				"CASE label, '1' is not a member of ENUM 'Mode'",
			},
		},
		{
			Name: "Invalid Range (Low > High)",
			Source: `
				MODULE enum_invalid_range;
					TYPE Day = (Mon, Tue, Wed, Thu);
					VAR d: Day;
				BEGIN
					CASE d OF
						Thu .. Tue: ASSERT(FALSE)
					END
				END enum_invalid_range.`,
			WantErrs: []string{
				"Invalid case label range",
			},
		},
		{
			Name: "Valid Pointer Record Type Case",
			Source: `
				MODULE pointer_record_valid;
					TYPE 
						Base = RECORD a: INTEGER END;
						Derived1 = RECORD (Base) b: INTEGER END;
						Derived2 = RECORD (Base) c: REAL END;
						PBase = POINTER TO Base;
						PDerived1 = POINTER TO Derived1;
						PDerived2 = POINTER TO Derived2;
					VAR p: PBase;
				BEGIN
					CASE p OF
						| PDerived1: p.b := 10
						| PDerived2: p.c := 3.14
						| NIL: ASSERT(TRUE)
					END
				END pointer_record_valid.`,
			WantErrs: nil,
		},
		{
			Name: "Label Not Extension of Static Type",
			Source: `
				MODULE pointer_record_not_extension;
					TYPE 
						Base = RECORD END;
						Unrelated = RECORD END;
						PBase = POINTER TO Base;
						PUnrelated = POINTER TO Unrelated;
					VAR p: PBase;
				BEGIN
					CASE p OF
						PUnrelated:  p := NIL
					END
				END pointer_record_not_extension.`,
			WantErrs: []string{
				"CASE label 'PUnrelated' is not an extension of 'PBase'",
			},
		},
		{
			Name: "Duplicate Record Label",
			Source: `
				MODULE pointer_record_duplicate;
					TYPE 
						Base = RECORD END;
						Ext1 = RECORD (Base) END;
						PBase = POINTER TO Base;
						PExt1 = POINTER TO Ext1;
					VAR p: PBase;
				BEGIN
					CASE p OF
						| PExt1: ASSERT(TRUE)
						| PExt1: ASSERT(FALSE)  (* Error: duplicate label *)
					END
				END pointer_record_duplicate.`,
			WantErrs: []string{
				"duplicate CASE label 'PExt1'",
			},
		},
		{
			Name: "Case Label Not a Pointer to Record",
			Source: `
				MODULE pointer_not_pointer_to_record;
					TYPE 
						Base = RECORD END;
						PBase = POINTER TO Base;
					VAR p: PBase;
				BEGIN
					CASE p OF
						Base: ASSERT(TRUE)  (* Error: must be POINTER TO RECORD *)
					END
				END pointer_not_pointer_to_record.`,
			WantErrs: []string{
				"CASE label 'Base' is not an extension of 'PBase'",
			},
		},
		{
			Name: "Static Type Not Pointer or VAR Record",
			Source: `
				MODULE pointer_static_not_allowed;
					TYPE Base = RECORD END;
					VAR r: Base;
				BEGIN
					CASE r OF  (* Error: not a VAR or POINTER TO RECORD *)
						Base: ASSERT(TRUE)
					END
				END pointer_static_not_allowed.`,
			WantErrs: nil,
		},
		{
			Name: "Reuse Dynamic Type in Statements",
			Source: `
				MODULE pointer_dynamic_binding;
					TYPE
						Base = RECORD END;
						Ext = RECORD (Base) f: INTEGER END;
						PBase = POINTER TO Base;
						PExt = POINTER TO Ext;
					VAR p: PBase;
				BEGIN
					CASE p OF
						PExt: p.f := 42  (* allowed: p is considered as Ext inside *)
					END
				END pointer_dynamic_binding.`,
			WantErrs: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			ctx := typeCheckSnippet(t, tt.Source)

			diags := ctx.Reporter.Diagnostics()
			if len(diags) != len(tt.WantErrs) {
				t.Errorf("got %d diagnostics, want %d", len(diags), len(tt.WantErrs))
				for _, d := range diags {
					t.Logf("diag: %s", d.Message)
				}
				return
			}
			for i, want := range tt.WantErrs {
				if !strings.Contains(diags[i].Message, want) {
					t.Errorf("diagnostic %d = %q, want substring %q", i, diags[i].Message, want)
				}
			}
		})
	}
}

func TestTypeCheckWithStatementsPrograms(t *testing.T) {
	tests := []typeCheckTestCase{
		{
			Name: "Simple WITH with record guards",
			Source: `
				MODULE ok_pointer_record;
				  TYPE
					P = RECORD END;
					T = RECORD (P) x: INTEGER END;
				  VAR p: P;
				BEGIN
				  WITH p: T DO
					p.x := 42
				  END
				END ok_pointer_record.`,
			WantErrs: nil,
		},
		{
			Name: "WITH with extended record",
			Source: `
				MODULE ok_extended_record;
				  TYPE 
					Base = RECORD a: INTEGER END;
					Sub = RECORD (Base) b: INTEGER END;
					P = POINTER TO Base;
					T = POINTER TO Sub
				  VAR p: P;
				BEGIN
					WITH p: T DO
						p.b := 99
				  	END
				END ok_extended_record.
			  `,
			WantErrs: nil,
		},
		{
			Name: "WITH with ELSE block",
			Source: `
				MODULE ok_else_block;
				  TYPE 
					P = RECORD END;
					T = RECORD (P) x: INTEGER END;
				  		
				  VAR p: P;
				BEGIN
				  WITH p: T DO
					p.x := 1
				  ELSE
					HALT(1)
				  END
				END ok_else_block.`,
			WantErrs: nil,
		},
		{
			Name: "Same static and dynamic guard type",
			Source: `
				MODULE err_non_pointer;
				  TYPE T = RECORD x: INTEGER END;
				  VAR r: T;
				BEGIN
				  WITH r: T DO
					r.x := 1
				  END
				END err_non_pointer.
			  `,
			WantErrs: nil,
		},
		{
			Name: "invalid/with_non_extension_guard",
			Source: `
				MODULE err_non_extension_guard;
				  TYPE
					A = RECORD a: INTEGER END;
					B = RECORD b: INTEGER END;
					P = POINTER TO A;
				  VAR p: P;
				BEGIN
				  WITH p: B DO
					p.b := 1
				  END
				END err_non_extension_guard.
			  `,
			WantErrs: []string{"'B' does not extend 'P'"},
		},
		{
			Name: "invalid/with_field_missing",
			Source: `
				MODULE err_missing_field;
				  TYPE 
					P = RECORD END;
					T = RECORD (P) x: INTEGER END;
				  VAR p: P;
				BEGIN
				  WITH p: T DO
					p.y := 10  (* y does not exist *)
				  END
				END err_missing_field.`,
			WantErrs: []string{
				"'p' has no field named 'y'",
				"expression '10' is not assignment compatible with variable 'p.y'",
			},
		},
		{
			Name: "WITH with multiple guards and ELSE",
			Source: `
				MODULE ok_multiple_guards;
				  TYPE 
					Q = RECORD END;
					T1 = RECORD (Q) a: INTEGER END;
					T2 = RECORD (Q) b: INTEGER END;
					T3 = RECORD (Q) c: INTEGER END;
					P = POINTER TO T1
				  VAR p: Q;
				BEGIN
				  WITH 
					| p: T1 DO p.a := 1
				  	| p: T2 DO p.b := 2
				  	| p: T3 DO p.c := 4
				  ELSE
					//p.a := 3
					ASSERT(TRUE)
				  END
				END ok_multiple_guards.`,
			WantErrs: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			ctx := typeCheckSnippet(t, tt.Source)

			diags := ctx.Reporter.Diagnostics()
			if len(diags) != len(tt.WantErrs) {
				t.Errorf("got %d diagnostics, want %d", len(diags), len(tt.WantErrs))
				for _, d := range diags {
					t.Logf("diag: %s", d.Message)
				}
				return
			}
			for i, want := range tt.WantErrs {
				if !strings.Contains(diags[i].Message, want) {
					t.Errorf("diagnostic %d = %q, want substring %q", i, diags[i].Message, want)
				}
			}
		})
	}
}

func TestTypeCheckMultiModulePrograms(t *testing.T) {
	tests := []typeCheckTestCase{
		{
			Name: "",
			Source: `
				MODULE Main;
				IMPORT Math, Strings;
				
				VAR r: REAL;
					msg: Strings.String;
				
				BEGIN
				  r := Math.Sqrt(4.0);
				  msg := Strings.Concat("Hello", " World")
				END Main.

				MODULE Math;
				
				PROCEDURE Sqrt*(x: REAL): REAL;
				BEGIN
				  RETURN x  (* placeholder *)
				END Sqrt;
				
				END Math.
				
				MODULE Strings;
				
				TYPE String* = ARRAY 32 OF CHAR;
				
				PROCEDURE Concat*(a, b: String): String;
				VAR res: String;
				BEGIN
				  RETURN res
				END Concat;
				
				END Strings.`,
			WantErrs: nil,
		},
		{
			Name: "Non-Exported Identifier",
			Source: `MODULE Main;
				IMPORT Hidden;
				
				BEGIN
				  Hidden.Secret()
				END Main.
				
				MODULE Hidden;
				
				PROCEDURE Secret();
				END Secret;
				
				END Hidden.
				
				`,
			WantErrs: []string{
				"identifier 'Secret', defined in module 'Hidden' is not exported",
			},
		},
		{
			Name: "Unknown Module",
			Source: `MODULE Main;
				IMPORT Strings;
				
				BEGIN
				  Math.Sqrt(9.0)  (* Math is not imported *)
				END Main.
				
				MODULE Strings;
				
				PROCEDURE UpperCase*(c: CHAR): CHAR;
				BEGIN
				  RETURN c
				END UpperCase;
				
				END Strings.`,
			WantErrs: []string{
				"undeclared identifier: 'Math'",
				"cannot select field 'Sqrt' of non-record 'Math'",
				"name 'Math.Sqrt' could not be resolved to a procedure",
			},
		},
		{
			Name: "Calling Undefined identifier in module",
			Source: `MODULE Main;
				IMPORT Strings;
				IMPORT Math

				BEGIN
				  Math.Sqrt(9.0)  (* Math is not imported *)
				END Main.
				
				MODULE Strings;
				
				PROCEDURE UpperCase*(c: CHAR): CHAR;
				BEGIN
				  RETURN c
				END UpperCase;
				
				END Strings.
				
				MODULE Math
				END Math.`,
			WantErrs: []string{
				"identifier 'Sqrt' is not defined in module 'Math'",
				"name 'Math.Sqrt' could not be resolved to a procedure",
			},
		},
		{
			Name: "Valid Type Import",
			Source: `MODULE Main;
				IMPORT Types;
				
				VAR p: Types.Point;
				
				BEGIN
				  p.x := 10; 
				  p.y := 20
				END Main.
				
				MODULE Types;
				
				TYPE Point* = RECORD x, y: INTEGER END;
				
				END Types.`,
			WantErrs: nil,
		},
		{
			Name: "Error Self Import",
			Source: `MODULE Self;
				IMPORT Self;
				
				BEGIN
				END Self.
				`,
			WantErrs: []string{
				"module 'Self' cannot import itself",
			},
		},
		{
			Name: "Valid Procedure Call Chain",
			Source: `MODULE Main;
				IMPORT A;
				
				BEGIN
				  A.Ping()
				END Main.
				
				MODULE A;
				IMPORT B;
				
				PROCEDURE Ping*();
				BEGIN
				  B.Pong()
				END Ping;
				
				END A.
				
				MODULE B;
				
				PROCEDURE Pong*();
				END Pong;
				
				END B.`,
			WantErrs: nil,
		},
	}

	for _, tt := range tests {
		t.Run(tt.Name, func(t *testing.T) {
			ctx := typeCheckMultiModuleSnippet(t, tt.Source)

			diags := ctx.Reporter.Diagnostics()
			if len(diags) != len(tt.WantErrs) {
				t.Errorf("got %d diagnostics, want %d", len(diags), len(tt.WantErrs))
				for _, d := range diags {
					t.Logf("diag: %s", d.Message)
				}
				return
			}
			for i, want := range tt.WantErrs {
				if !strings.Contains(diags[i].Message, want) {
					t.Errorf("diagnostic %d = %q, want substring %q", i, diags[i].Message, want)
				}
			}
		})
	}
}

func typeCheckMultiModuleSnippet(t *testing.T, code string) *report.Context {
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
		Source:          srcMgr,
		Reporter:        reporter,
		TabWidth:        4,
		Env:             ast.NewEnv(),
		Names:           adt.NewStack[string](),
		ExprLists:       adt.NewStack[[]ast.Expression](),
		SymbolOverrides: map[string]ast.Symbol{},
		TypeOverrides:   map[string]types.Type{},
	}

	headers, err := modgraph.ScanModuleHeaders(file)
	if err != nil {
		ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  err.Error(),
		})

		return ctx
	}

	// 3. Build graph
	graph, err := modgraph.BuildImportGraph(tmp, headers)
	if err != nil {
		ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  err.Error(),
		})
		return ctx
	}

	// 4. Topologically sort
	sorted, err := modgraph.TopoSort(graph)
	if err != nil {
		ctx.Reporter.Report(report.Diagnostic{
			Severity: report.Error,
			Message:  err.Error(),
		})

		return ctx

	}

	for _, header := range sorted {
		data, err := os.ReadFile(header.File)
		if err != nil {
			log.Fatal(err)
		}

		ctx.FileName = filepath.Base(header.File)
		ctx.FilePath = header.File
		ctx.Content = data[header.StartPos:header.EndPos]

		p := parser.NewParser(ctx)
		unit := p.Parse()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			log.Fatalf("%d errors found", ctx.Reporter.ErrorCount())
		}

		obx.AddUnit(unit)
	}

	s := NewSema(ctx, obx)
	s.Validate()

	return ctx
}
