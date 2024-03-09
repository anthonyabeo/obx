package sema

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/sema/scope"
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
	"github.com/anthonyabeo/obx/src/syntax/token"
)

// Two variables a and b with types Ta and Tb are of the same type if
//
//  1. Ta and Tb are both denoted by the same type identifier, or
//  2. Ta is declared to equal Tb in a type declaration of the form Ta = Tb, or
//  3. a and b appear in the same identifier list in a variable, record field,
//     or formal parameter declaration and are not open arrays.
func (v *Visitor) sameType(Ta, Tb types.Type) bool {
	if Ta.String() == Tb.String() {
		return true
	}

	sym := v.env.Lookup(Ta.String())
	if sym != nil && sym.Kind() == scope.TYPE {
		ty := sym.(*scope.TypeName)
		if ty.Type().String() == Tb.String() {
			return true
		}
	}

	// TODO can something be done about #3?

	return false
}

// Two types Ta and Tb are equal if
//
// 1. Ta and Tb are the same type, or
// 2. Ta and Tb are open array types with equal element types, or
// 3. Ta and Tb are procedure types whose formal parameters match, or
// 4. Ta and Tb are pointer types with equal base types.
func (v *Visitor) equalType(Ta, Tb types.Type) bool {
	if v.sameType(Ta, Tb) {
		return true
	}

	TaArr := Ta.(*Array)
	TbArr := Tb.(*Array)
	if (TaArr != nil && TaArr.IsOpen()) && (TbArr != nil && TbArr.IsOpen()) {
		if v.equalType(TbArr.ElemTy, TbArr.ElemTy) {
			return true
		}
	}

	TaProc := Ta.(*ProcedureType)
	TbProc := Tb.(*ProcedureType)
	if TaProc != nil && TbProc != nil {
		if v.paramListMatch(TaProc.fp, TbProc.fp) {
			return true
		}
	}

	TaPtr := Ta.(*PtrType)
	TbPtr := Tb.(*PtrType)
	if TaPtr != nil && TbPtr != nil {
		if v.equalType(TaPtr.UTy, TbPtr.UTy) {
			return true
		}
	}

	return false
}

// An expression e of type Te is assignment compatible with a variable v of type Tv if one of the following conditions hold:
//
// 1. Te and Tv are the same type;
// 2. Te and Tv are numeric or character types and Tv includes Te [3];
// 3. Tv is a SET type and Te is of INT32 or smaller type;
// 4. Tv is a BYTE type and Te is a Latin-1 character type;
// 5. Tv is an integer type and Te is a enumeration type;
// 6. Te and Tv are record types and Te is a type extension of Tv and the dynamic type of v is Tv;
// 7. Te and Tv are pointer types and Te is a type extension of Tv or the pointers have equal base types;
// 8. Tv is a pointer or a procedure type and e is NIL;
// 9. Te is an open array and Tv is an array of equal base type;
// 10. Tv is an array of WCHAR, Te is a Unicode BMP or Latin-1 string or character array, and STRLEN(e) < LEN(v);
// 11. Tv is an array of CHAR, Te is a Latin-1 string or character array, and STRLEN(e) < LEN(v);
// 12. Tv is a procedure type and e is the name of a procedure whose formal parameters match those of Tv.
func (v *Visitor) assignCompat(Te, Tv types.Type) bool {
	if v.sameType(Te, Tv) {
		return true
	}

	TeBasic := Te.(*types.Basic)
	TvBasic := Tv.(*types.Basic)
	if TeBasic.Info()|types.IsNumeric == types.IsNumeric && TvBasic.Info()|types.IsNumeric == types.IsNumeric {
		return TvBasic.Kind() >= TeBasic.Kind()
	}

	if TvBasic.Info() == types.IsSet && TeBasic.Kind() < types.Int32 {
		return true
	}

	if TvBasic.Kind() == types.Byte && TeBasic.Kind() == types.Char {
		return true
	}

	TeEnum := Te.(*Enum)
	if TvBasic.Info() == types.IsInteger && TeEnum != nil {
		return true
	}

	// TODO 6-12 Remain

	return false
}

// Given a type declaration Tb = RECORD(Ta)...END, Tb is a direct extension of Ta, and Ta is a direct base type of Tb.
// A type Tb is an extension of a type Ta (Ta is a base type of Tb) if:
//
// 1. Ta and Tb are the same types, or
// 2. Tb is a direct extension of Ta.
// 3. Ta is of type ANYREC.
func (v *Visitor) recordTyExt(baseTy, extTy types.Type) bool {
	if v.sameType(baseTy, extTy) {
		return true
	}

	if v.directExt(baseTy, extTy) {
		return true
	}

	if baseTy.String() == "ANYREC" {
		return true
	}

	return false
}

func (v *Visitor) directExt(baseTy, extTy types.Type) bool {
	sym := v.env.Lookup(extTy.String())
	if sym == nil || sym.Kind() != scope.TYPE {
		return false
	}

	ext := sym.Type().(*Record)
	if !v.equalType(ext.base, baseTy) {
		return false
	}

	return true
}

// An actual parameter 'a' of type Ta is parameter compatible with a formal parameter f of type Tf if
//
// 1. Tf and Ta are equal types, or
// 2. f is a value parameter and Ta is assignment compatible with Tf, or
// 3. f is an IN or VAR parameter Ta must be the same type as Tf, or Tf must be a record type and Ta an extension of Tf.
func (v *Visitor) paramCompat(fpKind token.Token, Ta, Tf types.Type) bool {
	if v.equalType(Tf, Ta) {
		return true
	}

	if fpKind == token.ILLEGAL && v.assignCompat(Ta, Tf) {
		return true
	}

	if fpKind == token.VAR || fpKind == token.IN {
		if v.sameType(Ta, Tf) {
			return true
		}

		TfRec := Tf.(*Record)
		if TfRec != nil && v.recordTyExt(Tf, Ta) {
			return true
		}
	}

	return false
}

//	An actual parameter 'a' of type Ta is array compatible with a formal parameter f of type Tf if
//
// 1. Tf and Ta are the equal type, or
// 2. Tf is an open array, Ta is any array, and their element types are array compatible, or
// 3. Tf is an open array of CHAR and Ta is a Latin-1 string, or
// 4. Tf is an open array of WCHAR and Ta is a Unicode BMP or Latin-1 string, or
// 5. Tf is an open array of BYTE and Ta is a byte string.
func (v *Visitor) arrayCompat(a, b types.Type) bool { return false }

// Two formal parameter lists match if
//
// 1. they have the same number of parameters, and
// 2. parameters at corresponding positions have equal types, and
// 3. parameters at corresponding positions are both either value, VAR or IN parameters.
func (v *Visitor) paramListMatch(Ta, Tb *ast.FormalParams) bool {
	if len(Ta.Params) != len(Tb.Params) {
		return false
	}

	for i := 0; i < len(Ta.Params); i++ {
		if Ta.Params[i].Mod != Tb.Params[i].Mod {
			return false
		}

		if !v.equalType(Ta.Params[i].Type.Type(), Tb.Params[i].Type.Type()) {
			return false
		}
	}

	return true
}

func (v *Visitor) resultTypeMatch(a, b types.Type) bool {
	if !v.sameType(a, b) {
		return false
	}

	return true
}

func (v *Visitor) checkBuiltin(b *scope.Builtin, call *ast.ProcCall) {
	proc := scope.PredeclaredProcedures[b.Id]
	switch b.Id {
	case scope.Assert_:
		if proc.Nargs != len(call.ActualParams) {
			v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
		}

		for i := 0; i < proc.Nargs; i++ {
			if !v.assignCompat(scope.Typ[types.Bool], call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], scope.Typ[types.Bool])
				v.error(call.ActualParams[i].Pos(), msg)
			}
		}
	//case _Assertn:
	//case _Bytes:
	//case _Dec:
	//case _Decn:
	//case _Excl:
	//case _Halt:
	case scope.Inc_:
		//case _Incn:
		//case _Incl:
		//case _New:
		//case _Newn:
		//case _Number:
		//case _PCall:
		//case _Raise:
		//case _Copy:
		//case _Pack:
		//case _UnPk:
		//
		//case _Abs:
		//case _Cap:
		//case _BitAnd:
		//case _BitAsr:
		//case _BitNot:
		//case _BitOr:
		//case _Bits:
		//case _BitShl:
		//case _BitShr:
		//case _BitXor:
		//case _Cast:
		//case _Chr:
		//case _Default:
		//case _Floor:
		//case _Flt:
		//case _LdCmd:
		//case _LdMod:
		//case _Len:
		//case _LenN:
		//case _Long:
		//case _Max:
		//case _MaxN:
		//case _Min:
		//case _MinN:
		//case _Odd:
		//case _Ord:
		//case _Short:
		//case _Size:
		//case _StrLen:
		//case _WChr:
		//case _ASh:
		//case _ASr:
		//case _Entier:
		//case _Lsl:
		//case _Ror:
	}
}
