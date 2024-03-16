package sema

import (
	"fmt"
	"strconv"

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
	if Ta == nil || Tb == nil {
		return false
	}

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
	if Ta == nil || Tb == nil {
		return false
	}

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

	TaPtr := Ta.(*types.PtrType)
	TbPtr := Tb.(*types.PtrType)
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
	if Te == nil && Tv == nil {
		return false
	}

	if v.sameType(Te, Tv) {
		return true
	}

	TeBasic, TeBasicOk := Te.(*types.Basic)
	TvBasic, TvBasicOk := Tv.(*types.Basic)
	if TeBasicOk && TvBasicOk {
		if TeBasic.Info()|types.IsNumeric == types.IsNumeric && TvBasic.Info()|types.IsNumeric == types.IsNumeric {
			return TvBasic.Kind() >= TeBasic.Kind()
		}

		if TvBasic.Info() == types.IsSet && TeBasic.Kind() < types.Int32 {
			return true
		}

		if TvBasic.Kind() == types.Byte && TeBasic.Kind() == types.Char {
			return true
		}
	}

	TeEnum, TeEnumOk := Te.(*types.Enum)
	if TeEnumOk && TvBasicOk {
		if TvBasic.Info() == types.IsInteger && TeEnum != nil {
			return true
		}
	}

	//TODO 6-12 Remain

	return false
}

// If Pa = POINTER TO Ta and Pb = POINTER TO Tb , Pb is an extension of Pa (Pa is a base type of Pb)
// if Tb is an extension of Ta.
func (v *Visitor) ptrExt(Ta, Tb *types.PtrType) bool {
	return true
}

// Given a type declaration Tb = RECORD(Ta)...END, Tb is a direct extension of Ta, and Ta is a direct base type of Tb.
// A type Tb is an extension of a type Ta (Ta is a base type of Tb) if:
//
// 1. Ta and Tb are the same types, or
// 2. Tb is a direct extension of Ta.
// 3. Ta is of type ANYREC.
func (v *Visitor) recordTyExt(baseTy, extTy types.Type) bool {
	if baseTy == nil || extTy == nil {
		return false
	}

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
	if baseTy == nil || extTy == nil {
		return false
	}

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
	if Tf == nil {
		return false
	}

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
	if Ta == nil || Tb == nil {
		return false
	}

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
	if a == nil || b == nil {
		return false
	}

	if !v.sameType(a, b) {
		return false
	}

	return true
}

func (v *Visitor) checkFuncBuiltin(b *scope.Builtin, call *ast.FuncCall) {
	proc := scope.PredeclaredProcedures[b.Id]

	if proc.Nargs != len(call.ActualParams) {
		v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
	}

	switch b.Id {
	case scope.Abs_:
	case scope.Cap_:
	case scope.BitAnd_:
	case scope.BitAsr_:
	case scope.BitNot_:
	case scope.BitOr_:
	case scope.Bits_:
	case scope.BitShl_:
	case scope.BitShr_:
	case scope.BitXor_:
	case scope.Cast_:
	case scope.Chr_:
	case scope.Default_:
	case scope.Floor_:
	case scope.Flt_:
	case scope.LdCmd_:
	case scope.LdMod_:
	case scope.Len_:
	case scope.LenN_:
	case scope.Long_:
	case scope.Max_:
	case scope.MaxN_:
	case scope.Min_:
	case scope.MinN_:
	case scope.Odd_:
	case scope.Ord_:
		obj := v.env.Lookup(call.ActualParams[0].String())
		if obj == nil || obj.Kind() != scope.CONST {
			msg := fmt.Sprintf("'%s' is not recognised as an enumeration variant", call.ActualParams[0].String())
			v.error(call.ActualParams[0].Pos(), msg)
		}

		bl, ok := obj.(*scope.Const).Value().(*ast.BasicLit)
		if !ok {
			msg := fmt.Sprintf("ordinal value of enum variant '%s' is not an integer", call.ActualParams[0].String())
			v.error(call.ActualParams[0].Pos(), msg)
		}

		i, err := strconv.Atoi(bl.Val)
		enum := call.ActualParams[0].Type().(*types.Enum)
		if err != nil || !enum.IsValidOrd(i) {
			msg := fmt.Sprintf("'%d' is not a valid ordinal value of the enum '%s'", i, enum)
			v.error(call.ActualParams[0].Pos(), msg)
		}

		call.EType = scope.Typ[types.Int]
	case scope.Short_:
	case scope.Size_:
	case scope.StrLen_:
	case scope.WChr_:
	case scope.ASh_:
	case scope.ASr_:
	case scope.Entier_:
	case scope.Lsl_:
	case scope.Ror_:
	}
}

func (v *Visitor) checkProcBuiltin(b *scope.Builtin, call *ast.ProcCall) {
	proc := scope.PredeclaredProcedures[b.Id]

	if proc.Nargs != len(call.ActualParams) {
		v.error(call.Pos(), fmt.Sprintf("not enough arguments to procedure call '%v'", call.String()))
	}

	switch b.Id {
	case scope.Assert_:
		for i := 0; i < proc.Nargs; i++ {
			if !v.assignCompat(scope.Typ[types.Bool], call.ActualParams[i].Type()) {
				msg := fmt.Sprintf("argument '%v' does not match the corresponding parameter type '%v'",
					call.ActualParams[i], scope.Typ[types.Bool])
				v.error(call.ActualParams[i].Pos(), msg)
			}
		}
	case scope.Assertn_:
	case scope.Bytes_:
	case scope.Dec_:
	case scope.Decn_:
	case scope.Excl_:
	case scope.Halt_:
	case scope.Inc_:
	case scope.Incn_:
	case scope.Incl_:
	case scope.New_:
	case scope.Newn_:
	case scope.Number_:
	case scope.PCall_:
	case scope.Raise_:
	case scope.Copy_:
	case scope.Pack_:
	case scope.UnPk_:
	}
}
