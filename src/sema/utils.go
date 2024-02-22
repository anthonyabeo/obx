package sema

import (
	"github.com/anthonyabeo/obx/src/sema/types"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

func (v *Visitor) match(a, b types.Type) bool { return false }

// Two variables a and b with types Ta and Tb are of the same type if
//
//  1. Ta and Tb are both denoted by the same type identifier, or
//  2. Ta is declared to equal Tb in a type declaration of the form Ta = Tb, or
//  3. a and b appear in the same identifier list in a variable, record field,
//     or formal parameter declaration and are not open arrays.
func (v *Visitor) sameType(a, b types.Type) bool { return false }

// Two types Ta and Tb are equal if
//
// 1. Ta and Tb are the same type, or
// 2. Ta and Tb are open array types with equal element types, or
// 3. Ta and Tb are procedure types whose formal parameters match, or
// 4. Ta and Tb are pointer types with equal base types.
func (v *Visitor) equalType(a, b types.Type) bool { return false }

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
func (v *Visitor) assignCompat(left, right types.Type) bool {
	if left.String() == right.String() {
		return true
	}

	l, _ := left.(*types.Basic)
	r, _ := right.(*types.Basic)
	if l.Info() == types.IsNumeric && r.Info() == types.IsNumeric {
		return l.Kind() >= r.Kind()
	}

	return false
}

func (v *Visitor) extends(a, b types.Type) bool { return false }

// An actual parameter a of type Ta is parameter compatible with a formal parameter f of type Tf if
//
// 1. Tf and Ta are equal types, or
// 2. f is a value parameter and Ta is assignment compatible with Tf, or
// 3. f is an IN or VAR parameter Ta must be the same type as Tf, or Tf must be a record type and Ta an extension of Tf.
func (v *Visitor) paramCompat(a, b types.Type) bool { return false }

//	An actual parameter a of type Ta is array compatible with a formal parameter f of type Tf if
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
func (v *Visitor) paramListMatch(a, b *ast.FormalParams) bool { return false }
