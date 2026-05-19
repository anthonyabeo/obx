package minir

// Temp ID generator for NewTemp/NewAnonTemp
var tempIDCounter int

func nextTempID() int {
	tempIDCounter++
	return tempIDCounter
}

// ResetTempCounter resets the package-level temp ID counter to zero.
// Exported so that external test packages (e.g. miniropt_test) can produce
// deterministic temp IDs without being inside the minir package.
func ResetTempCounter() { tempIDCounter = 0 }

// NewTemp creates a Temp with the given name and (canonicalized) type.
// The temp is assigned a fresh unique ID.
func NewTemp(name string, ty Type) *Temp {
	return &Temp{ID: nextTempID(), NameStr: name, Ty: NormalizeType(ty)}
}

// NewAnonTemp creates an anonymous Temp (no NameStr) with a fresh ID.
func NewAnonTemp(ty Type) *Temp {
	return &Temp{ID: nextTempID(), Ty: NormalizeType(ty)}
}
