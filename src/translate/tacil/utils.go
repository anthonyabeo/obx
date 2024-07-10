package tacil

func GetNullValue(ty Type) Expr {
	var numBits uint

	switch ty.(type) {
	case *Int8:
		numBits = 8
	case *Int16:
		numBits = 16
	case *Int32:
		numBits = 32
	case *Int64:
		numBits = 64
	}

	return NewConstantInt(GetIntegerType(numBits), 0, false)
}

func GetAllOnesValue(ty Type) Expr {
	return nil
}
