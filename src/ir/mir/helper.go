package mir

import (
	"bytes"
	"log"

	"github.com/anthonyabeo/obx/src/types"
)

func (g *Generator) genMIRType(t types.Type) Type {
	switch t := t.(type) {
	case *types.BasicType:
		switch t.Kind {
		case types.BOOLEAN:
			return Int1Type
		case types.BYTE:
			return UInt8Type
		case types.INT32:
			return Int32Type
		case types.INT16:
			return Int16Type
		case types.INT8:
			return Int8Type
		case types.INT64:
			return Int64Type
		case types.REAL:
			return Float32Type
		case types.LONGREAL:
			return Float64Type
		}
	default:
		return nil
	}

	return nil
}

func getMIRText(prog *Program) string {
	var buf bytes.Buffer
	if err := WriteProgram(&buf, prog); err != nil {
		log.Fatal(err)
	}
	return buf.String()
}
