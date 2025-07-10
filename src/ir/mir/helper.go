package mir

import (
	"bytes"
	"github.com/anthonyabeo/obx/src/ir/hir"
	"log"
)

func toMIRType(t hir.Type) Type {
	switch t := t.(type) {
	case *hir.IntType:
		return &IntegerType{Bits: t.Bits, Signed: t.Signed}
	default:
		return nil
	}
}

func getMIRText(prog *Program) string {
	var buf bytes.Buffer
	if err := WriteProgram(&buf, prog); err != nil {
		log.Fatal(err)
	}
	return buf.String()
}
