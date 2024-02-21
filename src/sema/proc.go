package sema

import "github.com/anthonyabeo/obx/src/sema/types"

type ProcedureType struct {
}

func (p ProcedureType) String() string { panic("implement me") }

func (p ProcedureType) Underlying() types.Type { panic("implement me") }

func (p ProcedureType) Width() int { panic("implement me") }
