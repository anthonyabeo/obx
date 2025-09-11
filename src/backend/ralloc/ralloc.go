package ralloc

import "fmt"

type RegAlloc interface {
	NewVReg(class string) string // e.g., returns "v12" for GPR
}

type DemoRegAlloc struct {
	vReg int
}

func NewRegisterAllocator() RegAlloc {
	return &DemoRegAlloc{vReg: 0}
}

func (r *DemoRegAlloc) NewVReg(class string) string {
	r.vReg++
	return fmt.Sprintf("v%d", r.vReg)
}
