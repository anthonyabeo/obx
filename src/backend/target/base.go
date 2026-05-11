package target

import "github.com/anthonyabeo/obx/src/backend/mir"

// Target is the common interface shared by all backend targets in the new
// minir-first pipeline.
type Target interface {
	Name() string
	ABIInfo() ABI
	SupportsIntegerScalar(*mir.Type) bool
	LowerPhiBlock(joinLabel string, phis []*mir.PhiInstr) (*PhiPlan, error)
	LowerSwitch(sw *mir.SwitchInstr) (*SwitchPlan, error)
	LowerCall(call *mir.CallInstr) (*CallPlan, error)
}

// BaseTarget carries the shared target identity and ABI metadata.
type BaseTarget struct {
	ID  string
	ABI ABI
}

func NewBaseTarget(id string, abi ABI) *BaseTarget {
	return &BaseTarget{ID: id, ABI: abi}
}

func (b *BaseTarget) Name() string {
	if b == nil {
		return ""
	}
	return b.ID
}

func (b *BaseTarget) ABIInfo() ABI {
	if b == nil {
		return ABI{}
	}
	return b.ABI
}

func (b *BaseTarget) SupportsIntegerScalar(ty *mir.Type) bool {
	return SupportsIntegerScalar(ty)
}

func (b *BaseTarget) LowerPhiBlock(joinLabel string, phis []*mir.PhiInstr) (*PhiPlan, error) {
	return BuildPhiPlan(joinLabel, phis)
}

func (b *BaseTarget) LowerSwitch(sw *mir.SwitchInstr) (*SwitchPlan, error) {
	if b == nil {
		return nil, nil
	}
	return BuildSwitchPlan(sw, b.ABI)
}

func (b *BaseTarget) LowerCall(call *mir.CallInstr) (*CallPlan, error) {
	if b == nil {
		return nil, nil
	}
	return BuildCallPlan(call, b.ABI)
}
