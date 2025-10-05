package target

import (
	"fmt"
	"sort"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

type FrameObjKind int

const (
	SpillSlot FrameObjKind = iota
	LocalVar
	SavedReg
	OutgoingArea
)

type FrameObject struct {
	Name   string // e.g. "spill0", "s0", "outgoing", "local1"
	Size   int
	Align  int
	Offset int // computed
	Kind   FrameObjKind
}

type FrameLayout struct {
	Objects   []FrameObject
	FrameSize int
	MaxAlign  int
}

// AnalyzeFrame scans the function’s metadata to build the list of frame objects
// needed for stack frame construction.
func AnalyzeFrame(fn *asm.Function, target Machine) []FrameObject {
	var objs []FrameObject

	// --- 1. Spills ---
	for _, spill := range fn.Spills {
		//size, align := SizeAndAlignOf(spill.Type)
		objs = append(objs, FrameObject{
			Name:  fmt.Sprintf("spill%d", spill.Index),
			Kind:  SpillSlot,
			Size:  spill.Size,
			Align: spill.Align,
		})
	}

	// --- 2. Locals ---
	for _, local := range fn.Locals {
		size, align := SizeAndAlignOf(local.Type)
		objs = append(objs, FrameObject{
			Name:  local.Name,
			Kind:  LocalVar,
			Size:  size,
			Align: align,
		})
	}

	// --- 3. Callee-saved registers ---
	for reg := range fn.CalleeRegsUed {
		objs = append(objs, FrameObject{
			Name:  reg,
			Kind:  SavedReg,
			Size:  target.FrameInfo().WordSize, // RISC-V LP64D: all GPRs and FP regs are 8 bytes
			Align: target.FrameInfo().WordSize,
		})
	}

	// --- 4. Outgoing call area (if needed) ---
	if fn.HasCalls {
		objs = append(objs, FrameObject{
			Name:  "outgoing_args",
			Kind:  OutgoingArea,
			Size:  target.FrameInfo().WordSize * 8, // RISC-V ABI: reserve space for 8 arguments
			Align: target.FrameInfo().FrameAlign,
		})
	}

	return objs
}

func ComputeFrameLayoutFPBased(objs []FrameObject, target Machine) FrameLayout {
	// Sort in logical order: locals/spills first, then saved regs,
	// then outgoing area (if any).
	sort.SliceStable(objs, func(i, j int) bool {
		order := func(k FrameObjKind) int {
			switch k {
			case SpillSlot, LocalVar:
				return 0
			case SavedReg:
				return 1
			case OutgoingArea:
				return 2
			default:
				return 3
			}
		}
		return order(objs[i].Kind) < order(objs[j].Kind)
	})

	offset := 0
	maxAlign := target.FrameInfo().FrameAlign // ABI minimum alignment

	for i := range objs {
		obj := &objs[i]
		offset = alignUp(offset, obj.Align)
		obj.Offset = -offset - obj.Size // negative offset from fp
		offset += obj.Size
		if obj.Align > maxAlign {
			maxAlign = obj.Align
		}
	}

	frameSize := alignUp(offset, target.FrameInfo().FrameAlign)

	return FrameLayout{
		Objects:   objs,
		FrameSize: frameSize,
		MaxAlign:  maxAlign,
	}
}

func alignUp(n, align int) int {
	if align == 0 {
		return n
	}
	return (n + align - 1) & ^(align - 1)
}

func SizeAndAlignOf(t asm.Type) (int, int) {
	switch t {
	case asm.I16, asm.U16:
		return 2, 2
	case asm.I8, asm.U8:
		return 1, 1
	case asm.I32, asm.U32, asm.F32:
		return 4, 4
	case asm.I64, asm.U64, asm.Ptr, asm.F64:
		return 8, 8
	default:
		panic("unknown type in SizeAndAlignOf")
	}
}
