package target

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/asm"
)

// LocationKind indicates how a parameter is passed.
type LocationKind int

const (
	InRegister LocationKind = iota
	OnStack
)

// Location describes one parameter's ABI location.
type Location struct {
	Kind     LocationKind
	Register string // if InRegister
	Offset   int    // if OnStack: offset from call-time sp (for caller), or from fp (for callee)
	Size     int
	Align    int
}

type FrameObjKind int

const (
	RegSpillSlot FrameObjKind = iota
	ArgSpillSlot
	LocalVar
	SavedReg
	OutgoingArea
)

func alignUp(n, align int) int {
	if align == 0 {
		return n
	}
	return (n + align - 1) & ^(align - 1)
}

type FrameLayout struct {
	Params []*FrameObject
	Locals []*FrameObject
	Spills []*FrameObject
	Saves  []*FrameObject // callee-saved registers (s0–s11, ra, fp)

	FrameSize    int
	OutgoingSize int
}

func (fl *FrameLayout) GetParamObjectByName(name string) *FrameObject {
	for _, param := range fl.Params {
		if param.Name == name {
			return param
		}
	}

	return nil
}

func (fl *FrameLayout) GetSpillObjectByName(name string) *FrameObject {
	for _, spill := range fl.Spills {
		if spill.Name == name {
			return spill
		}
	}

	return nil
}

func (fl *FrameLayout) GetLocalObjectByName(name string) *FrameObject {
	for _, local := range fl.Locals {
		if local.Name == name {
			return local
		}
	}

	return nil
}

func (fl *FrameLayout) Output() {
	// Print layout summary
	fmt.Printf("Computed FrameSize = %d\n\n", fl.FrameSize)

	fmt.Println("Frame objects (fp-relative offsets):")
	for _, p := range fl.Params {
		if p.InRegister {
			fmt.Printf(" Param %-4s -> reg %s\n", p.Name, p.Reg)
		} else {
			fmt.Printf(" Param %-4s -> [fp + %d]\n", p.Name, p.Offset)
		}
	}
	for _, s := range fl.Saves {
		fmt.Printf(" Save  %-4s -> [fp %d]\n", s.Name, s.Offset)
	}
	for _, l := range fl.Locals {
		fmt.Printf(" Local %-4s -> [fp %d]\n", l.Name, l.Offset)
	}
	for _, sp := range fl.Spills {
		fmt.Printf(" Spill %-5s -> [fp %d]\n", sp.Name, sp.Offset)
	}
	fmt.Println()
}

type FrameObject struct {
	Name        string
	Kind        FrameObjKind
	Size, Align int
	Offset      int // offset from fp
	InRegister  bool
	Reg         string
}

func ComputeFrameLayout(fn *asm.Function, target Machine) FrameLayout {
	paramBase := 0 // positive offsets from fp for stack-passed args
	ParamLocs := target.AssignParams(len(fn.Params))

	layout := FrameLayout{}

	// Step 1: Parameters
	for i, loc := range ParamLocs {
		fo := &FrameObject{
			Name:  fn.Params[i].Name,
			Kind:  ArgSpillSlot,
			Size:  8,
			Align: 8,
		}
		if loc.Kind == InRegister {
			fo.InRegister = true
			fo.Reg = loc.Register
		} else {
			fo.Offset = paramBase + loc.Offset
		}
		layout.Params = append(layout.Params, fo)
	}

	// Step 2: Callee-saved registers (starting from fp - 8 downwards)
	offset := 0
	for _, reg := range fn.CalleeRegsUed {
		offset -= 8
		fo := &FrameObject{
			Name:   reg,
			Kind:   SavedReg,
			Size:   8,
			Align:  8,
			Offset: offset,
		}
		layout.Saves = append(layout.Saves, fo)
	}

	// Step 3: Locals (below saved regs)
	for _, local := range fn.Locals {
		offset -= 8
		fo := &FrameObject{
			Name:   local.Name,
			Kind:   LocalVar,
			Size:   8,
			Align:  8,
			Offset: offset,
		}
		layout.Locals = append(layout.Locals, fo)
	}

	// Step 4: Spills (below locals)
	for reg, s := range fn.Spills {
		offset -= s.Size
		fo := &FrameObject{
			Name:   reg,
			Kind:   RegSpillSlot,
			Size:   s.Size,
			Align:  s.Align,
			Offset: offset,
		}
		layout.Spills = append(layout.Spills, fo)
	}

	MaxArgRegs := target.RegisterInfo().MaxArgRegs
	WordSize := target.FrameInfo().WordSize

	// Outgoing call area (space for max call args beyond 8 regs)
	outgoingBytes := 0
	if len(fn.Params) > MaxArgRegs {
		outgoingBytes = (len(fn.Params) - MaxArgRegs) * WordSize
	}
	layout.OutgoingSize = outgoingBytes
	offset -= outgoingBytes

	// Step 5: Compute frame size and align to 16 bytes
	frameSize := -offset
	frameSize = alignUp(frameSize, target.FrameInfo().FrameAlign)
	layout.FrameSize = frameSize

	return layout
}
