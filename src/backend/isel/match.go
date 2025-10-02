package isel

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/obx/src/backend/isel/bud"
	"github.com/anthonyabeo/obx/src/backend/isel/bud/ast"
	"github.com/anthonyabeo/obx/src/ir/asm"
)

var temp int

type MatchResult struct {
	Rule    *ast.Rule
	Pattern *bud.Node
	Bind    map[string]*bud.Value
}

func (m *MatchResult) Binding(env map[string]*bud.Value) {
	if m.Pattern.Dst != nil {
		rd := m.Rule.Out.(*ast.Register)

		kind := bud.KindGPR
		if rd.Type == "FPR" {
			kind = bud.KindFPR
		}

		env[rd.Name] = &bud.Value{
			Kind: kind,
			Reg: bud.Reg{
				Name: m.Pattern.Dst.Val.Reg.Name,
				Mode: rd.Mode,
				Kind: rd.Type,
			},
		}
	}

	for _, v := range m.Rule.Temps {
		t := v.(*ast.Register)

		env[t.Name] = &bud.Value{
			Kind: bud.KindGPR,
			Reg: bud.Reg{
				Name: fmt.Sprintf("temp_%d", temp),
				Mode: t.Mode,
				Kind: t.Type,
			},
		}
		temp++
	}
}

func (m *MatchResult) Emit() []*asm.Instr {
	out := make([]*asm.Instr, 0, len(m.Rule.Instructions))
	for _, instr := range m.Rule.Instructions {
		out = append(out, Subst(instr, m.Bind))
	}
	return out
}

// env binds "$name" -> Value captured from the IR during matching.
// classes is the union of rule.In, rule.Out, rule.Temps (name -> class).
func match(pt *ast.Pattern, ir *bud.Node, env map[string]*bud.Value, classes map[string]ast.Operand /*classes map[string]bud.Var*/) bool {
	if pt == nil || ir == nil {
		return false
	}

	// variable leaf?
	if len(pt.Var) > 0 {
		cls := classes[pt.Var] // "GPR" or "imm"

		// Check leaf node type
		if ir.Val == nil {
			return false
		}

		switch cls.Kind() {
		case ast.OKRegister:
			if ir.Val.Kind != bud.KindGPR && ir.Val.Kind != bud.KindFPR {
				return false
			}

			reg := cls.(*ast.Register)
			ir.Val.Reg.Mode = reg.Mode
			ir.Val.Reg.Kind = reg.Type
		case ast.OKImm:
			if ir.Val.Kind != bud.KindImm {
				return false
			}
		case ast.OKLabel:
			if ir.Val.Kind != bud.KindLabel {
				return false
			}
		case ast.OKReloc:
			if ir.Val.Kind != bud.KindReloc {
				return false
			}

			reloc := cls.(*ast.Reloc)
			ir.Val.Reloc.Fxn = reloc.Fxn
			ir.Val.Reloc.Symbol = reloc.Symbol
		case ast.OKMem:
			if ir.Val.Kind != bud.KindMem {
				return false
			}

			mem := cls.(*ast.Mem)
			ir.Val.Mem.Base = bud.Reg{
				Name: mem.Base.Name,
				Mode: mem.Base.Mode,
				Kind: mem.Base.Type,
			}

			if mem.Offs.Kind() == ast.OKImm {
				offs := mem.Offs.(*ast.Imm)
				ir.Val.Mem.Offs = offs.Value.(int)
			} else {
				offs := mem.Offs.(*ast.Reloc)
				ir.Val.Mem.Reloc = bud.Reloc{
					Fxn:    offs.Fxn,
					Symbol: offs.Symbol,
				}
			}
		case ast.OKGlobal:
			if ir.Val.Kind != bud.KindGlobal {
				return false
			}
		default:
			return false
		}

		// Bind
		env[pt.Var] = ir.Val
		return true
	}

	// Operator node
	if ir.Op != pt.Op || len(ir.Args) != len(pt.Args) {
		return false
	}

	for i := range pt.Args {
		if !match(pt.Args[i], ir.Args[i], env, classes) {
			return false
		}
	}
	return true
}

// //////////////////////////////////////////////////////////////////////////////
// Subst replaces the variables in an instruction string with their respective
// values in env
// //////////////////////////////////////////////////////////////////////////////

func Subst(inst ast.Instr, env map[string]*bud.Value) *asm.Instr {
	var (
		asmOperands []asm.Operand
		def         *asm.Register
		uses        []*asm.Register
	)

	for _, operand := range inst.Operands {
		switch op := operand.(type) {
		case *ast.Register:
			if op.Mode == "phys" {
				asmOperands = append(asmOperands, &asm.Register{
					Name: op.Name,
					Mode: RegMode(op.Mode),
					Kind: RegKind(op.Type),
				})

			} else {
				if v, ok := env[op.Name]; ok && v.Kind == bud.KindGPR || v.Kind == bud.KindFPR {
					asmOperands = append(asmOperands, &asm.Register{
						Name: v.Reg.Name,
						Mode: RegMode(op.Mode),
						Kind: RegKind(op.Type),
					})
				}
			}
		case *ast.Reloc:
			if v, ok := env[op.Symbol]; ok {
				var symbol string
				if v.Kind == bud.KindImm {
					symbol = strconv.Itoa(v.Imm)
				} else if v.Kind == bud.KindLabel {
					symbol = v.Label
				} else if v.Kind == bud.KindGlobal {
					symbol = v.Global.Name
				}

				asmOperands = append(asmOperands, &asm.RelocFunc{
					Kind:   RelocFxn(op.Fxn),
					Symbol: symbol,
				})

			}
		case *ast.Imm:
			if imm, ok := env[op.Name]; ok && imm.Kind == bud.KindImm {
				op.Value = imm.Imm
			}

			asmOperands = append(asmOperands, &asm.Imm{
				Value: op.Value,
			})
		case *ast.Label:
			if v, ok := env[op.Name]; ok {
				asmOperands = append(asmOperands, &asm.Label{Name: v.Label})
			}
		case *ast.Mem:
			var offset asm.Operand

			switch op.Offs.Kind() {
			case ast.OKImm:
				imm := op.Offs.(*ast.Imm)
				offset = &asm.Imm{Value: imm.Value}
			case ast.OKReloc:
				var symbol string
				rel := op.Offs.(*ast.Reloc)
				if v, ok := env[rel.Symbol]; ok {

					if v.Kind == bud.KindImm {
						symbol = strconv.Itoa(v.Imm)
					} else if v.Kind == bud.KindLabel {
						symbol = v.Label
					}
				}

				offset = &asm.RelocFunc{
					Kind:   RelocFxn(rel.Fxn),
					Symbol: symbol,
				}
			default:
				panic("unreachable")
			}

			asmOperands = append(asmOperands, &asm.MemAddr{
				Base: &asm.Register{
					Name: op.Base.Name,
					Mode: RegMode(op.Base.Mode),
					Kind: RegKind(op.Base.Type),
				},
				Offset: offset,
			})
		case *ast.Global:
			if v, ok := env[op.Name]; ok && v.Kind == bud.KindGlobal {
				asmOperands = append(asmOperands, &asm.Global{
					Name: v.Global.Name,
					Size: v.Global.Size,
				})
			}
		default:
			panic(fmt.Sprintf("invalid operand: %v", operand))
		}
	}

	// Destination register
	if inst.Def != nil && inst.Def.Mode == "virt" {
		if v, ok := env[inst.Def.Name]; ok && v.Kind == bud.KindGPR || v.Kind == bud.KindFPR {
			def = &asm.Register{
				Name: v.Reg.Name,
				Mode: RegMode(inst.Def.Mode),
				Kind: RegKind(inst.Def.Type),
			}
		}
	}

	// Source registers
	for _, use := range inst.Uses {
		if use.Mode == "phys" {
			continue
		}

		if v, ok := env[use.Name]; ok && (v.Kind == bud.KindGPR || v.Kind == bud.KindFPR) {
			uses = append(uses, &asm.Register{
				Name: v.Reg.Name,
				Mode: RegMode(use.Mode),
				Kind: RegKind(use.Type),
			})
		}
	}

	return &asm.Instr{
		Opcode:   inst.Opcode,
		Operands: asmOperands,
		Def:      def,
		Uses:     uses,
	}
}

func RegMode(mode string) asm.RegMode {
	switch mode {
	case "virt":
		return asm.Virt
	case "phys":
		return asm.Phys
	default:
		panic("invalid register kind")
	}
}

func RegKind(kind string) asm.RegKind {
	switch kind {
	case "GPR":
		return asm.GPR
	case "FPR":
		return asm.FPR
	case "SPR":
		return asm.SPR
	case "VEC":
		return asm.VEC
	default:
		panic("invalid register kind")
	}
}

func RelocFxn(kind string) asm.RelocKind {
	switch kind {
	case "hi":
		return asm.Hi
	case "lo":
		return asm.Lo
	default:
		panic("invalid reloc kind")
	}
}
