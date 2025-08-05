package lir

//import (
//	"fmt"
//	"io"
//)
//
//func FormatProgram(w io.Writer, prog *Program, indent string) error {
//	for _, mod := range prog.Modules {
//		if err := FormatModule(w, mod, indent); err != nil {
//			return err
//		}
//		fmt.Fprintln(w)
//	}
//	return nil
//}
//
//// FormatModule pretty-prints a LIR Module to the writer.
//func FormatModule(w io.Writer, m *Module, indent string) error {
//	fmt.Fprintf(w, "module %s {\n", m.Name)
//
//	// Print global variables (if any)
//	if len(m.Globals) > 0 {
//		fmt.Fprintln(w, "  globals:")
//		for _, g := range m.Globals {
//			if g.Init != nil {
//				fmt.Fprintf(w, "    VAR %s: %s = %s\n", g.Name, g.Type.String(), g.Init.String())
//			} else {
//				fmt.Fprintf(w, "    VAR %s: %s\n", g.Name, g.Type.String())
//			}
//		}
//		fmt.Fprintln(w)
//	}
//
//	// Print all procedures
//	for _, fn := range m.Func {
//		fmt.Fprintln(w)
//		if err := FormatFunction(w, fn, indent); err != nil {
//			return err
//		}
//	}
//
//	fmt.Fprintln(w, "\n}")
//
//	return nil
//}
//
//func FormatFunction(w io.Writer, fn *Function, indent string) error {
//	fmt.Fprintf(w, "func %s:\n%s", fn.Name, indent)
//
//	// Print return type if it exists
//	if fn.Ret != nil {
//		fmt.Fprintf(w, " -> %s", fn.Ret.String())
//		fmt.Fprintln(w, "\n"+indent)
//
//	}
//
//	ret := ""
//	if p.Ret != nil {
//		ret = fmt.Sprintf(" -> %s", p.Ret.String())
//	}
//
//	fmt.Fprintf(w, "%sproc %s(%s)%s\n", exportPrefix, p.Name, strings.Join(paramList, ", "), ret)
//
//	// Format locals
//	if len(p.Locals) > 0 {
//		fmt.Fprintf(w, "  locals:")
//		for _, local := range p.Locals {
//			fmt.Fprintf(w, " %s: %s", local.Name, local.Type)
//		}
//		fmt.Fprintln(w)
//	}
//
//	// Format blocks
//	for _, block := range p.Blocks {
//		if err := FormatBlock(w, block); err != nil {
//			return err
//		}
//		fmt.Fprintf(w, "\n%s", indent)
//	}
//
//	return nil
//}
//
//func FormatInstr(w io.Writer, instr Inst, indent string) error {
//	switch v := instr.(type) {
//	case *MoveInst:
//		fmt.Fprintf(w, "%s = %s", v.Dst, v.Src)
//	case *StoreInst:
//		fmt.Fprintf(w, "store %s, %s", v.Src, v.Dst)
//	case *LoadInst:
//		fmt.Fprintf(w, "%s = load %s", v.Dst, v.Src)
//	case *LabelInst:
//		fmt.Fprintf(w, "\n%s:", v.Label.Name)
//	case *CondBrInst:
//		fmt.Fprintf(w, "br %s, label %s, label %s", v.Cond, v.IfTrue, v.IfFalse)
//	case *JmpInst:
//		fmt.Fprintf(w, "jmp %s", v.Dst.Name)
//	case *RetInst:
//		if v.Value != nil {
//			fmt.Fprintf(w, "ret %s", v.Value)
//		} else {
//			fmt.Fprint(w, "ret")
//		}
//	case *AllocInst:
//		fmt.Fprintf(w, "alloc %s, %s", v.StartAddr, v.Size)
//	case *HaltInst:
//		if v.Code.Value != 0 {
//			fmt.Fprintf(w, "halt %s", v.Code)
//		} else {
//			fmt.Fprint(w, "halt")
//		}
//	default:
//		return fmt.Errorf("unknown instruction type: %T", instr)
//	}
//	return nil
//}
