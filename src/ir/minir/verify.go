package minir

import "fmt"

// VerifyError mirrors the verifier's findings.
type VerifyError struct {
	Func  string
	Block string
	Instr string
	Msg   string
}

func (e VerifyError) Error() string {
	switch {
	case e.Block == "" && e.Instr == "":
		return fmt.Sprintf("[%s] %s", e.Func, e.Msg)
	case e.Instr == "":
		return fmt.Sprintf("[%s/%s] %s", e.Func, e.Block, e.Msg)
	default:
		return fmt.Sprintf("[%s/%s] %q: %s", e.Func, e.Block, e.Instr, e.Msg)
	}
}

// VerifyIR checks structural invariants of the function and returns a slice
// of VerifyError describing any problems found. An empty slice means OK.
func VerifyIR(fn *Function) []VerifyError {
	var errs []VerifyError
	add := func(block, instr, msg string) {
		errs = append(errs, VerifyError{Func: fn.FnName, Block: block, Instr: instr, Msg: msg})
	}

	if fn.Entry == nil {
		add("", "", "Entry block is nil")
		return errs
	}
	if _, ok := fn.Blocks[fn.Entry.ID]; !ok {
		add("", "", fmt.Sprintf("Entry block %q not found in fn.Blocks", fn.Entry.Label))
	}
	if fn.Exit != nil {
		if _, ok := fn.Blocks[fn.Exit.ID]; !ok {
			add("", "", fmt.Sprintf("Exit block %q not found in fn.Blocks", fn.Exit.Label))
		}
	}

	for _, b := range fn.Blocks {
		bl := b.Label
		if b.Term == nil {
			add(bl, "", "block has no terminator (Term == nil)")
		}
		if len(b.Instrs) == 0 {
			add(bl, "", "block has no instructions")
		} else {
			last := b.Instrs[len(b.Instrs)-1]
			if b.Term != nil && b.Term != last {
				add(bl, last.String(), "Term field does not match last instruction in Instrs")
			}

			seenTerm := false
			for i, ins := range b.Instrs {
				if _, isT := ins.(Terminator); isT {
					if seenTerm {
						add(bl, ins.String(), "instruction appears after terminator")
					}
					if i != len(b.Instrs)-1 {
						add(bl, ins.String(), "terminator is not the last instruction")
					}
					seenTerm = true
				}
			}
			if !seenTerm && b.Term != nil {
				add(bl, "", "no Terminator-typed instruction found but Term is set")
			}
		}

		if b.Term != nil {
			checkLabel := func(label string) {
				if fn.GetBlock(label) == nil {
					add(bl, b.Term.String(), fmt.Sprintf("branch target %q does not exist", label))
				}
			}
			switch t := b.Term.(type) {
			case *JumpInst:
				checkLabel(t.Target)
			case *CondBrInst:
				checkLabel(t.TrueLabel)
				checkLabel(t.FalseLabel)
			case *SwitchInst:
				checkLabel(t.Default)
				for _, arm := range t.Arms {
					checkLabel(arm.Label)
				}
			}
		}

		for _, succ := range b.SortedSuccs() {
			if _, ok := succ.Preds[b.ID]; !ok {
				add(bl, "", fmt.Sprintf("succ %q does not list this block as a predecessor", succ.Label))
			}
		}
		for _, pred := range b.SortedPreds() {
			if _, ok := pred.Succs[b.ID]; !ok {
				add(bl, "", fmt.Sprintf("pred %q does not list this block as a successor", pred.Label))
			}
		}

		seenNonPhi := false
		for _, ins := range b.Instrs {
			if _, isPhi := ins.(*PhiInst); isPhi {
				if seenNonPhi {
					add(bl, ins.String(), "PhiInst appears after a non-phi instruction")
				}
			} else {
				seenNonPhi = true
			}
		}

		for _, ins := range b.Instrs {
			is := ins.String()
			for i, u := range ins.Uses() {
				if u == nil {
					add(bl, is, fmt.Sprintf("operand[%d] is nil", i))
				}
			}
			if d := ins.Def(); d != nil {
				if d.Type() == nil {
					add(bl, is, fmt.Sprintf("def %q has nil type", d.Name()))
				}
			}

			switch instr := ins.(type) {
			case *LoadInst:
				if instr.Addr != nil && !instr.Addr.IsMem() {
					add(bl, is, fmt.Sprintf("LoadInst addr %q is not a memory value", instr.Addr.Name()))
				}
			case *StoreInst:
				if instr.Addr != nil && !instr.Addr.IsMem() {
					add(bl, is, fmt.Sprintf("StoreInst addr %q is not a memory value", instr.Addr.Name()))
				}
			case *GEPInst:
				if instr.ElemType == nil {
					add(bl, is, "GEPInst has nil ElemType")
				}
				if instr.Base == nil {
					add(bl, is, "GEPInst base is nil")
				} else {
					// base should be an address-like Temp
					if bt, ok := instr.Base.(*Temp); ok {
						if !bt.IsMem() {
							add(bl, is, "GEPInst base Temp does not have IsAddr=true")
						}
					} else {
						add(bl, is, "GEPInst base is not a Temp value")
					}
				}
			case *PhiInst:
				for _, arg := range instr.Args {
					if arg.BlockLabel == "" {
						add(bl, is, "PhiInst arm has empty BlockLabel")
						continue
					}
					pred := fn.GetBlock(arg.BlockLabel)
					if pred == nil {
						add(bl, is, fmt.Sprintf("PhiInst arm block %q does not exist in function", arg.BlockLabel))
						continue
					}
					if _, ok := b.Preds[pred.ID]; !ok {
						add(bl, is, fmt.Sprintf("PhiInst arm block %q is not a predecessor of %q", pred.Label, bl))
					}
				}
			case *ReturnInst:
				if instr.Result != nil && fn.Result != nil {
					if !instr.Result.Type().Equal(fn.Result) {
						add(bl, is, fmt.Sprintf("ReturnInst result type %q does not match function result type %q", instr.Result.Type(), fn.Result))
					}
				}
				if instr.Result != nil && fn.Result == nil {
					add(bl, is, "ReturnInst returns a value but function result type is nil (void)")
				}
			case *BinaryInst:
				if instr.Left != nil && instr.Right != nil {
					if !instr.Left.Type().Equal(instr.Right.Type()) {
						add(bl, is, fmt.Sprintf("BinaryInst operand type mismatch: %q vs %q", instr.Left.Type(), instr.Right.Type()))
					}
				}
			case *ICmpInst:
				if instr.Left != nil && instr.Right != nil {
					if !instr.Left.Type().Equal(instr.Right.Type()) {
						add(bl, is, fmt.Sprintf("ICmpInst operand type mismatch: %q vs %q", instr.Left.Type(), instr.Right.Type()))
					}
				}
			case *FCmpInst:
				// FCmpInst embeds ICmpInst; compare types similarly
				base := &instr.ICmpInst
				if base.Left != nil && base.Right != nil {
					if !base.Left.Type().Equal(base.Right.Type()) {
						add(bl, is, fmt.Sprintf("FCmpInst operand type mismatch: %q vs %q", base.Left.Type(), base.Right.Type()))
					}
				}
			}
		}
	}
	return errs
}
