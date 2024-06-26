package ir

import "fmt"

type Builder struct {
	BB  *BasicBlock       // the current BasicBlock where instructions are inserted
	CFG *ControlFlowGraph // A graph modelling control-flow for the current function
}

func NewBuilder() *Builder {
	return &Builder{}
}

// GetInsertBlock returns the current BasicBlock where instructions are inserted
func (b *Builder) GetInsertBlock() *BasicBlock { return b.BB }

// SetInsertPoint updates the current BasicBlock to BB
func (b *Builder) SetInsertPoint(BB *BasicBlock) { b.BB = BB }

func (b *Builder) CreateAdd(lhs, rhs Value, name string) Value {
	var (
		ResTy Type
		LHSTy Type = lhs.Type()
		RHSTy Type = rhs.Type()
	)

	// lhs != integer-type && lhs != pointer-type && lhs != vector<integer-type>
	if !LHSTy.IsIntegerTy() && !LHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expcted add operand '%s' to be an integer or pointer type. Got '%s'", lhs, LHSTy)
		panic(msg)
	}

	// rhs != integer-type && rhs != pointer-type && rhs != vector<integer-type>
	if !RHSTy.IsIntegerTy() && !RHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expected add operand '%s' to be an integer or pointer type. Got '%s'", rhs, RHSTy)
		panic(msg)
	}

	if (LHSTy.IsPtrTy() && RHSTy.IsPtrTy()) || (LHSTy.IsIntegerTy() && RHSTy.IsIntegerTy()) {
		if LHSTy.IsIntegerTy() {
			ResTy = LHSTy
		} else {
			ResTy = LHSTy.(*PointerType).elemTy
		}
	} else {
		if LHSTy.IsPtrTy() {
			ptr, _ := LHSTy.(*PointerType)
			if ptr.elemTy != RHSTy {
				msg := fmt.Sprintf("[internal] cannot add operands of type '%s' and '%s'", ptr.elemTy, RHSTy)
				panic(msg)
			}

			ResTy = RHSTy
		}

		if RHSTy.IsPtrTy() {
			ptr, _ := RHSTy.(*PointerType)
			if ptr.elemTy != LHSTy {
				msg := fmt.Sprintf("[internal] cannot add operands of type '%s' and '%s'", ptr.elemTy, LHSTy)
				panic(msg)
			}

			ResTy = LHSTy
		}
	}

	add := CreateAdd(ResTy, lhs, rhs, name)
	lhs.AddUse(add)
	rhs.AddUse(add)
	b.BB.instr.PushBack(add)

	return add
}

func (b *Builder) CreateSub(lhs, rhs Value, name string) Value {
	var (
		ResTy Type
		LHSTy Type = lhs.Type()
		RHSTy Type = rhs.Type()
	)

	// lhs != integer-type && lhs != pointer-type && lhs != vector<integer-type>
	if !LHSTy.IsIntegerTy() && !LHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expected operand '%s' to be an integer or pointer type. Got '%s'", lhs, LHSTy)
		panic(msg)
	}

	// rhs != integer-type && rhs != pointer-type && rhs != vector<integer-type>
	if !RHSTy.IsIntegerTy() && !RHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expected operand '%s' to be an integer or pointer type. Got '%s'", rhs, RHSTy)
		panic(msg)
	}

	if (LHSTy.IsPtrTy() && RHSTy.IsPtrTy()) || (LHSTy.IsIntegerTy() && RHSTy.IsIntegerTy()) {
		if LHSTy.IsIntegerTy() {
			ResTy = LHSTy
		} else {
			ResTy = LHSTy.(*PointerType).elemTy
		}
	} else {
		if LHSTy.IsPtrTy() {
			ptr, _ := LHSTy.(*PointerType)
			if ptr.elemTy != RHSTy {
				msg := fmt.Sprintf("[internal] cannot subtract operands of type '%s' and '%s'", ptr.elemTy, RHSTy)
				panic(msg)
			}

			ResTy = RHSTy
		}

		if RHSTy.IsPtrTy() {
			ptr, _ := RHSTy.(*PointerType)
			if ptr.elemTy != LHSTy {
				msg := fmt.Sprintf("[internal] cannot subtract operands of type '%s' and '%s'", ptr.elemTy, LHSTy)
				panic(msg)
			}

			ResTy = LHSTy
		}
	}

	sub := CreateSub(ResTy, lhs, rhs, name)
	lhs.AddUse(sub)
	rhs.AddUse(sub)
	b.BB.instr.PushBack(sub)

	return sub
}

func (b *Builder) CreateXOR(lhs, rhs Value, name string) Value {
	var (
		ResTy Type
		LHSTy Type = lhs.Type()
		RHSTy Type = rhs.Type()
	)

	// lhs != integer-type && lhs != vector<integer-type>
	if !LHSTy.IsIntegerTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expected operand '%s' to be an integer or integer vector type. Got '%s'",
			lhs, LHSTy)
		panic(msg)
	}

	// rhs != integer-type && rhs != vector<integer-type>
	if !RHSTy.IsIntegerTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] expected operand '%s' to be an integer or pointer type. Got '%s'", rhs, RHSTy)
		panic(msg)
	}

	ResTy = LHSTy

	xor := CreateXOR(ResTy, lhs, rhs, name)
	lhs.AddUse(xor)
	rhs.AddUse(xor)
	b.BB.instr.PushBack(xor)

	return xor
}

func (b *Builder) CreateRet(v Value) *ReturnInst {
	var ret *ReturnInst
	if v != nil {
		ret = CreateRet(v.Type(), v)
		v.AddUse(ret)
	} else {
		ret = CreateRet(VoidType, nil)
	}

	b.BB.instr.PushBack(ret)

	return ret
}

func (b *Builder) CreateRetVoid() *ReturnInst {
	ret := CreateRetVoid()
	b.BB.instr.PushBack(ret)

	return ret
}

func (b *Builder) CreateCmp(pred Opcode, lhs, rhs Value, name string) Value {
	var (
		ResTy Type
		LHSTy Type = lhs.Type()
		RHSTy Type = rhs.Type()
	)

	// lhs != integer-type && lhs != pointer-type && lhs != vector<integer-type>
	if !LHSTy.IsIntegerTy() && !LHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] icmp operand '%s' to be an integer or pointer type. Got '%s'", lhs, LHSTy)
		panic(msg)
	}

	// rhs != integer-type && rhs != pointer-type && rhs != vector<integer-type>
	if !RHSTy.IsIntegerTy() && !RHSTy.IsPtrTy() /* && !LHS.IsVecTy() */ {
		msg := fmt.Sprintf("[internal] icmp operand '%s' to be an integer or pointer type. Got '%s'", rhs, RHSTy)
		panic(msg)
	}

	if (LHSTy.IsPtrTy() && RHSTy.IsPtrTy()) || (LHSTy.IsIntegerTy() && RHSTy.IsIntegerTy()) {
		ResTy = LHSTy
	} else {
		// lhs != rhs (meaning one is an integer and the other is a pointer)
		// if the ptr.elemTy != integer-type, error
		// otherwise, we use an integer-type
		if LHSTy.IsPtrTy() {
			ptr, _ := LHSTy.(*PointerType)
			if ptr.elemTy != RHSTy {
				msg := "[internal] cannot use icmp operation on pointer and integer types"
				panic(msg)
			}

			ResTy = RHSTy
		}

		if RHSTy.IsPtrTy() {
			ptr, _ := RHSTy.(*PointerType)
			if ptr.elemTy != LHSTy {
				msg := "[internal] cannot use icmp operation on pointer and integer types"
				panic(msg)
			}

			ResTy = LHSTy
		}
	}

	icmp := CreateICmp(ResTy, pred, lhs, rhs, name)
	lhs.AddUse(icmp)
	rhs.AddUse(icmp)
	b.BB.instr.PushBack(icmp)

	return icmp
}

func (b *Builder) CreateAlloca(ty Type, name string) *AllocaInst {
	alloc := CreateAlloca(ty, 1, 0, name)
	b.BB.instr.PushBack(alloc)

	return alloc
}

func (b *Builder) CreateCall(fty *FunctionType, callee Value, args []Value, name string) *CallInstr {
	call := CreateCall(fty, callee, args, name)
	for _, argV := range args {
		argV.AddUse(call)
	}
	b.BB.instr.PushBack(call)

	return call
}

func (b *Builder) CreateStore(Val, Dst Value) *StoreInst {
	store := CreateStore(Val, Dst)
	Val.AddUse(store)
	Dst.AddUse(store)
	b.BB.instr.PushBack(store)

	return store
}

func (b *Builder) CreateLoad(ty Type, ptr Value, name string) *LoadInst {
	load := CreateLoad(ty, ptr, name)
	ptr.AddUse(load)
	b.BB.instr.PushBack(load)

	return load
}

func (b *Builder) CreateCondBr(cond Value, ifTrue, ifFalse *BasicBlock) *BranchInst {
	br := CreateCondBrInst(cond, ifTrue, ifFalse)
	cond.AddUse(br)
	b.BB.instr.PushBack(br)

	b.CFG.AddSucc(b.BB.name, ifTrue, ifFalse)
	b.CFG.AddPred(ifTrue.name, b.BB)
	b.CFG.AddPred(ifFalse.name, b.BB)

	return br
}

func (b *Builder) CreateBr(dst *BasicBlock) *BranchInst {
	br := CreateBr(dst)
	b.BB.instr.PushBack(br)

	b.CFG.AddSucc(b.BB.name, dst)
	b.CFG.AddPred(dst.name, b.BB)

	return br
}

func (b *Builder) CreatePHI(ty Type, numIncomingValues uint, name string) *PHINode {
	phi := CreatePHINode(ty, numIncomingValues, name)
	b.BB.instr.PushBack(phi)

	return phi
}

func (b *Builder) CreateNeg(v Value, name string) Value {
	neg := b.CreateSub(GetNullValue(v.Type()), v, name)
	b.BB.instr.PushBack(neg)

	return neg
}

func (b *Builder) CreateNot(v Value, name string) Value {
	not := b.CreateXOR(v, GetAllOnesValue(v.Type()), name)
	v.AddUse(not)
	b.BB.instr.PushBack(not)

	return not
}
