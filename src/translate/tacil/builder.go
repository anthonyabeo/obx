package tacil

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

func (b *Builder) CreateAdd(lhs, rhs Expr) *BinaryOp {
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

	add := NewBinaryOp(ResTy, Add, lhs, rhs)

	return add
}

func (b *Builder) CreateSub(lhs, rhs Expr) Expr {
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

	sub := NewBinaryOp(ResTy, Sub, lhs, rhs)

	return sub
}

func (b *Builder) CreateXOR(lhs, rhs Expr) Expr {
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

	xor := NewBinaryOp(ResTy, Xor, lhs, rhs)

	return xor
}

func (b *Builder) CreateRet(v Expr) *Return {
	var ret *Return
	if v != nil {
		ret = CreateRet(v)
	} else {
		ret = CreateRet(nil)
	}

	b.BB.instr.PushBack(ret)

	ret.parent = b.BB

	return ret
}

func (b *Builder) CreateCmp(pred Opcode, lhs, rhs Expr) Expr {
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

	name := NextTemp()
	tmp := NewTemp(name, ResTy)
	icmp := CreateCmp(pred, lhs, rhs)
	b.CreateAssign(icmp, tmp)

	return tmp
}

func (b *Builder) CreateFuncCall(callee Expr, args []Expr) *FuncCall {
	call := CreateFuncCall(callee, args)
	b.BB.instr.PushBack(call)

	return call
}

func (b *Builder) CreateProcCall(callee Expr, args []Expr) *ProcCallInstr {
	call := CreateProcCall(callee, args)
	b.BB.instr.PushBack(call)

	call.parent = b.BB

	return call
}

func (b *Builder) CreateAssign(Val, Dst Expr) *Assign {
	assign := CreateAssign(Val, Dst)
	b.BB.instr.PushBack(assign)

	assign.parent = b.BB

	return assign
}

func (b *Builder) CreateLoad(addr Expr) *Load {
	load := CreateLoad(addr)
	b.BB.instr.PushBack(load)

	return load
}

func (b *Builder) CreateCondBr(cond Expr, ifTrue, ifFalse *BasicBlock) *CondBr {
	br := CreateCondBr(cond, ifTrue, ifFalse)
	b.BB.instr.PushBack(br)

	b.CFG.AddSucc(b.BB.name, ifTrue, ifFalse)
	b.CFG.AddPred(ifTrue.name, b.BB)
	b.CFG.AddPred(ifFalse.name, b.BB)

	br.parent = b.BB

	return br
}

func (b *Builder) CreateJmp(dst *BasicBlock) *Jump {
	jmp := CreateJmp(dst)
	b.BB.instr.PushBack(jmp)

	b.CFG.AddSucc(b.BB.name, dst)
	b.CFG.AddPred(dst.name, b.BB)

	jmp.parent = b.BB

	return jmp
}

func (b *Builder) CreateNeg(v Expr) Expr {
	neg := b.CreateSub(GetNullValue(v.Type()), v)
	b.BB.instr.PushBack(neg)

	return neg
}

func (b *Builder) CreateNot(v Expr) Expr {
	not := b.CreateXOR(v, GetAllOnesValue(v.Type()))
	b.BB.instr.PushBack(not)

	return not
}
