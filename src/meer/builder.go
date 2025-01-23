package meer

import "fmt"

type Builder struct {
	label        *Label
	instructions []Instruction
}

func NewBuilder() *Builder {
	return &Builder{}
}

func (b *Builder) Instr() []Instruction { return b.instructions }

func (b *Builder) GetInsertPoint() *Label { return b.label }

// SetInsertPoint updates the current BasicBlock to BB
func (b *Builder) SetInsertPoint(label *Label) { b.instructions = append(b.instructions, label) }

func (b *Builder) CreateAdd(lhs, rhs Expression) *BinaryOp {
	var (
		ResTy Type
		LHSTy = lhs.Type()
		RHSTy = rhs.Type()
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

	add := CreateBinaryOp(Add, lhs, rhs, ResTy)

	return add
}

func (b *Builder) CreateSub(lhs, rhs Expression) Expression {
	var (
		ResTy Type
		LHSTy = lhs.Type()
		RHSTy = rhs.Type()
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

	sub := CreateBinaryOp(Sub, lhs, rhs, ResTy)

	return sub
}

func (b *Builder) CreateXOR(lhs, rhs Expression) Expression {
	var (
		ResTy Type
		LHSTy = lhs.Type()
		RHSTy = rhs.Type()
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

	xor := CreateBinaryOp(Xor, lhs, rhs, ResTy)

	return xor
}

func (b *Builder) CreateRet(v Expression) *ReturnInst {
	var ret *ReturnInst
	if v != nil {
		ret = CreateRet(v)
	} else {
		ret = CreateRet(nil)
	}

	b.instructions = append(b.instructions, ret)

	return ret
}

func (b *Builder) CreateCmp(pred Opcode, lhs, rhs Expression) Expression {
	var (
		ResTy Type
		LHSTy = lhs.Type()
		RHSTy = rhs.Type()
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

	foo := CreateIdent(NextTemp(), ResTy)
	cmp := CreateCmpOp(pred, lhs, rhs, ResTy)

	b.CreateAssign(cmp, foo)

	return foo
}

func (b *Builder) CreateProcCall(callee Expression, args []Expression) *ProcCallInstr {
	call := CreateProcCall(callee, args)
	b.instructions = append(b.instructions, call)

	return call
}

func (b *Builder) CreateAssign(Val Expression, Dst Expression) *AssignInst {
	dst, ok := Dst.(NamedOperand)
	if !ok {
		panic("the destination of the assignInst is not a NamedOperand")
	}

	assign := CreateAssign(Val, dst)
	b.instructions = append(b.instructions, assign)

	return assign
}

func (b *Builder) CreateCondBr(cond Expression, ifTrue, ifFalse *Label) *CondBrInst {
	br := CreateCondBrInst(cond, ifTrue, ifFalse)
	b.instructions = append(b.instructions, br)

	return br
}

func (b *Builder) CreateJmp(dst *Label) *JumpInst {
	jmp := CreateJmp(dst)
	b.instructions = append(b.instructions, jmp)

	return jmp
}

func (b *Builder) CreateNeg(v Expression) Expression {
	neg := b.CreateSub(GetNullValue(v.Type()), v)

	return neg
}

func (b *Builder) CreateNot(v Expression) Expression {
	not := b.CreateXOR(v, GetAllOnesValue(v.Type()))

	return not
}

func (b *Builder) CreateFuncCall(callee *Ident, args []Expression) *FuncCallInst {
	call := CreateFuncCall(callee, args)

	return call
}
