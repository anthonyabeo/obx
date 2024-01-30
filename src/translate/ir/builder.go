package ir

type Builder struct {
	BB *BasicBlock // the current BasicBlock where instructions are inserted
}

// InsertPoint returns the current BasicBlock where instructions are inserted
func (b *Builder) InsertPoint() *BasicBlock { return b.BB }

// SetInsertPoint updates the current BasicBlock to BB
func (b *Builder) SetInsertPoint(BB *BasicBlock) { b.BB = BB }

func (b *Builder) CreateAdd(LHS, RHS Value, name string) Value {
	return CreateAdd(LHS.Type(), LHS, RHS, name)
}

func (b *Builder) CreateRet(v Value) *ReturnInst {
	return CreateRet(v.Type(), v)
}

func (b *Builder) CreateRetVoid() *ReturnInst {
	return CreateRetVoid()
}
