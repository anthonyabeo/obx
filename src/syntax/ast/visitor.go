package ast

type Visitor interface {
	VisitIdentifier(*Ident)
	VisitBinaryExpr(*BinaryExpr)
	VisitDesignator(*Designator)
	VisitFuncCall(*FuncCall)
	VisitUnaryExpr(*UnaryExpr)
	VisitQualifiedIdent(*QualifiedIdent)
	VisitSet(*Set)
	VisitBasicLit(*BasicLit)

	VisitIfStmt(*IfStmt)
	VisitAssignStmt(*AssignStmt)
	VisitReturnStmt(*ReturnStmt)
	VisitProcCall(*ProcCall)
	VisitRepeatStmt(*RepeatStmt)
	VisitWhileStmt(*WhileStmt)
	VisitLoopStmt(*LoopStmt)

	VisitProcDecl(*ProcDecl)
	VisitVarDecl(*VarDecl)

	VisitBasicType(*BasicType)

	VisitReceiver(*Receiver)
	VisitProcHead(*ProcHead)
	VisitProcBody(*ProcBody)
	VisitFPSection(*FPSection)
	VisitFormalParams(*FormalParams)

	VisitDotOp(*DotOp)
	VisitIndexOp(*IndexOp)
	VisitTypeGuard(*TypeGuard)
	VisitPointerDeref(*PointerDeref)
}
