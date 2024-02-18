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
	VisitExprRange(*ExprRange)

	VisitIfStmt(*IfStmt)
	VisitAssignStmt(*AssignStmt)
	VisitReturnStmt(*ReturnStmt)
	VisitProcCall(*ProcCall)
	VisitRepeatStmt(*RepeatStmt)
	VisitWhileStmt(*WhileStmt)
	VisitLoopStmt(*LoopStmt)
	VisitCaseStmt(*CaseStmt)
	VisitForStmt(*ForStmt)
	VisitExitStmt(*ExitStmt)
	VisitWithStmt(*WithStmt)

	VisitImport(*Import)
	VisitProcDecl(*ProcDecl)
	VisitVarDecl(*VarDecl)
	VisitConstDecl(*ConstDecl)
	VisitTypeDecl(*TypeDecl)

	VisitBasicType(*BasicType)
	VisitArrayType(*ArrayType)
	VisitPointerType(*PointerType)
	VisitProcType(*ProcType)
	VisitRecordType(*RecordType)
	VisitEnumType(*EnumType)

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
