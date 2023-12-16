package ast

type Visitor interface {
	VisitIdentifier(*Ident)
	VisitUInt(*UInt)
	VisitBinaryExpr(*BinaryExpr)
	VisitDesignator(*Designator)
	VisitFuncCall(*FuncCall)
	VisitUnaryExpr(*UnaryExpr)
	VisitQualifiedIdent(*QualifiedIdent)

	VisitIfStmt(*IfStmt)
	VisitAssignStmt(*AssignStmt)
	VisitReturnStmt(*ReturnStmt)
	VisitProcCall(*ProcCall)
	VisitRepeatStmt(*RepeatStmt)
	VisitWhileStmt(*WhileStmt)
	VisitLoopStmt(*LoopStmt)
	
	VisitProcDecl(*ProcDecl)
	VisitVarDecl(*VarDecl)
}
