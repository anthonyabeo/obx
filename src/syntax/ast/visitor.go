package ast

type Visitor interface {
	VisitOberon(*Oberon)
	VisitModule(*Module)
	VisitDefinition(*Definition)

	VisitBinaryExpr(*BinaryExpr)
	VisitDesignator(*Designator)
	VisitFunctionCall(*FunctionCall)
	VisitUnaryExpr(*UnaryExpr)
	VisitQualifiedIdent(*QualifiedIdent)
	VisitSet(*Set)
	VisitBasicLit(*BasicLit)
	VisitExprRange(*ExprRange)

	VisitIfStmt(*IfStmt)
	VisitAssignmentStmt(*AssignmentStmt)
	VisitReturnStmt(*ReturnStmt)
	VisitProcCall(*ProcedureCall)
	VisitRepeatStmt(*RepeatStmt)
	VisitWhileStmt(*WhileStmt)
	VisitLoopStmt(*LoopStmt)
	VisitCaseStmt(*CaseStmt)
	VisitForStmt(*ForStmt)
	VisitExitStmt(*ExitStmt)
	VisitWithStmt(*WithStmt)

	VisitImport(*Import)
	VisitProcDecl(*ProcedureDecl)
	VisitVarDecl(*VariableDecl)
	VisitConstDecl(*ConstantDecl)
	VisitTypeDecl(*TypeDecl)
	VisitProcHead(*ProcedureHeading)

	VisitBasicType(*BasicType)
	VisitArrayType(*ArrayType)
	VisitPointerType(*PointerType)
	VisitProcType(*ProcType)
	VisitRecordType(*RecordType)
	VisitEnumType(*EnumType)
	VisitNamedType(*NamedType)
}
