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
	VisitNil(*Nil)

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
	VisitProcedureDecl(*ProcedureDecl)
	VisitVariableDecl(*VariableDecl)
	VisitConstantDecl(*ConstantDecl)
	VisitTypeDecl(*TypeDecl)
	VisitProcedureHeading(*ProcedureHeading)

	VisitBasicType(*BasicType)
	VisitArrayType(*ArrayType)
	VisitPointerType(*PointerType)
	VisitProcType(*ProcedureType)
	VisitRecordType(*RecordType)
	VisitEnumType(*EnumType)
	VisitNamedType(*NamedType)
}
