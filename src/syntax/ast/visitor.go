package ast

type Visitor interface {
	VisitOberon(*Oberon) any
	VisitModule(*Module) any
	VisitDefinition(*Definition) any

	VisitBinaryExpr(*BinaryExpr) any
	VisitDesignator(*Designator) any
	VisitFunctionCall(*FunctionCall) any
	VisitUnaryExpr(*UnaryExpr) any
	VisitQualifiedIdent(*QualifiedIdent) any
	VisitSet(*Set) any
	VisitBasicLit(*BasicLit) any
	VisitExprRange(*ExprRange) any
	VisitNil(*Nil) any

	VisitIfStmt(*IfStmt) any
	VisitAssignmentStmt(*AssignmentStmt) any
	VisitReturnStmt(*ReturnStmt) any
	VisitProcedureCall(*ProcedureCall) any
	VisitRepeatStmt(*RepeatStmt) any
	VisitWhileStmt(*WhileStmt) any
	VisitLoopStmt(*LoopStmt) any
	VisitCaseStmt(*CaseStmt) any
	VisitForStmt(*ForStmt) any
	VisitExitStmt(*ExitStmt) any
	VisitWithStmt(*WithStmt) any

	VisitImport(*Import) any
	VisitProcedureDecl(*ProcedureDecl) any
	VisitVariableDecl(*VariableDecl) any
	VisitConstantDecl(*ConstantDecl) any
	VisitTypeDecl(*TypeDecl) any
	VisitProcedureHeading(*ProcedureHeading) any

	VisitBasicType(*BasicType) any
	VisitArrayType(*ArrayType) any
	VisitPointerType(*PointerType) any
	VisitProcType(*ProcedureType) any
	VisitRecordType(*RecordType) any
	VisitEnumType(*EnumType) any
	VisitNamedType(*NamedType) any
}
