package selector

// File is the root AST node of a descriptor file.
//
//	File = [Header] { TargetBlock } ;
type File struct {
	Header  *Header        // optional top-level header; nil if absent
	Targets []*TargetBlock // zero or more target blocks
}

// Header represents a "header { … }" block.
//
//	Header = "header" "{" { HeaderField } "}" ;
type Header struct {
	Fields []*HeaderField
}

func (*Header) blockItemNode() {}

// HeaderField is a single key: value; line inside a Header.
//
//	HeaderField = Ident ":" Value ";" ;
type HeaderField struct {
	Key string
	Val *Value
}

// TargetBlock is a named "target Ident { … }" body.
//
//	TargetBlock = "target" Ident "{" { BlockItem } "}" ;
type TargetBlock struct {
	Name  string
	Items []BlockItem
}

// BlockItem is any item that may appear inside a TargetBlock or Block body.
//
//	BlockItem = Header | Block | Rule ;
type BlockItem interface {
	blockItemNode()
}

// Block is a named "block Ident { … }" container.
//
//	Block = "block" Ident "{" { BlockItem } "}" ;
type Block struct {
	Name  string
	Items []BlockItem
}

func (*Block) blockItemNode() {}

// Rule is a named "rule Ident { … }" definition.
//
//	Rule = "rule" Ident "{" { RuleSection } "}" ;
type Rule struct {
	Name     string
	Sections []RuleSection
}

func (*Rule) blockItemNode() {}

// RuleSection is any section that may appear inside a Rule body.
//
//	RuleSection = MatchSection | LegalizeSection | PatternSection
//	            | CostSection | CondSection | EmitSection ;
type RuleSection interface {
	ruleSectionNode()
}

// MatchSection holds operand bindings, pattern declarations, and match attributes.
//
//	MatchSection = "match" "{" { Binding | PatternDecl | MatchAttr } "}" ;
type MatchSection struct {
	Items []MatchItem
}

func (*MatchSection) ruleSectionNode() {}

// MatchItem is any item that may appear inside a MatchSection.
type MatchItem interface {
	matchItemNode()
}

// Binding declares an in/out/temp operand with its TypeSpec and optional default.
//
//	Binding = ("in" | "out" | "temp") Ident ":" TypeSpec [ "=" Value ] ";" ;
type Binding struct {
	Dir     string   // "in", "out", or "temp"
	Name    string   // binding name, possibly "$"-prefixed
	Type    TypeSpec // operand type
	Default *Value   // nil if no default expression
}

func (*Binding) matchItemNode() {}

// TypeSpec describes the type of a bound operand.
//
//	TypeSpec = Ident [ ":" Ident ] [ ":" Ref ] ;
type TypeSpec struct {
	Kind string // register class or type name, e.g. "GPR", "imm", "label"
	Sub  string // allocation mode, e.g. "virt", "phys" (optional)
	Ref  string // specific register name without "$", e.g. "rd" (optional)
}

// PatternDecl declares the pattern matched inside a MatchSection.
//
//	PatternDecl = "pattern" PatternExpr ";" ;
type PatternDecl struct {
	Pattern *PatternExpr
}

func (*PatternDecl) matchItemNode() {}

// MatchAttr is a single match-time attribute keyword such as "commutative".
//
//	MatchAttr = "commutative" ";" ;
type MatchAttr struct {
	Name string
}

func (*MatchAttr) matchItemNode() {}

// LegalizeSection holds legalization constraints and rewrites.
//
//	LegalizeSection = "legalize" "{" { LegalizeItem } "}" ;
type LegalizeSection struct {
	Items []LegalizeItem
}

func (*LegalizeSection) ruleSectionNode() {}

// LegalizeItem is any item that may appear inside a LegalizeSection.
type LegalizeItem interface {
	legalizeItemNode()
}

// RequireItem asserts a predicate must hold before selection.
//
//	"require" Predicate ";"
type RequireItem struct{ Pred *Predicate }

func (*RequireItem) legalizeItemNode() {}

// RewriteItem rewrites the matched IR tree node.
//
//	"rewrite" RewriteExpr ";"
type RewriteItem struct{ Expr *RewriteExpr }

func (*RewriteItem) legalizeItemNode() {}

// SpillItem requests that a named operand be spilled.
//
//	"spill" Ident ";"
type SpillItem struct{ Name string }

func (*SpillItem) legalizeItemNode() {}

// ReloadItem requests that a named operand be reloaded.
//
//	"reload" Ident ";"
type ReloadItem struct{ Name string }

func (*ReloadItem) legalizeItemNode() {}

// MoveItem copies a value from one location to another.
//
//	"move" Value "->" Value ";"
type MoveItem struct{ From, To *Value }

func (*MoveItem) legalizeItemNode() {}

// IfItem conditionally applies a LegalizeAction when Pred holds.
//
//	"if" Predicate "then" LegalizeAction ";"
type IfItem struct {
	Pred   *Predicate
	Action LegalizeAction
}

func (*IfItem) legalizeItemNode() {}

// LegalizeAction is the action taken inside an IfItem.
type LegalizeAction interface {
	legalizeActionNode()
}

// RewriteExpr is a rewrite call expression.
//
//	RewriteExpr = Ident [ "(" [ ValueList ] ")" ] ;
type RewriteExpr struct {
	Name string
	Args []*Value // nil when no argument list is present
}

func (*RewriteExpr) legalizeActionNode() {}

// SpillAction spills an operand.
type SpillAction struct{ Name string }

func (*SpillAction) legalizeActionNode() {}

// ReloadAction reloads an operand.
type ReloadAction struct{ Name string }

func (*ReloadAction) legalizeActionNode() {}

// MoveAction moves a value from one location to another.
type MoveAction struct{ From, To *Value }

func (*MoveAction) legalizeActionNode() {}

// PatternSection is the top-level "pattern" section of a Rule.
//
//	PatternSection = "pattern" PatternExpr ";" ;
type PatternSection struct {
	Pattern *PatternExpr
}

func (*PatternSection) ruleSectionNode() {}

// CostSection holds the numeric cost of a Rule.
//
//	CostSection = "cost" Number ";" ;
type CostSection struct {
	Cost string // numeric literal
}

func (*CostSection) ruleSectionNode() {}

// CondSection holds the predicate guards of a Rule.
//
//	CondSection = "cond" PredicateList ";" ;
type CondSection struct {
	Predicates []*Predicate
}

func (*CondSection) ruleSectionNode() {}

// Predicate is a named Boolean guard with optional arguments.
// It may be negated with a leading "!".
//
//	Predicate = Ident [ "(" [ ValueList ] ")" ] ;
type Predicate struct {
	Negated bool
	Name    string
	Args    []*Value // nil when no argument list is present
}

// EmitSection holds the code-emission statements of a Rule.
//
//	EmitSection = "emit" "{" { EmitStmt } "}" ;
type EmitSection struct {
	Stmts []EmitStmt
}

func (*EmitSection) ruleSectionNode() {}

// EmitStmt is any statement that may appear inside an EmitSection.
type EmitStmt interface {
	emitStmtNode()
}

// InstrStmt is a single machine-instruction template.
//
//	Instr = "instr" "{" { InstrField } "}" ";" ;
type InstrStmt struct {
	Fields []*InstrField
}

func (*InstrStmt) emitStmtNode() {}

// InstrField is a key: value pair inside an InstrStmt.
//
//	InstrField = key ":" Value ";" ;
type InstrField struct {
	Name string
	Val  *Value
}

// TemplateStmt is a raw-string emit template.
//
//	Template = "template" String ";" ;
type TemplateStmt struct{ Text string }

func (*TemplateStmt) emitStmtNode() {}

// CommentStmt is an inline comment inside an EmitSection.
//
//	Comment = "comment" String ";" ;
type CommentStmt struct{ Text string }

func (*CommentStmt) emitStmtNode() {}

// PatternExpr is a call-style pattern node.
//
//	PatternExpr = Ident [ "(" [ PatternArgs ] ")" ] ;
type PatternExpr struct {
	Name string
	Args []PatternArg // nil when no argument list is present
}

func (*PatternExpr) patternArgNode() {}

// PatternArg is either a nested PatternExpr or a plain Value.
//
//	PatternArgs = PatternExpr { "," PatternExpr }
//	            | Value { "," Value } ;
type PatternArg interface {
	patternArgNode()
}

// ValueKind classifies the Value variants.
type ValueKind int

const (
	ValueString  ValueKind = iota // quoted string literal
	ValueNumber                   // numeric literal
	ValueIdent                    // plain identifier
	ValueRef                      // $name reference
	ValueList                     // [ … ] list
	ValueRecord                   // { … } record
	ValuePattern                  // Ident ( args… ) pattern call
)

// Value is any scalar, composite, or reference value in the grammar.
//
//	Value = String | Number | Ident | Ref | List | Record | PatternExpr ;
type Value struct {
	Kind    ValueKind
	Lit     string         // String lit, Number lit, or Ident name
	Ref     string         // Ref name without leading "$"
	Elems   []*Value       // List elements
	Fields  []*RecordField // Record fields
	Pattern *PatternExpr   // PatternExpr when Kind == ValuePattern
}

func (*Value) patternArgNode() {}

// RecordField is one key = value entry in a Record.
//
//	Field = Ident ("=" | ":") Value ;
type RecordField struct {
	Key string
	Sep string // "=" or ":"
	Val *Value
}

