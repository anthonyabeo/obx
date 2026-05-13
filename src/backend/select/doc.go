// Package selector implements parsing and plan derivation for target-selection
// descriptor files (.td).
//
// A descriptor file encodes target-specific instruction-selection rules using
// a block-structured grammar:
//
//	File        = [Header] { TargetBlock } ;
//	TargetBlock = "target" Ident "{" { BlockItem } "}" ;
//	BlockItem   = Header | Block | Rule ;
//	Block       = "block" Ident "{" { BlockItem } "}" ;
//	Rule        = "rule"  Ident "{" { RuleSection } "}" ;
//
// Each rule may contain match, legalize, pattern, cost, cond, and emit sections.
// Use [ParseFile], [ParseFileReader], or [ParseFilePath] to obtain a [*File]
// AST root from source text, an io.Reader, or a file path respectively.
//
// File        = [Header] { TargetBlock } ;
//
// Header      = "header" "{" { HeaderField } "}" ;
// HeaderField = Ident ":" Value ";" ;
//
// TargetBlock = "target" Ident "{" { BlockItem } "}" ;
// BlockItem   = Header
//
//	| Block
//	| Rule ;
//
// Block       = "block" Ident "{" { BlockItem } "}" ;
//
// Rule        = "rule" Ident "{" { RuleSection } "}" ;
//
// RuleSection = MatchSection
//
//	| LegalizeSection
//	| PatternSection
//	| CostSection
//	| CondSection
//	| EmitSection ;
//
// MatchSection =
//
//	"match" "{" { Binding | PatternDecl | MatchAttr } "}" ;
//
// Binding     = ("in" | "out" | "temp") Ident ":" TypeSpec [ "=" Value ] ";" ;
// PatternDecl = "pattern" PatternExpr ";" ;
// MatchAttr   = "commutative" ";" ;
//
// LegalizeSection =
//
//	"legalize" "{" { LegalizeItem } "}" ;
//
// LegalizeItem =
//
//	  "require" Predicate ";"
//	| "rewrite" RewriteExpr ";"
//	| "spill" Ident ";"
//	| "reload" Ident ";"
//	| "move" Value "->" Value ";"
//	| "if" Predicate "then" LegalizeAction ";" ;
//
// LegalizeAction =
//
//	  RewriteExpr
//	| "spill" Ident
//	| "reload" Ident
//	| "move" Value "->" Value ;
//
// In practice, require guards whether a legalization rule applies, rewrite
// describes a structural replacement, spill/reload model stack traffic for an
// operand, and move materializes an explicit value transfer (often into an ABI
// return register).
//
// PatternSection =
//
//	"pattern" PatternExpr ";" ;
//
// CostSection =
//
//	"cost" Number ";" ;
//
// CondSection =
//
//	"cond" PredicateList ";" ;
//
// PredicateList =
//
//	Predicate { "," Predicate } ;
//
// Predicate =
//
//	Ident [ "(" [ ValueList ] ")" ] ;
//
// EmitSection =
//
//	"emit" "{" { EmitStmt } "}" ;
//
// EmitStmt =
//
//	  Instr
//	| Template
//	| Comment ;
//
// Instr =
//
//	"instr" "{" { InstrField } "}" ";" ;
//
// InstrField =
//
//	  "opcode" ":" String ";"
//	| "dst" ":" Value ";"
//	| "src" ":" Value ";"
//	| "def" ":" Value ";"
//	| "uses" ":" ValueList ";"
//	| "attrs" ":" Record ";"
//	| "flags" ":" Value ";"
//	| "imm" ":" Value ";" ;
//
// Template =
//
//	"template" String ";" ;
//
// Comment =
//
//	"comment" String ";" ;
//
// PatternExpr =
//
//	Ident [ "(" [ PatternArgs ] ")" ] ;
//
// PatternArgs =
//
//	  PatternExpr { "," PatternExpr }
//	| Value { "," Value } ;
//
// RewriteExpr =
//
//	Ident [ "(" [ ValueList ] ")" ] ;
//
// ValueList =
//
//	Value { "," Value } ;
//
// Value =
//
//	  String
//	| Number
//	| Ident
//	| Ref
//	| List
//	| Record
//	| PatternExpr ;
//
// Ref =
//
//	"$" Ident ;
//
// TypeSpec =
//
//	Ident [ ":" Ident ] ;
//
// List =
//
//	"[" [ Value { "," Value } ] "]" ;
//
// Record =
//
//	"{" [ Field { "," Field } ] "}" ;
//
// Field =
//
//	Ident ("=" | ":") Value ;
package selector
