package directive

import (
    "github.com/anthonyabeo/obx/src/support/diag"
    "github.com/anthonyabeo/obx/src/syntax/token"
)

// TokenSupplier supplies raw tokens from a scanner. The concrete scanner
// implements NextToken() token.Token.
type TokenSupplier interface {
    NextToken() token.Token
}

// Resolver resolves a directive identifier to a primitive value (bool,int64,float64).
// Returns (value, true) if found.
type Resolver func(name string) (any, bool)

// Reporter duplicates the diag.Reporter subset used by directive helpers.
type Reporter interface {
    Report(diag.Diagnostic)
    ErrorCount() int
}

// EvalState encapsulates evaluation state for directive expressions.
type EvalState struct {
    Supplier TokenSupplier
    Resolver Resolver
    cur token.Token
}

// NewEvalState constructs an EvalState.
func NewEvalState(s TokenSupplier, r Resolver) *EvalState {
    es := &EvalState{Supplier: s, Resolver: r}
    es.advance()
    return es
}

