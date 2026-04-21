package ast

type Scope interface {
	Insert(Symbol) Symbol
	Lookup(string) Symbol
	Parent() Scope
}

type LexicalScope struct {
	Name    string
	parent  *LexicalScope
	symbols map[string]Symbol
}

func NewLexicalScope(parent *LexicalScope, name string) *LexicalScope {
	return &LexicalScope{
		Name:    name,
		parent:  parent,
		symbols: make(map[string]Symbol),
	}
}

func (s *LexicalScope) Insert(obj Symbol) Symbol {
	if obj == nil || obj.Name() == "" {
		return nil
	}

	if other, exists := s.symbols[obj.Name()]; exists {
		return other
	}

	s.insert(obj.Name(), obj)
	if obj.Parent() == nil {
		obj.SetParent(s)
	}

	return nil
}

func (s *LexicalScope) insert(name string, obj Symbol) {
	if s.symbols == nil {
		s.symbols = make(map[string]Symbol)
	}
	s.symbols[name] = obj
}

func (s *LexicalScope) Lookup(name string) Symbol {
	for sc := s; sc != nil; sc = sc.parent {
		if sym, ok := sc.symbols[name]; ok {
			return sym
		}
	}
	return nil
}

func (s *LexicalScope) Parent() *LexicalScope {
	return s.parent
}

// Elems returns the underlying symbol map for this lexical scope. The map is
// the internal storage of the scope and callers should not mutate it.
func (s *LexicalScope) Elems() map[string]Symbol {
	return s.symbols
}

// -------------------------------------------------------------
type RecordScope struct {
	fields  map[string]Symbol
	methods map[string]Symbol
	base    *RecordScope
}

func NewRecordScope(base *RecordScope) *RecordScope {
	return &RecordScope{
		fields:  make(map[string]Symbol),
		methods: make(map[string]Symbol),
		base:    base,
	}
}

func (r *RecordScope) Insert(obj Symbol) Symbol {
	if other, exists := r.fields[obj.Name()]; exists {
		return other
	}

	r.insert(obj.Name(), obj)

	return nil
}

func (r *RecordScope) insert(name string, obj Symbol) {
	if r.fields == nil {
		r.fields = make(map[string]Symbol)
	}
	r.fields[name] = obj
}

func (r *RecordScope) Lookup(name string) Symbol {
	if sym, ok := r.fields[name]; ok {
		return sym
	}
	if sym, ok := r.methods[name]; ok {
		return sym
	}
	if r.base != nil {
		return r.base.Lookup(name)
	}
	return nil
}

// InsertMethod inserts a type-bound procedure symbol into the record's method
// table and sets its parent scope.  Returns the existing symbol if duplicate.
func (r *RecordScope) InsertMethod(obj Symbol, parent *LexicalScope) Symbol {
	if other, exists := r.methods[obj.Name()]; exists {
		return other
	}
	if r.methods == nil {
		r.methods = make(map[string]Symbol)
	}
	r.methods[obj.Name()] = obj
	if obj.Parent() == nil && parent != nil {
		obj.SetParent(parent)
	}
	return nil
}

// LookupMethod searches the method table of this record and its base chain.
func (r *RecordScope) LookupMethod(name string) Symbol {
	if sym, ok := r.methods[name]; ok {
		return sym
	}
	if r.base != nil {
		return r.base.LookupMethod(name)
	}
	return nil
}

func (r *RecordScope) Elems() map[string]Symbol {
	return r.fields
}

func (r *RecordScope) Methods() map[string]Symbol {
	return r.methods
}
