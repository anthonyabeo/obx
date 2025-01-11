package scope

type Scope interface {
	Parent() Scope
	Size() int
	Lookup(string) Symbol
	Insert(Symbol) Symbol
}

type SymbolTable struct {
	name   string
	parent Scope
	elems  map[string]Symbol
}

func NewScope(parent Scope, name string) *SymbolTable {
	return &SymbolTable{parent: parent, name: name}
}

// Parent returns the scope's containing (parent) scope.
func (s *SymbolTable) Parent() Scope { return s.parent }

// Size returns the number of symbols in the scope.
func (s *SymbolTable) Size() int { return len(s.elems) }

// Lookup checks the current and (possibly) parent (and ancestor) scopes for
// the object with the given name if no such object exists, return nil.
func (s *SymbolTable) Lookup(name string) Symbol {
	if obj := s.elems[name]; obj != nil {
		return obj
	}

	if s.parent != nil {
		return s.parent.Lookup(name)
	}

	return nil
}

func (s *SymbolTable) Insert(obj Symbol) Symbol {
	name := obj.Name()
	//if alt := s.Lookup(name); alt != nil {
	//	return alt
	//}

	s.insert(name, obj)
	if obj.Parent() == nil {
		obj.setParent(s)
	}

	return nil
}

func (s *SymbolTable) Elems() map[string]Symbol {
	return s.elems
}

func (s *SymbolTable) insert(name string, obj Symbol) {
	if s.elems == nil {
		s.elems = make(map[string]Symbol)
	}
	s.elems[name] = obj
}

// RecordSymTable represent the structure and operation of symbol table
// specific for record types. It manages the fields of a record in a
// map and the symbol table of its base type if any.
// ------------------------------------------------------------------
type RecordSymTable struct {
	base   *RecordSymTable
	elems  map[string]Symbol
	parent Scope
}

func NewRecordScope(base *RecordSymTable, parent Scope) *RecordSymTable {
	return &RecordSymTable{base: base, parent: parent, elems: map[string]Symbol{}}
}

func (r *RecordSymTable) Size() int { return len(r.elems) }

func (r *RecordSymTable) Parent() Scope { return r.parent }

func (r *RecordSymTable) Lookup(name string) Symbol {
	if obj := r.elems[name]; obj != nil {
		return obj
	}

	if r.base != nil {
		return r.base.Lookup(name)
	}

	return nil
}

func (r *RecordSymTable) Insert(obj Symbol) Symbol {
	name := obj.Name()
	if alt := r.Lookup(name); alt != nil {
		return alt
	}

	r.insert(name, obj)
	if obj.Parent() == nil {
		obj.setParent(r)
	}

	return nil
}

func (r *RecordSymTable) Elems() map[string]Symbol {
	return r.elems
}

func (r *RecordSymTable) insert(name string, obj Symbol) {
	if r.elems == nil {
		r.elems = make(map[string]Symbol)
	}
	r.elems[name] = obj
}
