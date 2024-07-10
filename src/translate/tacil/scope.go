package tacil

type Scope interface {
	Parent() Scope
	Size() int
	Lookup(string) Object
	Insert(Object) Object
}

type SymbolTable struct {
	name   string
	parent Scope
	elems  map[string]Object
}

func NewScope(parent Scope, name string) *SymbolTable {
	return &SymbolTable{parent: parent, name: name, elems: map[string]Object{}}
}

// Parent returns the scope's containing (parent) scope.
func (s *SymbolTable) Parent() Scope { return s.parent }

// Size returns the number of symbols in the scope.
func (s *SymbolTable) Size() int { return len(s.elems) }

// Lookup checks the current and (possibly) parent (and ancestor) scopes for
// the object with the given name if no such object exists, return nil.
func (s *SymbolTable) Lookup(name string) Object {
	if obj := s.elems[name]; obj != nil {
		return obj
	}

	if s.parent != nil {
		return s.parent.Lookup(name)
	}

	return nil
}

func (s *SymbolTable) Insert(obj Object) Object {
	name := obj.Name()
	if alt := s.Lookup(name); alt != nil {
		return alt
	}

	s.insert(name, obj)
	if obj.Parent() == nil {
		obj.SetParent(s)
	}

	return nil
}

func (s *SymbolTable) Elems() map[string]Object {
	return s.elems
}

func (s *SymbolTable) insert(name string, obj Object) {
	if s.elems == nil {
		s.elems = make(map[string]Object)
	}
	s.elems[name] = obj
}
