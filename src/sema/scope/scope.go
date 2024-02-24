package scope

type Scope struct {
	name   string
	parent *Scope
	elems  map[string]Symbol
}

func NewScope(parent *Scope, name string) *Scope {
	return &Scope{parent: parent, name: name}
}

// Parent returns the scope's containing (parent) scope.
func (s *Scope) Parent() *Scope { return s.parent }

// Size returns the number of symbols in the scope.
func (s *Scope) Size() int { return len(s.elems) }

// Lookup checks the current and (possibly) parent (and ancestor) scopes for
// the object with the given name if no such object exists, return nil.
func (s *Scope) Lookup(name string) Symbol {
	if obj := s.elems[name]; obj != nil {
		return obj
	}

	if s.parent != nil {
		return s.parent.Lookup(name)
	}

	return nil
}

func (s *Scope) Insert(obj Symbol) Symbol {
	name := obj.Name()
	if alt := s.Lookup(name); alt != nil {
		return alt
	}

	s.insert(name, obj)
	if obj.Parent() == nil {
		obj.setParent(s)
	}

	return nil
}

func (s *Scope) Elems() map[string]Symbol {
	return s.elems
}

func (s *Scope) insert(name string, obj Symbol) {
	if s.elems == nil {
		s.elems = make(map[string]Symbol)
	}
	s.elems[name] = obj
}
