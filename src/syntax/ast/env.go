package ast

type Environment struct {
	name   string
	parent *Environment
	elems  map[string]Symbol
}

func NewEnvironment(parent *Environment, name string) *Environment {

	return &Environment{parent: parent, name: name}
}

func (s *Environment) Name() string         { return s.name }
func (s *Environment) SetName(name string)  { s.name = name }
func (s *Environment) Parent() *Environment { return s.parent }
func (s *Environment) Size() int            { return len(s.elems) }

// Lookup checks the current and (possibly) parent (and ancestor) scopes for
// the object with the given name if no such object exists, return nil.
func (s *Environment) Lookup(name string) Symbol {
	if obj := s.elems[name]; obj != nil {
		return obj
	}

	if s.parent != nil {
		return s.parent.Lookup(name)
	}

	return nil
}

func (s *Environment) Insert(obj Symbol) Symbol {
	if obj == nil || obj.Name() == "" {
		return nil
	}
	if alt := s.Lookup(obj.Name()); alt != nil {
		return alt
	}

	s.insert(obj.Name(), obj)
	if obj.Parent() == nil {
		obj.SetParent(s)
	}

	return nil
}

func (s *Environment) Elems() map[string]Symbol {
	return s.elems
}

func (s *Environment) insert(name string, obj Symbol) {
	if s.elems == nil {
		s.elems = make(map[string]Symbol)
	}
	s.elems[name] = obj
}

// RecordEnv represent the structure and operation of symbol table
// specific for record types. It manages the fields of a record in a
// map and the symbol table of its base type if any.
// ------------------------------------------------------------------
type RecordEnv struct {
	base   *RecordEnv
	elems  map[string]Symbol
	parent *Environment
}

func NewRecordEnv(base *RecordEnv, parent *Environment) *RecordEnv {
	return &RecordEnv{base: base, parent: parent, elems: map[string]Symbol{}}
}

func (r *RecordEnv) Size() int { return len(r.elems) }

func (r *RecordEnv) Parent() *Environment { return r.parent }

func (r *RecordEnv) Lookup(name string) Symbol {
	if obj := r.elems[name]; obj != nil {
		return obj
	}

	if r.base != nil {
		return r.base.Lookup(name)
	}

	return nil
}

func (r *RecordEnv) Insert(obj Symbol) Symbol {
	if obj == nil || obj.Name() == "" {
		return nil
	}

	r.insert(obj.Name(), obj)
	obj.SetParent(r.parent)

	return nil
}

func (r *RecordEnv) Elems() map[string]Symbol {
	return r.elems
}

func (r *RecordEnv) insert(name string, obj Symbol) {
	if r.elems == nil {
		r.elems = make(map[string]Symbol)
	}
	r.elems[name] = obj
}
