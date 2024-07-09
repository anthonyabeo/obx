package tacil

type Program struct {
	Modules []*Module
}

func (p *Program) AddModule(mod *Module) {
	p.Modules = append(p.Modules, mod)
}
