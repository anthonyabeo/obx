package target

// KnownTargets is the initial minir-first target factory set.
var KnownTargets = map[string]func() Target{
	"riscv64": func() Target { return NewRISCV64Target() },
	"arm64":   func() Target { return arm64Default },
}

var _ = KnownTargets
