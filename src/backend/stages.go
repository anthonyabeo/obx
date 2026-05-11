package backend

import (
	"fmt"
	"sort"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

// Stage represents one backend pipeline phase.
type Stage interface {
	Name() string
	Enabled(target.Target) bool
	Run(*PipelineDriver, *mir.Program) (*mir.Program, error)
}

// StageFactory constructs a Stage.
type StageFactory func() Stage

var stageRegistry = map[string]StageFactory{}

// DefaultStageOrder is the canonical backend pipeline phase order.
var DefaultStageOrder = []string{
	"instruction-selection",
	"legalization",
	"instruction-scheduling",
	"register-allocation",
}

// RegisterStage makes a backend pipeline stage available by name.
func RegisterStage(name string, factory StageFactory) {
	stageRegistry[name] = factory
}

// LookupStage returns a freshly constructed Stage for the given name.
func LookupStage(name string) (Stage, error) {
	f, ok := stageRegistry[name]
	if !ok {
		return nil, fmt.Errorf("unknown backend stage %q; available: %s", name, strings.Join(AvailableStages(), ", "))
	}
	return f(), nil
}

// AvailableStages returns the names of all registered backend stages in sorted order.
func AvailableStages() []string {
	names := make([]string, 0, len(stageRegistry))
	for k := range stageRegistry {
		names = append(names, k)
	}
	sort.Strings(names)
	return names
}

type pipelineStage struct {
	name    string
	enabled func(target.Target) bool
	run     func(*PipelineDriver, *mir.Program) (*mir.Program, error)
}

func (s *pipelineStage) Name() string {
	if s == nil {
		return ""
	}
	return s.name
}

func (s *pipelineStage) Enabled(tgt target.Target) bool {
	if s == nil || s.enabled == nil {
		return true
	}
	return s.enabled(tgt)
}

func (s *pipelineStage) Run(p *PipelineDriver, prog *mir.Program) (*mir.Program, error) {
	if s == nil || s.run == nil {
		return prog, nil
	}
	return s.run(p, prog)
}

func init() {
	RegisterStage(DefaultStageOrder[0], func() Stage {
		return &pipelineStage{
			name:    DefaultStageOrder[0],
			enabled: func(target.Target) bool { return true },
			run: func(p *PipelineDriver, prog *mir.Program) (*mir.Program, error) {
				return p.InstructionSelection(prog)
			},
		}
	})
	RegisterStage(DefaultStageOrder[1], func() Stage {
		return &pipelineStage{
			name:    DefaultStageOrder[1],
			enabled: func(tgt target.Target) bool { return tgt != nil },
			run: func(p *PipelineDriver, prog *mir.Program) (*mir.Program, error) {
				return p.Legalization(prog)
			},
		}
	})
	RegisterStage(DefaultStageOrder[2], func() Stage {
		return &pipelineStage{
			name:    DefaultStageOrder[2],
			enabled: func(tgt target.Target) bool { return tgt != nil && tgt.ABIInfo().WordSize > 0 },
			run: func(p *PipelineDriver, prog *mir.Program) (*mir.Program, error) {
				return p.InstructionScheduling(prog)
			},
		}
	})
	RegisterStage(DefaultStageOrder[3], func() Stage {
		return &pipelineStage{
			name:    DefaultStageOrder[3],
			enabled: func(tgt target.Target) bool { return tgt != nil && tgt.ABIInfo().Align > 0 },
			run: func(p *PipelineDriver, prog *mir.Program) (*mir.Program, error) {
				return p.RegisterAllocation(prog)
			},
		}
	})
}
