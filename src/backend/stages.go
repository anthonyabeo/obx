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
	"call-lowering",          // [0]  ABI arg/result expansion; runs before instruction selection
	"switch-lowering",        // [1]  switch terminator expansion; runs before instruction selection
	"instruction-selection",  // [2]
	"legalization",           // [3]
	"instruction-scheduling", // [4]
	"register-allocation",    // [5]
	"assemble",               // [6]
	"link",                   // [7]
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
