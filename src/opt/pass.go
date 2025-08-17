package opt

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/obx/src/format"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

type ChangeSet struct {
	changed bool
	logs    []string
}

func (c *ChangeSet) Any() bool { return c.changed }
func (c *ChangeSet) Notef(format string, args ...any) {
	c.changed = true
	c.logs = append(c.logs, fmt.Sprintf(format, args...))
}
func (c *ChangeSet) MergeFrom(o *ChangeSet) {
	if o.changed {
		c.changed = true
		c.logs = append(c.logs, o.logs...)
	}
}

// PassContext carries shared analysis state between passes in a pipeline run.
type PassContext struct {
	analyses map[string]any
	cs       *ChangeSet
}

func NewPassContext() *PassContext {
	return &PassContext{analyses: make(map[string]any), cs: &ChangeSet{}}
}
func (ctx *PassContext) Get(name string) (any, bool) { v, ok := ctx.analyses[name]; return v, ok }
func (ctx *PassContext) Put(name string, v any)      { ctx.analyses[name] = v }
func (ctx *PassContext) Invalidate(names ...string) {
	if len(names) == 0 {
		ctx.analyses = make(map[string]any)
		return
	}
	for _, n := range names {
		delete(ctx.analyses, n)
	}
}

// Pass is implemented by all transformations/analyses.
type Pass interface {
	Name() string
	Run(fn *mir.Function, ctx *PassContext) *ChangeSet
}

// PassManager coordinates running passes.
type PassManager struct {
	Verbose bool
	passes  []Pass
	ctx     *PassContext

	defaultO map[int][]string
}

func NewPassManager() *PassManager {
	return &PassManager{
		passes: make([]Pass, 0),
		ctx:    NewPassContext(),

		defaultO: map[int][]string{
			0: {},
			1: {"constprop", "dce"},
			2: {"constprop", "dce", "sccp"},
			3: {"constprop", "dce", "sccp", "loopunroll"},
		},
	}
}

func (pm *PassManager) Add(p Pass) { pm.passes = append(pm.passes, p) }

func (pm *PassManager) ConfigurePasses(config map[string]any) {
	var (
		optLevel int
		verbose  bool
		passes   string
		disable  string
	)

	optLevel = config["optlevel"].(int)
	verbose = config["verbose"].(bool)
	if enablePasses := config["enablePasses"]; enablePasses != nil {
		passes = enablePasses.(string)
	}
	if disablePasses := config["disablePasses"]; disablePasses != nil {
		disable = disablePasses.(string)
	}

	selected := map[string]bool{}

	if passes != "" {
		for _, name := range strings.Split(passes, ",") {
			selected[strings.ToLower(strings.TrimSpace(name))] = true
		}
	} else {
		for _, name := range pm.defaultO[optLevel] {
			selected[name] = true
		}
	}

	if disable != "" {
		for _, name := range strings.Split(disable, ",") {
			delete(selected, strings.ToLower(strings.TrimSpace(name)))
		}
	}

	pm.Verbose = verbose

	// Add passes in order
	for name := range selected {
		if p, ok := passRegistry[name]; ok {
			pm.Add(p)
		}
	}
}

// RunOnce runs all passes once in order.
func (pm *PassManager) RunOnce(fn *mir.Function) {
	//ctx := NewPassContext()
	for _, p := range pm.passes {
		if pm.Verbose {
			fmt.Printf("==> Running pass: %s\n", p.Name())
			fmt.Println("----- BEFORE -----")
			format.FormatFunction(fn)
		}
		cs := p.Run(fn, pm.ctx)
		if pm.Verbose {
			if cs != nil && cs.Any() {
				for _, l := range cs.logs {
					fmt.Printf("  • %s\n", l)
				}
				fmt.Println("----- AFTER ------")
				format.FormatFunction(fn)
			} else {
				fmt.Println("  (no changes)")
			}
		}
	}
}

// RunFixedPoint keeps running the whole pipeline until no pass changes anything.
func (pm *PassManager) RunFixedPoint(fn *mir.Function, maxIters int) {
	if maxIters <= 0 {
		maxIters = 20
	}
	//ctx := NewPassContext()
	for iter := 1; iter <= maxIters; iter++ {
		if pm.Verbose {
			fmt.Printf("\n==== OPT ITERATION %d ====\n", iter)
		}
		var anyChange bool
		for _, p := range pm.passes {
			if pm.Verbose {
				fmt.Printf("==> Running pass: %s\n", p.Name())
			}
			if pm.Verbose {
				fmt.Println("----- BEFORE -----")
				fmt.Println(format.FormatFunction(fn))
			}
			cs := p.Run(fn, pm.ctx)
			if cs != nil && cs.Any() {
				anyChange = true
				if pm.Verbose {
					for _, l := range cs.logs {
						fmt.Printf("  • %s\n", l)
					}
					fmt.Println("----- AFTER ------")
					fmt.Println(format.FormatFunction(fn))
				}
			} else if pm.Verbose {
				fmt.Println("  (no changes)")
			}
		}
		if !anyChange {
			if pm.Verbose {
				fmt.Printf("Reached fixed point after %d iteration(s).\n", iter)
			}
			return
		}
		// In a real pipeline you may selectively invalidate analyses here
	}
	if pm.Verbose {
		fmt.Println("Stopped due to maxIters; may not be at a fixed point.")
	}
}

var passRegistry = map[string]Pass{}

func RegisterPass(p Pass) {
	passRegistry[strings.ToLower(p.Name())] = p
}

func init() {
	RegisterPass(ConstantFold{})
}
