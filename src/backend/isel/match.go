package isel

import (
	"regexp"
	"strconv"

	"github.com/anthonyabeo/obx/src/backend/isel/dsl"
	"github.com/anthonyabeo/obx/src/backend/ralloc"
)

type MatchResult struct {
	Rule    *dsl.Rule
	Pattern *dsl.Node
	Bind    map[string]*dsl.Value
}

func (m *MatchResult) BindTempsAndOuts(env map[string]*dsl.Value, ra ralloc.RegAlloc) {
	env[m.Rule.Out.Name] = &dsl.Value{Kind: dsl.KindGPR, Reg: m.Pattern.Dst.Val.Reg}

	for _, v := range m.Rule.Temps {
		env[v.Name] = &dsl.Value{Kind: dsl.KindGPR, Reg: ra.NewVReg(v.Kind)}
	}
}

func (m *MatchResult) Emit() []string {
	out := make([]string, 0, len(m.Rule.Instructions))
	for _, l := range m.Rule.Instructions {
		out = append(out, Subst(l, m.Bind))
	}
	return out
}

// env binds "$name" -> Value captured from the IR during matching.
// classes is the union of rule.In, rule.Out, rule.Temps (name -> class).
func match(pt *dsl.Expr, ir *dsl.Node, env map[string]*dsl.Value, classes map[string]string) bool {
	if pt == nil || ir == nil {
		return false
	}

	// variable leaf?
	if len(pt.Var) > 0 {
		cls := classes[pt.Var] // "GPR" or "imm"

		// Check leaf node type
		if ir.Val == nil {
			return false
		}

		switch cls {
		case "GPR":
			if ir.Val.Kind != dsl.KindGPR {
				return false
			}
		case "imm":
			if ir.Val.Kind != dsl.KindImm {
				return false
			}
		default:
			return false
		}

		// Bind
		env[pt.Var] = ir.Val
		return true
	}

	// Operator node
	if ir.Op != pt.Op || len(ir.Args) != len(pt.Args) {
		return false
	}
	for i := range pt.Args {
		if !match(pt.Args[i], ir.Args[i], env, classes) {
			return false
		}
	}
	return true
}

// //////////////////////////////////////////////////////////////////////////////
// Subst replaces the variables in an instruction string with their respective
// values in env
// //////////////////////////////////////////////////////////////////////////////
var reVar = regexp.MustCompile(`\$(\w+)`)

func Subst(line string, env map[string]*dsl.Value) string {
	return reVar.ReplaceAllStringFunc(line, func(s string) string {
		key := s[1:] // "rd"
		v, ok := env[key]
		if !ok {
			return s
		} // leave as-is if unknown
		switch v.Kind {
		case dsl.KindGPR:
			return v.Reg
		case dsl.KindImm:
			return strconv.Itoa(v.Imm)
		default:
			return s
		}
	})
}
