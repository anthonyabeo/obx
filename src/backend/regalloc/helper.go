package regalloc

import (
	"strconv"
	"strings"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
)

func isPhi(instr mir.Instr) bool {
	_, ok := instr.(*mir.PhiInstr)
	return ok
}

func isTerminator(instr mir.Instr) bool {
	_, ok := instr.(mir.Terminator)
	return ok
}

func cloneSet(src map[string]bool) map[string]bool {
	if src == nil {
		return make(map[string]bool)
	}
	dst := make(map[string]bool, len(src))
	for k := range src {
		dst[k] = true
	}
	return dst
}

func unionInto(dst map[string]bool, src map[string]bool) {
	for k := range src {
		dst[k] = true
	}
}

func equalSets(a, b map[string]bool) bool {
	if len(a) != len(b) {
		return false
	}
	for k := range a {
		if !b[k] {
			return false
		}
	}
	return true
}

func uniqueStrings(in []string) []string {
	out := make([]string, 0, len(in))
	seen := make(map[string]bool, len(in))
	for _, s := range in {
		if seen[s] {
			continue
		}
		seen[s] = true
		out = append(out, s)
	}
	return out
}

func filterStrings(in []string, keep func(string) bool) []string {
	out := make([]string, 0, len(in))
	for _, s := range in {
		if keep(s) {
			out = append(out, s)
		}
	}
	return uniqueStrings(out)
}

func contains(list []string, want string) bool {
	for _, s := range list {
		if s == want {
			return true
		}
	}
	return false
}

func containsString(list []string, want string) bool { return contains(list, want) }

func isFloatRegName(name string) bool {
	n := strings.ToLower(strings.TrimSpace(name))
	if n == "" {
		return false
	}
	if strings.HasPrefix(n, "d") && isDecimalSuffix(n[1:]) {
		return true
	}
	if strings.HasPrefix(n, "fa") && isDecimalSuffix(n[2:]) {
		return true
	}
	if strings.HasPrefix(n, "ft") && isDecimalSuffix(n[2:]) {
		return true
	}
	if strings.HasPrefix(n, "fs") && isDecimalSuffix(n[2:]) {
		return true
	}
	return false
}

func isDecimalSuffix(s string) bool {
	if s == "" {
		return false
	}
	_, err := strconv.Atoi(s)
	return err == nil
}

func regMatchesType(reg string, ty *mir.Type) bool {
	if ty == nil {
		return true
	}
	wantFloat := target.IsFloatType(ty)
	return wantFloat == isFloatRegName(reg)
}

func classifyVirtualRegs(fn *mir.Function) map[string]bool {
	classes := make(map[string]bool)
	if fn == nil {
		return classes
	}
	for _, p := range fn.Params {
		if p == nil || p.Name == "" || p.Type == nil {
			continue
		}
		classes[p.Name] = target.IsFloatType(p.Type)
	}
	for _, blk := range fn.Blocks {
		if blk == nil {
			continue
		}
		for _, ins := range blk.Instrs {
			collectVirtualRegClassesFromInstr(classes, ins)
		}
		collectVirtualRegClassesFromInstr(classes, blk.Term)
	}
	return classes
}

func collectVirtualRegClassesFromInstr(classes map[string]bool, ins mir.Instr) {
	if ins == nil {
		return
	}
	for _, d := range ins.Defs() {
		updateVirtualRegClass(classes, d)
	}
	for _, u := range ins.Uses() {
		collectVirtualRegClassesFromOperand(classes, u)
	}
}

func collectVirtualRegClassesFromOperand(classes map[string]bool, op mir.Operand) {
	switch v := op.(type) {
	case *mir.Register:
		updateVirtualRegClass(classes, v)
	case *mir.Memory:
		collectVirtualRegClassesFromOperand(classes, v.Base)
		collectVirtualRegClassesFromOperand(classes, v.Offset)
	}
}

func updateVirtualRegClass(classes map[string]bool, r *mir.Register) {
	if r == nil || r.Kind != mir.VirtualReg || r.Name == "" || r.Type() == nil {
		return
	}
	if target.IsFloatType(r.Type()) {
		classes[r.Name] = true
		return
	}
	if _, seen := classes[r.Name]; !seen {
		classes[r.Name] = false
	}
}
