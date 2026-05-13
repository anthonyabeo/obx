package regalloc

import "github.com/anthonyabeo/obx/src/backend/mir"

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
