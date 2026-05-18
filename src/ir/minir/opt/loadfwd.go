package miniropt

import (
	"fmt"

	"github.com/anthonyabeo/obx/src/ir/minir"
)

// LoadForward performs intra-block store-to-load forwarding (also called
// redundant-load elimination).
//
// For each basic block it walks instructions in program order, maintaining a
// map from address key → most-recently-stored Value.  When a LoadInst is
// encountered whose address already has a known value, the load result is
// substituted with that value and the load is deleted.
//
// Aliasing / invalidation rules (conservative):
//   - StoreInst:          updates the known value for that address.
//   - LoadInst (unknown): records the load result as the current known value
//     (useful for chaining: load → icmp, no intervening
//     store).
//   - CallInst:           flushes the entire map — a call may clobber any
//     global variable or escaped alloca.
//   - GEPInst:            does not invalidate (computes an address, no write).
//
// This is a local (single-block) analysis.  Inter-block propagation requires
// a full GVN / available-expression analysis and is left for a future pass.
//
// Returns the number of loads eliminated.
func LoadForward(fn *minir.Function) int {
	if fn == nil {
		return 0
	}
	total := 0
	for _, b := range fn.Blocks {
		total += forwardBlock(b)
	}
	return total
}

// forwardBlock performs load-forwarding within one block.
func forwardBlock(b *minir.Block) int {
	// known maps addrKey(addr) → the value currently held at that address.
	known := make(map[string]minir.Value)

	// subst maps a forwarded load-result *Temp → its replacement Value.
	// Reuses replaceInInstr from mem2reg.go (same package).
	subst := make(map[*minir.Temp]minir.Value)

	// toDelete holds the LoadInst instructions to be removed.
	toDelete := make(map[minir.Instr]bool)

	for _, ins := range b.Instrs {
		switch instr := ins.(type) {
		case *minir.StoreInst:
			key := addrKey(instr.Addr)
			if key == "" {
				break
			}
			// Keep the map current: if the value being stored was itself a
			// forwarded load result, chase one level of subst now.
			known[key] = applySubstToValue(instr.Val, subst)

		case *minir.LoadInst:
			key := addrKey(instr.Addr)
			if key == "" {
				break
			}
			if cached, ok := known[key]; ok {
				// The address holds a value we already know — eliminate this load.
				subst[instr.Dst] = cached
				toDelete[ins] = true
			} else {
				// First load of this address in the block; record its result so
				// a later load of the same address can be forwarded from it.
				known[key] = instr.Dst
			}

		case *minir.CallInst:
			// Conservative flush: any call may write to a global or escaped
			// alloca whose address we cannot inspect here.
			_ = instr
			known = make(map[string]minir.Value)
		}
	}

	if len(toDelete) == 0 {
		return 0
	}

	// Rebuild the block, applying substitutions and dropping forwarded loads.
	var rebuilt []minir.Instr
	for _, ins := range b.Instrs {
		if toDelete[ins] {
			continue
		}
		rebuilt = append(rebuilt, replaceInInstr(ins, subst))
	}
	b.Instrs = rebuilt
	if n := len(rebuilt); n > 0 {
		if t, ok := rebuilt[n-1].(minir.Terminator); ok {
			b.Term = t
		}
	}
	return len(toDelete)
}

// addrKey returns a stable string key for an address value used as the key
// in the load-forwarding map.
//
//   - *GlobalRef    → "global:<GlobalName>"
//   - *Temp(IsAddr) → "alloca:<ID>"
//   - anything else → "" (caller skips the instruction)
func addrKey(v minir.Value) string {
	switch a := v.(type) {
	case *minir.GlobalRef:
		return "global:" + a.GlobalName
	case *minir.Temp:
		if a.IsAddr {
			return fmt.Sprintf("alloca:%d", a.ID)
		}
	}
	return ""
}

// applySubstToValue chases one level of the substitution map so that the
// known-value map always holds the most current representation of an
// address's contents.
func applySubstToValue(v minir.Value, subst map[*minir.Temp]minir.Value) minir.Value {
	if t, ok := v.(*minir.Temp); ok {
		if r, found := subst[t]; found {
			return r
		}
	}
	return v
}
