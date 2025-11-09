package opt

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/obx/modgraph"
	"github.com/anthonyabeo/obx/src/ir/mir"
)

func filterBlocks(blocks map[int]*mir.Block, reachable map[int]bool) map[int]*mir.Block {
	out := make(map[int]*mir.Block)
	for id, b := range blocks {
		if reachable[id] {
			out[id] = b
		}
	}
	return out
}

func intersectAllPreds(doms map[*mir.Block]map[*mir.Block]struct{}, preds []*mir.Block) map[*mir.Block]struct{} {
	if len(preds) == 0 {
		return map[*mir.Block]struct{}{}
	}

	// Start with copy of first pred's dominators
	res := copySet(doms[preds[0]])
	for _, p := range preds[1:] {
		res = intersect(res, doms[p])
	}
	return res
}

func intersect(a, b map[*mir.Block]struct{}) map[*mir.Block]struct{} {
	res := make(map[*mir.Block]struct{})
	for k := range a {
		if _, ok := b[k]; ok {
			res[k] = struct{}{}
		}
	}
	return res
}

func copySet(s map[*mir.Block]struct{}) map[*mir.Block]struct{} {
	res := make(map[*mir.Block]struct{}, len(s))
	for k := range s {
		res[k] = struct{}{}
	}
	return res
}

func sameSet(a, b map[*mir.Block]struct{}) bool {
	if len(a) != len(b) {
		return false
	}
	for k := range a {
		if _, ok := b[k]; !ok {
			return false
		}
	}
	return true
}

func contains(list []*mir.Block, b *mir.Block) bool {
	for _, x := range list {
		if x == b {
			return true
		}
	}
	return false
}

func PrintDominators(doms map[*mir.Block]map[*mir.Block]struct{}) {
	for b, set := range doms {
		fmt.Printf("Dom(%s): ", b.Label)
		for bb := range set {
			fmt.Printf("%s ", bb.Label)
		}
		fmt.Println()
	}
}

func printDomTree(root *mir.Block, tree map[*mir.Block][]*mir.Block, depth int) {
	fmt.Printf("%s%s\n", strings.Repeat("  ", depth), root.Label)
	for _, child := range tree[root] {
		printDomTree(child, tree, depth+1)
	}
}

func printIDoms(idom map[*mir.Block]*mir.Block) {
	for b, d := range idom {
		if d == nil {
			fmt.Printf("idom(%s) = <nil>\n", b.Label)
		} else {
			fmt.Printf("idom(%s) = %s\n", b.Label, d.Label)
		}
	}
}

func VizCFG(fn *mir.Function) error {
	dot := fn.OutputDOT()

	Root, err := modgraph.FindProjectRoot()
	if err != nil {
		return err
	}

	outDir := filepath.Join(Root, "out")
	if _, err := os.Stat(outDir); os.IsNotExist(err) {
		if err := os.MkdirAll(outDir, 0755); err != nil {
			return fmt.Errorf("failed to create output directory: %w", err)
		}
	}

	dotFile := fmt.Sprintf("%s/%s.ssa.cfg.dot", outDir, fn.FnName)
	pngFile := fmt.Sprintf("%s/%s.ssa.cfg.png", outDir, fn.FnName)

	if err := os.WriteFile(dotFile, []byte(dot), 0644); err != nil {
		return err
	}

	cmd := exec.Command("dot", "-Tpng", dotFile, "-o", pngFile)
	if err := cmd.Run(); err != nil {
		return err
	} else {
		fmt.Printf("Generated %s\n", pngFile)
	}

	return nil
}

func VizSSA(fn *mir.Function) error {
	dot := fn.OutputDOT()

	Root, err := modgraph.FindProjectRoot()
	if err != nil {
		return err
	}

	outDir := filepath.Join(Root, "out")
	if _, err := os.Stat(outDir); os.IsNotExist(err) {
		if err := os.MkdirAll(outDir, 0755); err != nil {
			return fmt.Errorf("failed to create output directory: %w", err)
		}
	}

	dotFile := fmt.Sprintf("%s/%s.ssa.cfg.dot", outDir, fn.FnName)
	pngFile := fmt.Sprintf("%s/%s.ssa.cfg.png", outDir, fn.FnName)

	if err := os.WriteFile(dotFile, []byte(dot), 0644); err != nil {
		return err
	}

	cmd := exec.Command("dot", "-Tpng", dotFile, "-o", pngFile)
	if err := cmd.Run(); err != nil {
		return err
	} else {
		fmt.Printf("Generated %s\n", pngFile)
	}

	return nil
}
