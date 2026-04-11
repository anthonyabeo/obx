package opt

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/obx/src/ir/obxir"
	"github.com/anthonyabeo/obx/src/project"
)

func filterBlocks(blocks map[int]*obxir.Block, reachable map[int]bool) map[int]*obxir.Block {
	out := make(map[int]*obxir.Block)
	for id, b := range blocks {
		if reachable[id] {
			out[id] = b
		}
	}
	return out
}

func intersectAllPreds(doms map[*obxir.Block]map[*obxir.Block]struct{}, preds []*obxir.Block) map[*obxir.Block]struct{} {
	if len(preds) == 0 {
		return map[*obxir.Block]struct{}{}
	}

	// Start with copy of first pred's dominators
	res := copySet(doms[preds[0]])
	for _, p := range preds[1:] {
		res = intersect(res, doms[p])
	}
	return res
}

func intersect(a, b map[*obxir.Block]struct{}) map[*obxir.Block]struct{} {
	res := make(map[*obxir.Block]struct{})
	for k := range a {
		if _, ok := b[k]; ok {
			res[k] = struct{}{}
		}
	}
	return res
}

func copySet(s map[*obxir.Block]struct{}) map[*obxir.Block]struct{} {
	res := make(map[*obxir.Block]struct{}, len(s))
	for k := range s {
		res[k] = struct{}{}
	}
	return res
}

func sameSet(a, b map[*obxir.Block]struct{}) bool {
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

func contains(list []*obxir.Block, b *obxir.Block) bool {
	for _, x := range list {
		if x == b {
			return true
		}
	}
	return false
}

func PrintDominators(doms map[*obxir.Block]map[*obxir.Block]struct{}) {
	for b, set := range doms {
		fmt.Printf("Dom(%s): ", b.Label)
		for bb := range set {
			fmt.Printf("%s ", bb.Label)
		}
		fmt.Println()
	}
}

func printDomTree(root *obxir.Block, tree map[*obxir.Block][]*obxir.Block, depth int) {
	fmt.Printf("%s%s\n", strings.Repeat("  ", depth), root.Label)
	for _, child := range tree[root] {
		printDomTree(child, tree, depth+1)
	}
}

func printIDoms(idom map[*obxir.Block]*obxir.Block) {
	for b, d := range idom {
		if d == nil {
			fmt.Printf("idom(%s) = <nil>\n", b.Label)
		} else {
			fmt.Printf("idom(%s) = %s\n", b.Label, d.Label)
		}
	}
}

func VizCFG(fn *obxir.Function) error {
	dot := fn.OutputDOT()

	Root, err := project.FindProjectRoot()
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

func VizSSA(fn *obxir.Function) error {
	dot := fn.OutputDOT()

	Root, err := project.FindProjectRoot()
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
