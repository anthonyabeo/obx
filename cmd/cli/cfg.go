package cli

import (
	"fmt"
	"log"
	"os"
	"path/filepath"
	"strings"

	zlog "github.com/rs/zerolog/log"
	"github.com/spf13/cobra"

	"github.com/anthonyabeo/obx/src/ir/desugar"
	"github.com/anthonyabeo/obx/src/ir/minir"
	miniropt "github.com/anthonyabeo/obx/src/ir/minir/opt"
	"github.com/anthonyabeo/obx/src/project"
	"github.com/anthonyabeo/obx/src/sema"
	"github.com/anthonyabeo/obx/src/support/cache"
	"github.com/anthonyabeo/obx/src/syntax/ast"
)

var cfgArgs struct {
	Path    string
	Target  string
	Out     string   // output path or "-" for stdout (when format=dot)
	Format  string   // dot|png|svg
	Fn      string   // function name to dump; defaults to module entry or first lowered function
	Defines []string // --define (repeatable); NAME or NAME=VALUE
}

func init() {
	cfgCmd.Flags().StringVarP(&cfgArgs.Path, "path", "p", "", "source root directory (defaults to roots in obx.mod)")
	cfgCmd.Flags().StringVarP(&cfgArgs.Target, "target", "T", "rv64imafd", "target architecture (used to select the stdlib platform layer)")
	cfgCmd.Flags().StringVarP(&cfgArgs.Out, "out", "o", "", "output path for CFG; use '-' for stdout when format=dot")
	cfgCmd.Flags().StringVar(&cfgArgs.Format, "format", "dot", "output format: dot (default), png, svg, or minir")
	cfgCmd.Flags().StringVar(&cfgArgs.Fn, "fn", "", "name of function to dump; defaults to module entry or first lowered function")
	cfgCmd.Flags().StringArrayVarP(&cfgArgs.Defines, "define", "d", nil,
		"set a compile-time directive constant: NAME (bool true) or NAME=VALUE (bool/int/float)")
}

var cfgCmd = &cobra.Command{
	Use:   "cfg",
	Short: "generate a control-flow graph (DOT/PNG/SVG) for a lowered function",
	Long: `Generate a CFG for a lowered function. The command runs parsing and semantic
analysis, lowers the program to minir, then emits a DOT, PNG, SVG, or textual
minir file for a selected function. Use --fn to select a function by name;
otherwise the module entry or the first lowered function is used.`,
	Run: func(cmd *cobra.Command, args []string) {
		entry := buildArgs.Entry
		roots := []string{cfgArgs.Path}

		ctx, _ := newContext(32)

		// Inject platform directives first so --define can still override.
		injectPlatformDirectives(ctx, cfgArgs.Target)

		if err := applyDirectives(ctx, cfgArgs.Defines); err != nil {
			fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
			os.Exit(1)
		}

		// ── 0. Fall back to obx.mod when --path is not given ─────────────
		var manifest project.Manifest
		if cfgArgs.Path == "" {
			dir, err := project.FindProjectRoot()
			if err != nil {
				log.Fatalf("cfg: no --path given and %s", err)
			}
			manifest, err = project.LoadManifest(dir)
			if err != nil {
				log.Fatalf("cfg: %v", err)
			}
			roots = manifest.Roots
			if entry == "" {
				entry = manifest.Entry
			}
		}

		if stdlibRoot := project.ResolveStdlibRoot(manifest); stdlibRoot != "" {
			roots = append([]string{stdlibRoot}, roots...)
		}

		// Discover and order modules
		sorted, graph, err := resolveModules(ctx, roots...)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
			os.Exit(1)
		}

		sorted, err = project.ReachableFrom(sorted, graph, entry)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
			os.Exit(1)
		}

		obx := ast.NewOberonX()

		// Attempt to load precompiled .obxi bundles for discovered modules so
		// we can inject scopes and skip reparsing when possible.
		preBundles := make(map[string]*minir.Module)
		loadedNames := make(map[string]bool)
		for _, h := range sorted {
			obxPath := h.File
			obxiPath := obxPath[:len(obxPath)-len(".obx")] + ".obxi"
			if _, err := os.Stat(obxiPath); err == nil {
				if b, err := cache.LoadBundle(obxiPath, nil); err == nil {
					ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
					preBundles[b.ModuleName] = b.Module
					loadedNames[h.Key.Name()] = true
					zlog.Info().Str("module", b.ModuleName).Str("path", obxiPath).Msg("cfg: precompiled module loaded; will skip parsing")
					continue
				}
			}
			cachePath := filepath.Join(filepath.Dir(obxPath), "cache", filepath.Base(obxPath[:len(obxPath)-len(".obx")]+".obxi"))
			if _, err := os.Stat(cachePath); err == nil {
				if b, err := cache.LoadBundle(cachePath, nil); err == nil {
					ctx.Env.AddModuleScope(b.ModuleName, b.Scope)
					preBundles[b.ModuleName] = b.Module
					loadedNames[h.Key.Name()] = true
					zlog.Info().Str("module", b.ModuleName).Str("path", cachePath).Msg("cfg: precompiled module loaded; will skip parsing")
					continue
				}
			}
		}

		var toParse []project.Header
		var skipped []string
		for _, h := range sorted {
			if loadedNames[h.Key.Name()] {
				skipped = append(skipped, h.Key.Name())
				continue
			}
			toParse = append(toParse, h)
		}
		if len(skipped) > 0 {
			zlog.Info().Int("count", len(skipped)).Str("modules", strings.Join(skipped, ", ")).Msg("cfg: skipping parse/sema for precompiled modules")
		}

		if ok := parseModules(toParse, ctx, obx); !ok {
			n := ctx.Reporter.ErrorCount()
			fmt.Fprintf(os.Stderr, "cfg failed: %d parse error(s)\n", n)
			os.Exit(1)
		}

		s := sema.NewSema(ctx, obx)
		s.Validate()
		if ctx.Reporter.ErrorCount() > 0 {
			ctx.Reporter.Flush()
			n := ctx.Reporter.ErrorCount()
			fmt.Fprintf(os.Stderr, "cfg failed: %d error(s)\n", n)
			os.Exit(1)
		}

		// Lower and pick target function
		gen := desugar.NewGenerator(obx, ctx)
		prog := gen.Generate()
		lowered := minir.Lower(prog)
		// Merge any precompiled minir modules loaded earlier (.obxi bundles).
		if len(preBundles) > 0 {
			exist := make(map[string]bool)
			for _, m := range lowered.Modules {
				exist[m.Name] = true
			}
			for name, mod := range preBundles {
				if exist[name] {
					continue
				}
				lowered.Modules = append(lowered.Modules, mod)
			}

			// Extern dedup: merge ExternalFunc declarations across all lowered
			// modules and precompiled bundles into a single canonical table keyed
			// by (CName|DLLName|signature). Replace module.Externals entries
			// with pointers to the canonical ExternalFunc objects.
			uniq := make(map[string]*minir.ExternalFunc)
			for _, m := range lowered.Modules {
				newExts := make([]*minir.ExternalFunc, 0, len(m.Externals))
				seen := make(map[string]bool)
				for _, e := range m.Externals {
					key := func() string {
						cname := e.Name
						dll := ""
						vari := false
						if e.Attrs != nil {
							if e.Attrs.CName != "" {
								cname = e.Attrs.CName
							}
							dll = e.Attrs.DLLName
							vari = e.Attrs.Variadic
						}
						sig := ""
						if e.Sig != nil {
							parts := make([]string, 0, len(e.Sig.Params)+1)
							for _, p := range e.Sig.Params {
								if p == nil {
									parts = append(parts, "<nil>")
								} else {
									parts = append(parts, p.String())
								}
							}
							if vari {
								parts = append(parts, "...")
							}
							res := "void"
							if e.Sig.Result != nil {
								res = e.Sig.Result.String()
							}
							sig = res + "(" + strings.Join(parts, ",") + ")"
						}
						return cname + "|" + dll + "|" + sig
					}()
					if seen[key] {
						continue
					}
					seen[key] = true
					if ex, ok := uniq[key]; ok {
						if ex.Attrs == nil && e.Attrs != nil {
							ex.Attrs = e.Attrs
						}
						newExts = append(newExts, ex)
					} else {
						uniq[key] = e
						newExts = append(newExts, e)
					}
				}
				m.Externals = newExts
			}
		}

		// Promote non-escaping scalar allocas, forward store-to-load pairs,
		// then run CFG cleanup passes.
		for _, mod := range lowered.Modules {
			for _, fn := range mod.Functions {
				miniropt.Mem2Reg(fn)
				miniropt.LoadForward(fn)
				miniropt.CleanCFG(fn)
			}
		}
		var fns []*minir.Function
		for _, mod := range lowered.Modules {
			fns = append(fns, mod.Functions...)
		}
		var target *minir.Function
		if cfgArgs.Fn != "" {
			for _, f := range fns {
				if f.FnName == cfgArgs.Fn {
					target = f
					break
				}
			}
		} else if entry != "" {
			for _, f := range fns {
				if f.FnName == entry {
					target = f
					break
				}
			}
		}
		if target == nil && len(fns) > 0 {
			target = fns[0]
		}
		if target == nil {
			fmt.Fprintln(os.Stderr, "cfg: no lowered functions available to dump")
			os.Exit(1)
		}

		format := strings.ToLower(cfgArgs.Format)
		switch format {
		case "dot", "":
			if cfgArgs.Out == "-" {
				fmt.Print(target.OutputDOT())
			} else if cfgArgs.Out == "" {
				// print to stdout by default for dot
				fmt.Print(target.OutputDOT())
			} else {
				f, err := os.Create(cfgArgs.Out)
				if err != nil {
					fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
					os.Exit(1)
				}
				defer f.Close()
				if _, err := f.WriteString(target.OutputDOT()); err != nil {
					fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
					os.Exit(1)
				}
			}
		case "png":
			if cfgArgs.Out == "" {
				fmt.Fprintf(os.Stderr, "cfg: --out required for png output\n")
				os.Exit(1)
			}
			if err := minir.SavePNG(target, cfgArgs.Out); err != nil {
				fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
				os.Exit(1)
			}
		case "svg":
			data, err := minir.RenderSVG(target)
			if err != nil {
				fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
				os.Exit(1)
			}
			if cfgArgs.Out == "" || cfgArgs.Out == "-" {
				fmt.Print(string(data))
			} else {
				if err := os.WriteFile(cfgArgs.Out, data, 0644); err != nil {
					fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
					os.Exit(1)
				}
			}
		case "minir":
			// Emit the textual minir representation of the selected function.
			if cfgArgs.Out == "" || cfgArgs.Out == "-" {
				if _, err := minir.NewEmitter(os.Stdout).EmitFunction(target); err != nil {
					fmt.Fprintf(os.Stderr, "cfg: emit minir: %v\n", err)
					os.Exit(1)
				}
			} else {
				f, err := os.Create(cfgArgs.Out)
				if err != nil {
					fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
					os.Exit(1)
				}
				defer f.Close()
				if _, err := minir.NewEmitter(f).EmitFunction(target); err != nil {
					fmt.Fprintf(os.Stderr, "cfg: emit minir: %v\n", err)
					os.Exit(1)
				}
			}
		default:
			fmt.Fprintf(os.Stderr, "cfg: unsupported format %q\n", format)
			os.Exit(1)
		}
		if cfgArgs.Out != "" && cfgArgs.Out != "-" {
			fmt.Fprintf(os.Stderr, "cfg: wrote %s (format=%s)\n", cfgArgs.Out, format)
		}
	},
}
