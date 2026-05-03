package cli

import (
    "fmt"
    "log"
    "os"
    "strings"

    "github.com/spf13/cobra"

    "github.com/anthonyabeo/obx/src/ir/desugar"
    "github.com/anthonyabeo/obx/src/ir/minir"
    "github.com/anthonyabeo/obx/src/project"
    "github.com/anthonyabeo/obx/src/sema"
    "github.com/anthonyabeo/obx/src/syntax/ast"
)

var cfgArgs struct {
    Path   string
    Target string
    Out    string // output path or "-" for stdout (when format=dot)
    Format string // dot|png|svg
    Fn     string // function name to dump; defaults to module entry or first lowered function
    Defines []string // --define (repeatable); NAME or NAME=VALUE
}

func init() {
    cfgCmd.Flags().StringVarP(&cfgArgs.Path, "path", "p", "", "source root directory (defaults to roots in obx.mod)")
    cfgCmd.Flags().StringVarP(&cfgArgs.Target, "target", "T", "rv64imafd", "target architecture (used to select the stdlib platform layer)")
    cfgCmd.Flags().StringVarP(&cfgArgs.Out, "out", "o", "", "output path for CFG; use '-' for stdout when format=dot")
    cfgCmd.Flags().StringVar(&cfgArgs.Format, "format", "dot", "output format: dot (default), png, or svg")
    cfgCmd.Flags().StringVar(&cfgArgs.Fn, "fn", "", "name of function to dump; defaults to module entry or first lowered function")
    cfgCmd.Flags().StringArrayVarP(&cfgArgs.Defines, "define", "d", nil,
        "set a compile-time directive constant: NAME (bool true) or NAME=VALUE (bool/int/float)")
}

var cfgCmd = &cobra.Command{
    Use:   "cfg",
    Short: "generate a control-flow graph (DOT/PNG/SVG) for a lowered function",
    Long: `Generate a CFG for a lowered function. The command runs parsing and semantic
analysis, lowers the program to minir, then emits a DOT, PNG, or SVG file for a
selected function. Use --fn to select a function by name; otherwise the module
entry or the first lowered function is used.`,
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
        if ok := parseModules(sorted, ctx, obx); !ok {
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
        fns := minir.Lower(prog)
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
        opts := minir.DotOptions{Title: target.FnName}
        switch format {
        case "dot", "":
            if cfgArgs.Out == "-" {
                fmt.Print(minir.FormatDOT(target))
            } else if cfgArgs.Out == "" {
                // print to stdout by default for dot
                fmt.Print(minir.FormatDOT(target))
            } else {
                f, err := os.Create(cfgArgs.Out)
                if err != nil {
                    fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
                    os.Exit(1)
                }
                defer f.Close()
                if err := minir.WriteDOT(f, target, opts); err != nil {
                    fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
                    os.Exit(1)
                }
            }
        case "png":
            if cfgArgs.Out == "" {
                fmt.Fprintf(os.Stderr, "cfg: --out required for png output\n")
                os.Exit(1)
            }
            if err := minir.SavePNG(target, opts, cfgArgs.Out); err != nil {
                fmt.Fprintf(os.Stderr, "cfg: %v\n", err)
                os.Exit(1)
            }
        case "svg":
            data, err := minir.RenderSVG(target, opts)
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
        default:
            fmt.Fprintf(os.Stderr, "cfg: unsupported format %q\n", format)
            os.Exit(1)
        }
        if cfgArgs.Out != "" {
            fmt.Fprintf(os.Stderr, "cfg: wrote %s (format=%s)\n", cfgArgs.Out, format)
        }
    },
}



