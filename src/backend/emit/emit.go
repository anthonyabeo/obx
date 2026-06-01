package emit

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"sync"

	zlog "github.com/rs/zerolog/log"

	"github.com/anthonyabeo/obx/src/backend/mir"
	"github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/project"
)

// Toolchain names the external assembler and linker used by the backend emit
// stage. Empty command names disable the corresponding external step, which is
// useful for tests and dry runs.
type Toolchain struct {
	Assembler     string
	AssemblerArgs []string
	Linker        string
	LinkerArgs    []string
}

// Config controls backend emission for a lowered MIR program.
type Config struct {
	ProjectDir string
	BuildDir   string
	Output     string
	Manifest   project.Manifest

	Target    target.Target
	Toolchain Toolchain

	PrintAsm bool
	KeepAsm  bool
	KeepObj  bool
}

// Emitter owns the assemble/link lifecycle for one pipeline run.
type Emitter struct {
	cfg       Config
	buildDir  string
	asmPaths  []string
	objPaths  []string
	linkLibs  map[string]struct{}
	assembled bool
	mu        sync.Mutex
}

// New constructs a backend emitter with defaulted paths and empty state.
func New(cfg Config) *Emitter {
	cfg = defaultConfig(cfg)
	return &Emitter{
		cfg:      cfg,
		buildDir: cfg.BuildDir,
		linkLibs: make(map[string]struct{}),
	}
}

// Assemble lowers the backend MIR to target assembly, writes .s files, and
// optionally invokes the external assembler to produce .o files.
func (e *Emitter) Assemble(prog *mir.Program) error {
	if e == nil {
		return fmt.Errorf("backend emit: nil emitter")
	}
	e.mu.Lock()
	defer e.mu.Unlock()

	if prog == nil {
		return fmt.Errorf("backend emit: nil MIR program")
	}
	if e.cfg.Target == nil {
		return fmt.Errorf("backend emit: nil target")
	}

	e.resetRunState()
	if err := os.MkdirAll(e.buildDir, 0o755); err != nil {
		return fmt.Errorf("backend emit: create build dir: %w", err)
	}

	for _, mod := range prog.Modules {
		if mod == nil {
			continue
		}
		asmText := e.cfg.Target.Emit(mod)
		if e.cfg.PrintAsm {
			fmt.Println(asmText)
		}

		asmPath := filepath.Join(e.buildDir, mod.Name+".s")
		if err := os.WriteFile(asmPath, []byte(asmText+"\n"), 0o644); err != nil {
			return fmt.Errorf("backend emit: write %s: %w", asmPath, err)
		}
		e.asmPaths = append(e.asmPaths, asmPath)

		objPath := filepath.Join(e.buildDir, mod.Name+".o")
		if err := e.assembleObject(asmPath, objPath); err != nil {
			return err
		}
		e.objPaths = append(e.objPaths, objPath)

		for _, ext := range mod.Externals {
			if ext.DLLName == "" {
				continue
			}
			e.linkLibs[ext.DLLName] = struct{}{}
		}
	}

	e.assembled = true
	return nil
}

// Link invokes the configured linker over the assembled object files and then
// applies the requested cleanup policy.
func (e *Emitter) Link(_ *mir.Program) error {
	if e == nil {
		return fmt.Errorf("backend emit: nil emitter")
	}
	e.mu.Lock()
	defer e.mu.Unlock()

	if !e.assembled {
		return fmt.Errorf("backend emit: link requested before assemble")
	}
	outputPath := resolveOutputPath(e.cfg.ProjectDir, e.cfg.Manifest, e.cfg.Target.Name(), e.cfg.Output, e.buildDir)
	if err := os.MkdirAll(filepath.Dir(outputPath), 0o755); err != nil {
		return fmt.Errorf("backend emit: create output dir: %w", err)
	}

	flagsPath := filepath.Join(e.buildDir, "link.flags")
	flags := e.linkFlags()
	if err := os.WriteFile(flagsPath, []byte(strings.Join(flags, "\n")+"\n"), 0o644); err != nil {
		zlog.Warn().Err(err).Str("file", flagsPath).Msg("backend emit: failed to write link.flags")
	} else {
		fmt.Printf("Linker flags written to %s\n", flagsPath)
	}

	if e.cfg.Toolchain.Linker != "" {
		args := append([]string{}, e.cfg.Toolchain.LinkerArgs...)
		args = append(args, "-o", outputPath)
		args = append(args, e.objPaths...)
		args = append(args, flags...)
		cmd := exec.Command(e.cfg.Toolchain.Linker, args...)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err != nil {
			return fmt.Errorf("backend emit: link failed: %w", err)
		}
	} else {
		if err := os.WriteFile(outputPath, []byte{}, 0o755); err != nil {
			return fmt.Errorf("backend emit: write placeholder output: %w", err)
		}
	}

	fmt.Printf("Executable written to %s\n", outputPath)
	e.cleanup()
	return nil
}

func (e *Emitter) resetRunState() {
	e.asmPaths = e.asmPaths[:0]
	e.objPaths = e.objPaths[:0]
	for k := range e.linkLibs {
		delete(e.linkLibs, k)
	}
	e.assembled = false
}

func (e *Emitter) cleanup() {
	if !e.cfg.KeepAsm {
		for _, p := range e.asmPaths {
			if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
				zlog.Warn().Err(err).Str("file", p).Msg("backend emit: failed to remove assembly file")
			}
		}
	}
	if !e.cfg.KeepObj {
		for _, p := range e.objPaths {
			if err := os.Remove(p); err != nil && !os.IsNotExist(err) {
				zlog.Warn().Err(err).Str("file", p).Msg("backend emit: failed to remove object file")
			}
		}
	}
}

func (e *Emitter) assembleObject(asmPath, objPath string) error {
	if e.cfg.Toolchain.Assembler == "" {
		return os.WriteFile(objPath, []byte{}, 0o644)
	}

	args := append([]string{}, e.cfg.Toolchain.AssemblerArgs...)
	args = append(args, "-c", asmPath, "-o", objPath)
	cmd := exec.Command(e.cfg.Toolchain.Assembler, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("backend emit: assemble %s: %w", asmPath, err)
	}
	return nil
}

func (e *Emitter) linkFlags() []string {
	if len(e.linkLibs) == 0 {
		return nil
	}
	libs := make([]string, 0, len(e.linkLibs))
	for lib := range e.linkLibs {
		norm, keep := e.normalizeLinkLibrary(lib)
		if !keep || norm == "" {
			continue
		}
		libs = append(libs, "-l"+norm)
	}
	sort.Strings(libs)
	return libs
}

func (e *Emitter) normalizeLinkLibrary(lib string) (string, bool) {
	name := canonicalLinkLibraryName(lib)
	if name == "" {
		return "", false
	}

	if isDarwinTarget(e.cfg.Target) {
		if name == "c" {
			if usesClangDriver(e.cfg.Toolchain.Linker) {
				return "", false // clang driver already links libSystem on Darwin
			}
			return "System", true // direct ld expects libSystem
		}
	}

	return name, true
}

func canonicalLinkLibraryName(lib string) string {
	name := strings.TrimSpace(lib)
	if name == "" {
		return ""
	}
	name = strings.TrimPrefix(name, "-l")
	lower := strings.ToLower(name)
	if strings.HasSuffix(lower, ".dylib") || strings.HasSuffix(lower, ".so") || strings.HasSuffix(lower, ".a") {
		name = name[:strings.LastIndex(name, ".")]
		lower = strings.ToLower(name)
	}
	if strings.HasPrefix(lower, "lib") && len(name) > 3 {
		name = name[3:]
	}
	return name
}

func isDarwinTarget(t target.Target) bool {
	if t == nil {
		return false
	}
	name := strings.ToLower(strings.TrimSpace(t.Name()))
	return strings.Contains(name, "darwin") || strings.Contains(name, "macos")
}

func usesClangDriver(linker string) bool {
	base := strings.ToLower(filepath.Base(strings.TrimSpace(linker)))
	return strings.Contains(base, "clang")
}

func defaultConfig(cfg Config) Config {
	if cfg.BuildDir == "" {
		if cfg.ProjectDir != "" {
			cfg.BuildDir = filepath.Join(cfg.ProjectDir, "build")
		} else {
			cfg.BuildDir = "build"
		}
	}
	return cfg
}

func resolveOutputPath(projectDir string, manifest project.Manifest, targetName, explicit, buildDir string) string {
	if strings.TrimSpace(explicit) == "" {
		base := defaultExecutableName(manifest, targetName)
		if buildDir == "" {
			buildDir = filepath.Join(projectDir, "build")
		}
		return filepath.Join(buildDir, base)
	}

	if filepath.IsAbs(explicit) {
		return explicit
	}

	if strings.ContainsRune(explicit, filepath.Separator) {
		return filepath.Join(projectDir, explicit)
	}

	if buildDir == "" {
		buildDir = filepath.Join(projectDir, "build")
	}
	return filepath.Join(buildDir, explicit)
}

func defaultExecutableName(manifest project.Manifest, targetName string) string {
	base := normalizeExecutableBase(manifest.Name)
	if base == "" {
		base = "obx"
	}
	if strings.Contains(strings.ToLower(targetName), "windows") && !strings.HasSuffix(strings.ToLower(base), ".exe") {
		return base + ".exe"
	}
	return base
}

func normalizeExecutableBase(name string) string {
	name = strings.TrimSpace(name)
	if name == "" {
		return "obx"
	}

	var b strings.Builder
	lastUnderscore := false
	for _, r := range name {
		switch {
		case r >= 'a' && r <= 'z', r >= 'A' && r <= 'Z', r >= '0' && r <= '9':
			b.WriteRune(r)
			lastUnderscore = false
		case r == '-' || r == '_' || r == '.' || r == ' ' || r == '\t':
			if !lastUnderscore {
				b.WriteRune('_')
				lastUnderscore = true
			}
		default:
			if !lastUnderscore {
				b.WriteRune('_')
				lastUnderscore = true
			}
		}
	}

	out := strings.Trim(b.String(), "_")
	if out == "" {
		return "obx"
	}
	return out
}
