package cli

import (
	"path/filepath"
	"testing"

	bmir "github.com/anthonyabeo/obx/src/backend/mir"
	btarget "github.com/anthonyabeo/obx/src/backend/target"
	"github.com/anthonyabeo/obx/src/ir/minir"
	"github.com/anthonyabeo/obx/src/project"
)

type fakeToolchainTarget struct{ *btarget.BaseTarget }

func newFakeToolchainTarget(name string) btarget.Target {
	return &fakeToolchainTarget{BaseTarget: btarget.NewBaseTarget(name, btarget.ABI{})}
}

func (f *fakeToolchainTarget) Emit(_ *bmir.Module) string { return "" }

func TestNormalizeExecutableBase(t *testing.T) {
	cases := []struct {
		in   string
		want string
	}{
		{"Calculator", "Calculator"},
		{"my project", "my_project"},
		{"tool@v2", "tool_v2"},
		{"---", "obx"},
		{"", "obx"},
	}

	for _, tc := range cases {
		if got := normalizeExecutableBase(tc.in); got != tc.want {
			t.Fatalf("normalizeExecutableBase(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}

func TestDefaultExecutableName(t *testing.T) {
	m := project.Manifest{Name: "my project"}
	if got, want := defaultExecutableName(m, btarget.RV64IMAFDName), "my_project"; got != want {
		t.Fatalf("defaultExecutableName(rv64) = %q, want %q", got, want)
	}
	if got, want := defaultExecutableName(m, btarget.RV64IMAFDName+"-windows"), "my_project.exe"; got != want {
		t.Fatalf("defaultExecutableName(windows) = %q, want %q", got, want)
	}
}

func TestResolveOutputPath(t *testing.T) {
	m := project.Manifest{Name: "calc"}
	projectDir := string(filepath.Separator) + "tmp" + string(filepath.Separator) + "proj"

	if got, want := resolveOutputPath(projectDir, m, btarget.RV64IMAFDName, ""), filepath.Join(projectDir, "build", "calc"); got != want {
		t.Fatalf("resolveOutputPath(default) = %q, want %q", got, want)
	}
	if got, want := resolveOutputPath(projectDir, m, btarget.RV64IMAFDName, "dist/app"), filepath.Join(projectDir, "dist", "app"); got != want {
		t.Fatalf("resolveOutputPath(relative) = %q, want %q", got, want)
	}
	if got, want := resolveOutputPath(projectDir, m, btarget.RV64IMAFDName, filepath.Join(string(filepath.Separator), "opt", "app")), filepath.Join(string(filepath.Separator), "opt", "app"); got != want {
		t.Fatalf("resolveOutputPath(absolute) = %q, want %q", got, want)
	}
}

func TestBuildToolchainFor(t *testing.T) {
	tc, err := buildToolchainFor(newFakeToolchainTarget(btarget.RV64IMAFDName))
	if err != nil {
		t.Fatalf("buildToolchainFor(rv64imafd): %v", err)
	}
	if tc.assembler != "riscv64-linux-gnu-gcc" || tc.linker != "riscv64-linux-gnu-gcc" {
		t.Fatalf("unexpected riscv toolchain: %+v", tc)
	}

	tc, err = buildToolchainFor(newFakeToolchainTarget(btarget.Arm64Name))
	if err != nil {
		t.Fatalf("buildToolchainFor(arm64 canonical): %v", err)
	}
	if tc.assembler != "clang" || tc.linker != "clang" {
		t.Fatalf("unexpected arm64 canonical toolchain: %+v", tc)
	}

	tc, err = buildToolchainFor(newFakeToolchainTarget(btarget.Arm64AppleMacosName))
	if err != nil {
		t.Fatalf("buildToolchainFor(arm64): %v", err)
	}
	if tc.assembler != "clang" || tc.linker != "clang" {
		t.Fatalf("unexpected arm64 toolchain: %+v", tc)
	}
	if got, want := len(tc.assemblerArgs), 2; got != want {
		t.Fatalf("arm64 assemblerArgs len = %d, want %d", got, want)
	}
	if got, want := tc.assemblerArgs[0], "-arch"; got != want {
		t.Fatalf("arm64 assemblerArgs[0] = %q, want %q", got, want)
	}
	if got, want := tc.assemblerArgs[1], "arm64"; got != want {
		t.Fatalf("arm64 assemblerArgs[1] = %q, want %q", got, want)
	}
}

func TestMergePrecompiledMinirModules(t *testing.T) {
	lowered := &minir.Program{Modules: []*minir.Module{{Name: "Main"}}}
	preBundles := map[string]*minir.Module{
		"Main": {Name: "Main"},
		"IO":   {Name: "IO"},
	}

	mergePrecompiledMinirModules(lowered, preBundles)

	if got, want := len(lowered.Modules), 2; got != want {
		t.Fatalf("len(lowered.Modules) = %d, want %d", got, want)
	}
	if lowered.Modules[0].Name != "Main" || lowered.Modules[1].Name != "IO" {
		t.Fatalf("unexpected module order after merge: %+v", lowered.Modules)
	}
}

func TestSplitParsedHeaders(t *testing.T) {
	headers := []project.Header{
		{Key: project.NewKey("A")},
		{Key: project.NewKey("B")},
		{Key: project.NewKey("C")},
	}
	loaded := map[string]bool{"B": true}

	toParse, skipped := splitParsedHeaders(headers, loaded)

	if got, want := len(toParse), 2; got != want {
		t.Fatalf("len(toParse) = %d, want %d", got, want)
	}
	if got, want := len(skipped), 1; got != want {
		t.Fatalf("len(skipped) = %d, want %d", got, want)
	}
	if toParse[0].Key.Name() != "A" || toParse[1].Key.Name() != "C" {
		t.Fatalf("unexpected toParse order: %+v", toParse)
	}
	if skipped[0] != "B" {
		t.Fatalf("unexpected skipped list: %+v", skipped)
	}
}

func TestDedupMinirExternals(t *testing.T) {
	sig := &minir.FunctionType{Params: []minir.Type{minir.I32()}, Result: minir.I32()}
	shared := &minir.ExternalFunc{Name: "printf", Sig: sig, Attrs: &minir.ExternalAttrs{CName: "printf", DLLName: "c"}}
	dup := &minir.ExternalFunc{Name: "printf", Sig: sig, Attrs: &minir.ExternalAttrs{CName: "printf", DLLName: "c"}}
	other := &minir.ExternalFunc{Name: "puts", Sig: sig, Attrs: &minir.ExternalAttrs{CName: "puts", DLLName: "c"}}

	prog := &minir.Program{Modules: []*minir.Module{
		{Name: "A", Externals: []*minir.ExternalFunc{shared, other}},
		{Name: "B", Externals: []*minir.ExternalFunc{dup}},
	}}

	dedupMinirExternals(prog)

	if got, want := len(prog.Modules[0].Externals), 2; got != want {
		t.Fatalf("module A extern count = %d, want %d", got, want)
	}
	if got, want := len(prog.Modules[1].Externals), 1; got != want {
		t.Fatalf("module B extern count = %d, want %d", got, want)
	}
	if prog.Modules[0].Externals[0] != prog.Modules[1].Externals[0] {
		t.Fatalf("expected duplicate externs to be canonicalized to the same pointer")
	}
}
