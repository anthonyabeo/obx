package web

import (
	"errors"
	"testing"

	"github.com/anthonyabeo/obx/src/backend/target"
)

func TestHostRunBackendConfigFor_DarwinArm64(t *testing.T) {
	look := func(file string) (string, error) {
		if file == "clang" {
			return "/usr/bin/clang", nil
		}
		return "", errors.New("not found")
	}

	tgt, tc, err := hostRunBackendConfigFor("darwin", "arm64", look)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tgt == nil || tgt.Name() != target.Arm64Name {
		t.Fatalf("unexpected target: %#v", tgt)
	}
	if tc.Assembler != "clang" || tc.Linker != "clang" {
		t.Fatalf("unexpected darwin toolchain: %+v", tc)
	}
	if len(tc.AssemblerArgs) != 2 || tc.AssemblerArgs[0] != "-arch" || tc.AssemblerArgs[1] != "arm64" {
		t.Fatalf("expected darwin assembler args, got: %+v", tc.AssemblerArgs)
	}
}

func TestHostRunBackendConfigFor_LinuxArm64(t *testing.T) {
	look := func(file string) (string, error) {
		if file == "clang" {
			return "/usr/bin/clang", nil
		}
		return "", errors.New("not found")
	}

	tgt, tc, err := hostRunBackendConfigFor("linux", "arm64", look)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tgt == nil || tgt.Name() != target.Arm64Name {
		t.Fatalf("unexpected target: %#v", tgt)
	}
	if tc.Assembler != "clang" || tc.Linker != "clang" {
		t.Fatalf("unexpected linux toolchain: %+v", tc)
	}
	if len(tc.AssemblerArgs) != 0 || len(tc.LinkerArgs) != 0 {
		t.Fatalf("expected empty linux args, got assembler=%+v linker=%+v", tc.AssemblerArgs, tc.LinkerArgs)
	}
}

func TestHostRunBackendConfigFor_LinuxArm64FallbackGNU(t *testing.T) {
	look := func(file string) (string, error) {
		if file == "aarch64-linux-gnu-gcc" {
			return "/usr/bin/aarch64-linux-gnu-gcc", nil
		}
		return "", errors.New("not found")
	}

	_, tc, err := hostRunBackendConfigFor("linux", "arm64", look)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tc.Assembler != "aarch64-linux-gnu-gcc" || tc.Linker != "aarch64-linux-gnu-gcc" {
		t.Fatalf("unexpected fallback linux toolchain: %+v", tc)
	}
}

func TestHostRunBackendConfigFor_UnsupportedHost(t *testing.T) {
	look := func(file string) (string, error) {
		return "", errors.New("not found")
	}
	if _, _, err := hostRunBackendConfigFor("linux", "amd64", look); err == nil {
		t.Fatal("expected unsupported host arch error")
	}
}
