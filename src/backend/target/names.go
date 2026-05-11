package target

// Canonical backend target names and compatibility aliases.
const (
	Arm64Name              = "arm64"
	Arm64AppleMacosName    = "arm64-apple-macos"
	AArch64AppleDarwinName = "aarch64-apple-darwin"

	// RV64IMAFDName is the canonical RISC-V backend target name.
	RV64IMAFDName = "rv64imafd"

	// Compatibility aliases still accepted by Lookup and CLI helpers.
	RISCVAliasName    = "riscv"
	RISCV64AliasName   = "riscv64"
)

