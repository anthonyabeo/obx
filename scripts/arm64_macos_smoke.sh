#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: arm64_macos_smoke.sh [--source-root PATH] [--entry NAME] [--output NAME] [--target TRIPLE] [--alias]

Runs an end-to-end build smoke test for Apple Silicon macOS:
  1) builds a tiny example with obx build
  2) checks the resulting binary is a Mach-O arm64 executable
  3) runs the binary
  4) optionally repeats the same check with the aarch64-apple-darwin alias
EOF
}

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SOURCE_ROOT="${SOURCE_ROOT:-examples/smoke}"
ENTRY_MODULE="${ENTRY_MODULE:-Main}"
TARGET="${TARGET:-arm64-apple-macos}"
OUTPUT_NAME="${OUTPUT_NAME:-obx-arm64-smoke}"
CHECK_ALIAS="${CHECK_ALIAS:-0}"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --source-root)
      SOURCE_ROOT="$2"
      shift 2
      ;;
    --entry)
      ENTRY_MODULE="$2"
      shift 2
      ;;
    --output)
      OUTPUT_NAME="$2"
      shift 2
      ;;
    --target)
      TARGET="$2"
      shift 2
      ;;
    --alias)
      CHECK_ALIAS="1"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

require() {
  command -v "$1" >/dev/null 2>&1 || {
    echo "Missing required command: $1" >&2
    exit 1
  }
}

require go
require file
require otool

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "Warning: this smoke test is intended for macOS." >&2
fi
if [[ "$(uname -m)" != "arm64" ]]; then
  echo "Warning: host architecture is not arm64; the binary may build but cannot be run natively here." >&2
fi

run_build() {
  local target_name="$1"
  local output="$2"
  local binary_path="$ROOT_DIR/build/$output"

  rm -f "$binary_path"

  echo "==> building target=$target_name source=$SOURCE_ROOT entry=$ENTRY_MODULE output=$binary_path"
  go run ./cmd/obx.go build \
    --target "$target_name" \
    -r "$SOURCE_ROOT" \
    -e "$ENTRY_MODULE" \
    -o "$output"

  if [[ ! -f "$binary_path" ]]; then
    echo "Expected binary was not produced: $binary_path" >&2
    exit 1
  fi

  echo "==> inspecting $binary_path"
  local file_output
  file_output="$(file "$binary_path")"
  echo "$file_output"
  if [[ "$file_output" != *"Mach-O"* || "$file_output" != *"arm64"* ]]; then
    echo "Binary does not look like an arm64 Mach-O executable." >&2
    exit 1
  fi

  echo "==> Mach-O header"
  otool -hv "$binary_path" | sed -n '1,8p'

  echo "==> running $binary_path"
  "$binary_path"
  echo "==> ok"
}

run_build "$TARGET" "$OUTPUT_NAME"

if [[ "$CHECK_ALIAS" == "1" ]]; then
  alias_output="${OUTPUT_NAME}-alias"
  if [[ "$TARGET" == "aarch64-apple-darwin" ]]; then
    alias_target="arm64-apple-macos"
  else
    alias_target="aarch64-apple-darwin"
  fi
  run_build "$alias_target" "$alias_output"
fi

