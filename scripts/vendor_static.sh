#!/usr/bin/env bash
# Vendors monaco-editor (min/vs) under cmd/web/static/js/monaco and
# downloads viz.js and full.render.js into cmd/web/static/js.
# Run this from the repository root.

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "Vendoring viz.js into cmd/web/static/js/..."
mkdir -p cmd/web/static/js
curl -fL -o cmd/web/static/js/viz.js  https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/viz.js
curl -fL -o cmd/web/static/js/full.render.js https://cdnjs.cloudflare.com/ajax/libs/viz.js/2.1.2/full.render.js

echo "Vendoring monaco-editor into cmd/web/static/js/monaco (recommended via npm)"
echo "This will create a temporary node_modules; you can delete it afterwards."

if command -v npm >/dev/null 2>&1; then
  tmpdir=$(mktemp -d)
  pushd "$tmpdir" >/dev/null
  npm init -y >/dev/null
  npm install monaco-editor@0.33.0 --no-audit --no-fund >/dev/null
  mkdir -p "$ROOT_DIR/cmd/web/static/js/monaco"
  cp -R node_modules/monaco-editor/min/vs "$ROOT_DIR/cmd/web/static/js/monaco/"
  popd >/dev/null
  rm -rf "$tmpdir"
  echo "monaco copied to cmd/web/static/js/monaco/vs"
else
  echo "npm not found. You can manually copy the monaco 'min/vs' tree into cmd/web/static/js/monaco/vs"
  echo "Or use wget to mirror:"
  echo "  wget -r -nH --cut-dirs=5 -P cmd/web/static/js/monaco https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.33.0/min/vs/"
  exit 1
fi

echo "Done. Verify files exist:"
ls -l cmd/web/static/js/viz.js cmd/web/static/js/full.render.js cmd/web/static/js/monaco/vs/loader.js || true

# fingerprint static assets (sha256 short) and write manifest.json mapping placeholders
manifest="cmd/web/static/manifest.json"
declare -A manifest_map

fingerprint_asset() {
  local key="$1"
  local src="$2"

  if [ ! -f "$src" ]; then
    echo "warning: missing asset for $key: $src" >&2
    return 0
  fi

  local dir base ext sum short out rel raw_base
  dir=$(dirname "$src")
  raw_base=$(basename "$src")
  base="$raw_base"
  ext=".${base##*.}"
  base="${base%$ext}"
  if [[ "$base" =~ ^(.+)\.[0-9a-f]{8,}$ ]]; then
    base="${BASH_REMATCH[1]}"
  fi

  sum=$(shasum -a 256 "$src" | awk '{print $1}')
  short=${sum:0:8}
  out="$dir/${base}.${short}${ext}"

  # Remove stale fingerprinted variants for this base/ext, keep source + current output.
  shopt -s nullglob
  for old in "$dir"/"$base".[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]"$ext"; do
    if [ "$old" != "$out" ]; then
      rm -f "$old"
    fi
  done
  shopt -u nullglob

  cp "$src" "$out"
  rel="/${out#cmd/web/static/}"
  manifest_map["$key"]="/static${rel}"
}

# Use canonical files when present; fall back to already-fingerprinted vendored files.
find_js_src() {
  local canonical="$1"
  local pattern="$2"
  if [ -f "$canonical" ]; then
    printf '%s\n' "$canonical"
    return
  fi
  shopt -s nullglob
  local matches=( $pattern )
  shopt -u nullglob
  if [ ${#matches[@]} -gt 0 ]; then
    printf '%s\n' "${matches[0]}"
  fi
}

viz_src=$(find_js_src "cmd/web/static/js/viz.js" "cmd/web/static/js/viz.[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f].js")
full_render_src=$(find_js_src "cmd/web/static/js/full.render.js" "cmd/web/static/js/full.render.[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f].js")

if [ -n "${viz_src:-}" ]; then
  fingerprint_asset "VIZ_JS" "$viz_src"
fi
if [ -n "${full_render_src:-}" ]; then
  fingerprint_asset "VIZ_FULL_RENDER_JS" "$full_render_src"
fi

fingerprint_asset "PLAYGROUND_JS" "cmd/web/static/js/playground.js"
fingerprint_asset "PLAYGROUND_CSS" "cmd/web/static/css/playground.css"
fingerprint_asset "APPLE_TOUCH_ICON" "cmd/web/static/favicon/apple-touch-icon.png"
fingerprint_asset "FAVICON_32" "cmd/web/static/favicon/favicon-32x32.png"
fingerprint_asset "FAVICON_16" "cmd/web/static/favicon/favicon-16x16.png"

# Keep monaco loader stable because upstream worker graph references static vs/* names.
manifest_map["MONACO_LOADER"]="/static/js/monaco/vs/loader.js"

echo "Writing manifest to $manifest"
printf "{\n" > "$manifest"
first=true
for k in "PLAYGROUND_CSS" "APPLE_TOUCH_ICON" "FAVICON_32" "FAVICON_16" "VIZ_JS" "VIZ_FULL_RENDER_JS" "PLAYGROUND_JS" "MONACO_LOADER"; do
  v=${manifest_map[$k]:-}
  if [ -n "$v" ]; then
    if [ "$first" = true ]; then
      first=false
    else
      printf ",\n" >> "$manifest"
    fi
    printf "  \"%s\": \"%s\"" "$k" "$v" >> "$manifest"
  fi
done
printf "\n}\n" >> "$manifest"

echo "Manifest contents:" && cat "$manifest" || true

echo "Now build the project to embed the static assets:"
echo "  go build -o obx ./cmd/obx"

echo "Run server: ./obx web"

