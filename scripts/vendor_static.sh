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

# fingerprint viz files (sha256 short) and write manifest.json mapping placeholders
manifest="cmd/web/static/manifest.json"
declare -A manifest_map
for f in cmd/web/static/js/viz.js cmd/web/static/js/full.render.js; do
  if [ -f "$f" ]; then
    sum=$(shasum -a 256 "$f" | awk '{print $1}')
    short=${sum:0:8}
    base=$(basename "$f" .js)
    dir=$(dirname "$f")
    new="$dir/${base}.${short}.js"
    mv "$f" "$new"
    if [ "$base" = "viz" ]; then
      manifest_map["VIZ_JS"]="/static/js/${base}.${short}.js"
    else
      # full.render has dot in name; use key FULL_RENDER_JS
      manifest_map["VIZ_FULL_RENDER_JS"]="/static/js/${base}.${short}.js"
    fi
  fi
done
# add monaco loader mapping (not fingerprinted)
manifest_map["MONACO_LOADER"]="/static/js/monaco/vs/loader.js"

echo "Writing manifest to $manifest"
printf "{\n" > "$manifest"
first=true
for k in "VIZ_JS" "VIZ_FULL_RENDER_JS" "MONACO_LOADER"; do
  v=${manifest_map[$k]}
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

