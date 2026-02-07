#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo ">>> Generating Haskell types from proto..."

# Requires: run from within `nix develop` which provides compile-proto-file
if ! command -v compile-proto-file &> /dev/null; then
    echo "ERROR: compile-proto-file not found."
    echo "Run this script from within 'nix develop' shell."
    exit 1
fi

COMPILE="compile-proto-file"

# Clean and regenerate (preserve Compat.hs - it's hand-written)
echo ">>> Cleaning generated files..."
for f in haskell/proto/src/ExoMonad/*.hs; do
    base=$(basename "$f")
    if [[ "$base" != "Compat.hs" ]]; then
        rm -f "$f"
    fi
done

mkdir -p haskell/proto/src/ExoMonad

# Generate from each proto file
for proto in proto/exomonad/*.proto; do
    proto_rel="${proto#proto/}"
    echo "    Processing: $proto_rel"
    $COMPILE --includeDir proto --proto "$proto_rel" --out haskell/proto/src
done

echo ">>> Post-processing generated files..."

for f in haskell/proto/src/ExoMonad/*.hs haskell/proto/src/Exomonad/*.hs 2>/dev/null; do
    [[ -f "$f" ]] || continue
    base=$(basename "$f")
    [[ "$base" == "Compat.hs" ]] && continue

    # Strip ToSchema instances (not compatible with WASM, requires swagger)
    # Pattern: instance (HsJSONPB.ToSchema ... where ... multi-line block
    echo "    Stripping ToSchema from: $base"

    # Use perl for multi-line regex (more reliable than sed)
    perl -i -0pe 's/instance \(HsJSONPB\.ToSchema[^}]+\}[^}]*\}//gs' "$f"

    # Fix module names: Exomonad -> ExoMonad (proto3-suite lowercases)
    sed -i.bak 's/^module Exomonad\./module ExoMonad./' "$f"
    sed -i.bak 's/import Exomonad\./import ExoMonad./' "$f"
    sed -i.bak 's/qualified Exomonad\./qualified ExoMonad./' "$f"

    # Fix type references
    sed -i.bak 's/Exomonad\.Ffi\./ExoMonad.Ffi./g' "$f"
    sed -i.bak 's/Exomonad\.Common\./ExoMonad.Common./g' "$f"
    sed -i.bak 's/Exomonad\.Hook\./ExoMonad.Hook./g' "$f"
    sed -i.bak 's/Exomonad\.Agent\./ExoMonad.Agent./g' "$f"
    sed -i.bak 's/Exomonad\.Popup\./ExoMonad.Popup./g' "$f"

    rm -f "${f}.bak"
done

# Move files from Exomonad/ to ExoMonad/ if proto3-suite created wrong directory
if [ -d "haskell/proto/src/Exomonad" ]; then
    echo ">>> Moving files from Exomonad/ to ExoMonad/..."
    for f in haskell/proto/src/Exomonad/*.hs; do
        [[ -f "$f" ]] || continue
        base=$(basename "$f")
        mv "$f" "haskell/proto/src/ExoMonad/$base"
    done
    rmdir haskell/proto/src/Exomonad 2>/dev/null || true
fi

# Also need to update Compat.hs module name if it's wrong
if grep -q "^module Exomonad\.Compat" haskell/proto/src/ExoMonad/Compat.hs 2>/dev/null; then
    echo ">>> Fixing Compat.hs module name..."
    sed -i.bak 's/^module Exomonad\.Compat/module ExoMonad.Compat/' haskell/proto/src/ExoMonad/Compat.hs
    rm -f haskell/proto/src/ExoMonad/Compat.hs.bak
fi

echo ">>> Generated files:"
ls -la haskell/proto/src/ExoMonad/*.hs

echo ""
echo ">>> Done! Remember to commit the generated files."
echo ""
echo "Next steps:"
echo "  1. cabal build exomonad-proto   # Verify Haskell builds"
echo "  2. cargo build -p exomonad-proto # Verify Rust builds"
echo "  3. just proto-test               # Run wire format tests"
