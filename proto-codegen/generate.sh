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
rm -rf haskell/proto/src/ExoMonad/Effects

mkdir -p haskell/proto/src/ExoMonad
mkdir -p haskell/proto/src/ExoMonad/Effects

# Generate from core proto files
for proto in proto/exomonad/*.proto; do
    proto_rel="${proto#proto/}"
    echo "    Processing: $proto_rel"
    $COMPILE --includeDir proto --proto "$proto_rel" --out haskell/proto/src
done

# Generate from effects proto files
for proto in proto/effects/*.proto; do
    proto_rel="${proto#proto/}"
    echo "    Processing: $proto_rel"
    $COMPILE --includeDir proto --proto "$proto_rel" --out haskell/proto/src
done

echo ">>> Post-processing generated files..."

# Process all generated files (core + effects)
find haskell/proto/src -name '*.hs' | while read -r f; do
    [[ -f "$f" ]] || continue
    base=$(basename "$f")
    [[ "$base" == "Compat.hs" ]] && continue

    # Strip ToSchema instances (not compatible with WASM, requires swagger)
    echo "    Stripping ToSchema from: $f"
    perl -i -0pe 's/instance \(HsJSONPB\.ToSchema[^}]+\}[^}]*\}\n?//gs' "$f"
    # Remove any stray closing braces left after stripping
    perl -i -pe 's/^}\n?// if /^}$/' "$f"

    # Strip gRPC imports and service code (not compatible with WASM)
    # Remove gRPC import blocks (including multi-line continuations like "hiding (serverLoop)")
    perl -i -0pe 's/import Network\.GRPC\.HighLevel\.\S+.*?(?=\n(?:import |newtype |data |class ))//gs' "$f"
    # Remove everything from the service record to end of file
    # (service records are always at the end, after all message types)
    perl -i -0pe 's/\ndata \w+Effects request response\b.*\z/\n/s' "$f"

    # Fix module names: Exomonad -> ExoMonad (proto3-suite lowercases)
    sed -i.bak 's/Exomonad\./ExoMonad./g' "$f"

    rm -f "${f}.bak"
done

# Move files from Exomonad/ to ExoMonad/ if proto3-suite created wrong directory.
# On macOS (case-insensitive FS), Exomonad/ and ExoMonad/ are the same dir —
# the sed fixup already corrected module names in-place, so mv is a no-op.
# We test with a temp file to detect case-insensitive FS.
if [ -d "haskell/proto/src/Exomonad" ]; then
    _test_file="haskell/proto/src/Exomonad/.case_test_$$"
    touch "$_test_file"
    if [ -f "haskell/proto/src/ExoMonad/.case_test_$$" ]; then
        # Case-insensitive: same directory, skip move
        echo ">>> Case-insensitive FS detected, skipping Exomonad → ExoMonad move"
        rm -f "$_test_file"
    else
        # Case-sensitive: actually need to move
        rm -f "$_test_file"
        echo ">>> Moving files from Exomonad/ to ExoMonad/..."
        for f in haskell/proto/src/Exomonad/*.hs; do
            [[ -f "$f" ]] || continue
            mv "$f" "haskell/proto/src/ExoMonad/$(basename "$f")"
        done
        if [ -d "haskell/proto/src/Exomonad/Effects" ]; then
            mkdir -p haskell/proto/src/ExoMonad/Effects
            for f in haskell/proto/src/Exomonad/Effects/*.hs; do
                [[ -f "$f" ]] || continue
                mv "$f" "haskell/proto/src/ExoMonad/Effects/$(basename "$f")"
            done
            rmdir haskell/proto/src/Exomonad/Effects 2>/dev/null || true
        fi
        rmdir haskell/proto/src/Exomonad 2>/dev/null || true
    fi
fi

echo ">>> Generated files:"
ls -la haskell/proto/src/ExoMonad/*.hs
if ls haskell/proto/src/Effects/*.hs &>/dev/null; then
    ls -la haskell/proto/src/Effects/*.hs
fi

echo ""
echo ">>> Done! Remember to commit the generated files."
echo ""
echo "Next steps:"
echo "  1. cabal build exomonad-proto   # Verify Haskell builds"
echo "  2. cargo build -p exomonad-proto # Verify Rust builds"
echo "  3. just proto-test               # Run wire format tests"
