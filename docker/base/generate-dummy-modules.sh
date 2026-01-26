#!/bin/bash
# Generate dummy Haskell modules from .cabal files for dependency caching.
# Creates minimal module stubs with correct qualified names.
set -e

for cabal_file in $(find haskell -name '*.cabal'); do
    pkg_dir=$(dirname "$cabal_file")

    # Extract hs-source-dirs (default to src if not specified)
    src_dir=$(grep -i 'hs-source-dirs:' "$cabal_file" | head -1 | sed 's/.*hs-source-dirs:[[:space:]]*//' | awk '{print $1}')
    [ -z "$src_dir" ] && src_dir="src"

    # Extract module names (lines with 4+ leading spaces followed by qualified name)
    grep -E '^\s{4,}[A-Z][A-Za-z0-9.]*\s*$' "$cabal_file" | \
    sed 's/^[[:space:]]*//' | sed 's/[[:space:]]*$//' | \
    while read module; do
        [ -z "$module" ] && continue

        # Convert module name to path (ExoMonad.Control.Version -> ExoMonad/Control/Version.hs)
        module_path=$(echo "$module" | tr '.' '/').hs
        full_path="$pkg_dir/$src_dir/$module_path"

        mkdir -p "$(dirname "$full_path")"
        echo "module $module where" > "$full_path"
    done

    # Create Main.hs for executables
    grep -i 'main-is:' "$cabal_file" | sed 's/.*main-is:[[:space:]]*//' | \
    while read main_file; do
        main_file=$(echo "$main_file" | tr -d '[:space:]')
        [ -z "$main_file" ] && continue

        for try_dir in app exe bin src .; do
            full_path="$pkg_dir/$try_dir/$main_file"
            if [ ! -f "$full_path" ]; then
                mkdir -p "$(dirname "$full_path")"
                cat > "$full_path" << 'EOF'
module Main where
main :: IO ()
main = pure ()
EOF
                break
            fi
        done
    done
done

echo "Generated dummy modules for $(find haskell -name '*.cabal' | wc -l) packages"
