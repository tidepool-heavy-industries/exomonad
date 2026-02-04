#!/usr/bin/env bash
set -euo pipefail

ROLE="${1:-}"
if [ -z "$ROLE" ]; then
  echo "Usage: $0 <role>"
  exit 1
fi

# Validate ROLE (prevent path traversal/injection)
if [[ ! "$ROLE" =~ ^[a-zA-Z0-9_-]+$ ]]; then
  echo "Error: Invalid role name '$ROLE'. Must match ^[a-zA-Z0-9_-]+$"
  exit 1
fi

# Locate root
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
ROLE_DIR="$ROOT_DIR/.exomonad/roles/$ROLE"
GEN_DIR="$ROLE_DIR/gen"
DIST_DIR="$ROLE_DIR/dist"

# Cleanup trap
PROJ_FILE="cabal.project.role-$ROLE"
cleanup() {
  rm -f "$PROJ_FILE"
}
trap cleanup EXIT

if [ ! -d "$ROLE_DIR" ]; then
  echo "Error: Role directory $ROLE_DIR not found"
  exit 1
fi

mkdir -p "$GEN_DIR"
mkdir -p "$DIST_DIR"

echo "Generating Main.hs for role '$ROLE'..."

cat > "$GEN_DIR/Main.hs" <<EOF
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DataKinds #-}

module Main where

import ExoMonad.Guest.Tool.Runtime (hookHandler, listHandlerRecord, mcpHandlerRecord, wrapHandler, testHandler)
import Foreign.C.Types (CInt (..))
import Role (config, Tools(..))
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Types (RoleConfig(..))

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall handle_test_call :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ mcpHandlerRecord (tools config)

handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ listHandlerRecord @Tools

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler hookHandler

handle_test_call :: IO CInt
handle_test_call = wrapHandler testHandler

main :: IO ()
main = pure ()
EOF

echo "Generating Cabal file..."

cat > "$GEN_DIR/wasm-guest-$ROLE.cabal" <<EOF
cabal-version:      3.4
name:               wasm-guest-$ROLE
version:            0.1.0.0
build-type:         Simple

executable wasm-guest-$ROLE
    main-is:          Main.hs
    hs-source-dirs:   . ../
    
    default-language: GHC2021
    default-extensions:
        AllowAmbiguousTypes
        DataKinds
        DeriveAnyClass
        DeriveGeneric
        DerivingStrategies
        FlexibleContexts
        FlexibleInstances
        ForeignFunctionInterface
        GADTs
        LambdaCase
        OverloadedStrings
        ScopedTypeVariables
        TypeApplications
        TypeFamilies
        TypeOperators
        UndecidableInstances

    build-depends:
        base,
        wasm-guest,
        extism-pdk

    ghc-options:
        -no-hs-main
        -optl-mexec-model=reactor
        -optl-Wl,--export=hs_init
        -optl-Wl,--export=handle_mcp_call
        -optl-Wl,--export=handle_pre_tool_use
        -optl-Wl,--export=handle_list_tools
        -optl-Wl,--export=handle_test_call
        -optl-Wl,--allow-undefined
EOF

# Create temporary project file in ROOT
cat > "$PROJ_FILE" <<EOF
import: cabal.project.wasm
packages: .exomonad/roles/$ROLE/gen
EOF

echo "Building..."
cd "$ROOT_DIR"

# Check if wasm32-wasi-cabal is available
# Use --project-file=... (relative to current dir which is ROOT)
if ! command -v wasm32-wasi-cabal &> /dev/null; then
    echo "wasm32-wasi-cabal not found. Running via 'nix develop .#wasm'..."
    nix develop .#wasm --command wasm32-wasi-cabal build --project-file="$PROJ_FILE" "wasm-guest-$ROLE"
else
    wasm32-wasi-cabal build --project-file="$PROJ_FILE" "wasm-guest-$ROLE"
fi

# Find and copy artifact
# Note: we are in ROOT_DIR
WASM_FILE=$(find dist-newstyle -name "wasm-guest-$ROLE.wasm" -type f | head -n 1)
if [ -z "$WASM_FILE" ]; then
    echo "Error: Build failed, no WASM file found."
    exit 1
fi

cp "$WASM_FILE" "$DIST_DIR/wasm-guest-$ROLE.wasm"
echo "Build complete: $DIST_DIR/wasm-guest-$ROLE.wasm"

