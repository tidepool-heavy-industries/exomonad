{
  description = "Tidepool - Type-safe LLM agent graphs with WASM deployment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        wasmPkgs = ghc-wasm-meta.packages.${system};
      in {
        devShells = {
          # Native development with HLS
          default = pkgs.mkShell {
            packages = with pkgs; [
              # Native Haskell toolchain
              haskell.compiler.ghc910
              cabal-install
              haskell-language-server

              # Runtime deps for HTTP clients
              zlib

              # Node/TypeScript (wrangler installed via pnpm in deploy/)
              nodejs_20
              pnpm

              # Dev utilities
              jq
              just
              sqlite
            ];

            shellHook = ''
              echo "Tidepool Development Shell"
              echo "=========================="
              echo ""
              echo "Native Haskell:"
              echo "  cabal build all"
              echo "  cabal test all"
              echo ""
              echo "TypeScript (deploy/):"
              echo "  cd deploy && pnpm install && pnpm dev"
              echo ""
              echo "For WASM builds: nix develop .#wasm"
              echo ""
            '';
          };

          # Worker development (TypeScript + wrangler)
          worker = pkgs.mkShell {
            packages = with pkgs; [
              nodejs_20
              pnpm
              jq
            ];

            shellHook = ''
              echo "Cloudflare Worker Development Shell"
              echo "===================================="
              echo ""
              echo "Setup:"
              echo "  cd deploy && pnpm install"
              echo ""
              echo "Development:"
              echo "  pnpm dev      # wrangler dev"
              echo "  pnpm deploy   # wrangler deploy"
              echo ""
            '';
          };

          # Claude Code++ (human-augmented sessions)
          claude-code-plus = pkgs.mkShell {
            packages = with pkgs; [
              # Native Haskell toolchain
              haskell.compiler.ghc912
              cabal-install

              # Rust toolchain for mantle-agent
              cargo
              rustc

              # Zellij for split-pane layout
              zellij

              # Runtime deps
              zlib

              # Dev utilities
              jq
            ];

            shellHook = ''
              echo "Claude Code++ Development Shell"
              echo "================================"
              echo ""

              # Detect repo root (git root or current dir)
              TIDEPOOL_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo "$PWD")
              export TIDEPOOL_ROOT

              # Set environment variables
              export MANTLE_CONTROL_HOST=127.0.0.1
              export MANTLE_CONTROL_PORT=7432

              echo "Environment:"
              echo "  TIDEPOOL_ROOT=$TIDEPOOL_ROOT"
              echo "  MANTLE_CONTROL_HOST=$MANTLE_CONTROL_HOST"
              echo "  MANTLE_CONTROL_PORT=$MANTLE_CONTROL_PORT"
              echo ""

              # Build control server (always, uses cabal cache)
              echo "Building Haskell control server..."
              if ! (cd "$TIDEPOOL_ROOT" && cabal build tidepool-control-server); then
                echo ""
                echo "ERROR: Control server build failed."
                echo "Fix the errors above and try again."
                echo "Or run manually: cabal build tidepool-control-server"
                return 1
              fi

              # Build mantle-agent if needed
              if [ ! -f "$TIDEPOOL_ROOT/rust/target/debug/mantle-agent" ]; then
                echo "Building Rust mantle-agent..."
                if ! (cd "$TIDEPOOL_ROOT/rust" && cargo build -p mantle-agent); then
                  echo ""
                  echo "ERROR: mantle-agent build failed."
                  echo "Fix the errors above and try again."
                  echo "Or run manually: cargo build -p mantle-agent"
                  return 1
                fi
              fi

              # Add mantle-agent to PATH
              export PATH="$TIDEPOOL_ROOT/rust/target/debug:$PATH"

              # Auto-copy Claude Code settings template if not exists
              if [ ! -f "$TIDEPOOL_ROOT/.claude/settings.local.json" ]; then
                if [ ! -f "$TIDEPOOL_ROOT/.claude/settings.local.json.template" ]; then
                  echo ""
                  echo "WARNING: Claude Code template not found."
                  echo "  Expected: .claude/settings.local.json.template"
                  echo "  Hooks will not work until you create .claude/settings.local.json"
                else
                  echo "Copying Claude Code settings template..."
                  mkdir -p "$TIDEPOOL_ROOT/.claude"
                  if cp "$TIDEPOOL_ROOT/.claude/settings.local.json.template" \
                        "$TIDEPOOL_ROOT/.claude/settings.local.json" 2>/dev/null; then
                    echo "  -> Created .claude/settings.local.json"
                  else
                    echo ""
                    echo "ERROR: Failed to copy template."
                    echo "Check permissions on .claude/ directory."
                    return 1
                  fi
                fi
              fi

              echo ""
              echo "Starting zellij session 'tidepool'..."
              echo ""

              # Generate zellij layout with correct cwd (KDL doesn't expand env vars)
              cat > /tmp/tidepool-layout-$$.kdl <<EOF
layout {
    pane split_direction="vertical" {
        pane size="40%" {
            name "control-server"
            cwd "$TIDEPOOL_ROOT"
            command "cabal"
            args "run" "tidepool-control-server"
            focus true
        }
        pane size="60%" {
            name "claude"
        }
    }
}
EOF

              # Check if zellij session exists
              if zellij list-sessions 2>/dev/null | grep -q "tidepool"; then
                echo "Attaching to existing session..."
                zellij attach tidepool
              else
                echo "Creating new session..."
                zellij --layout /tmp/tidepool-layout-$$.kdl --session tidepool
              fi
            '';
          };

          # WASM cross-compilation
          wasm = pkgs.mkShell {
            packages = [
              # GHC 9.10 for stability (9.14 is default but newer)
              wasmPkgs.all_9_10

              # wizer NOT bundled in ghc-wasm-meta
              pkgs.wizer

              # Node for TH external interpreter + wrangler
              pkgs.nodejs_20
              pkgs.pnpm

              # Dev utilities
              pkgs.jq
              pkgs.just
            ];

            shellHook = ''
              echo "WASM Cross-Compilation Shell"
              echo "============================="
              echo ""
              echo "Tools available:"
              echo "  wasm32-wasi-ghc --version"
              echo "  wasm32-wasi-cabal build tidepool-wasm"
              echo "  wasm32-wasi-ghci  (yes, it works!)"
              echo ""
              echo "Build pipeline:"
              echo "  wasm32-wasi-cabal build tidepool-wasm"
              echo "  # Output: dist-newstyle/.../tidepool-wasm.wasm"
              echo ""
              echo "NOTE: Using GHC 9.10 (not 9.14) for stability"
              echo "NOTE: Cabal 3.14 bundled (3.16 has WASM TH regression)"
              echo ""
            '';
          };
        };
      }
    );
}
