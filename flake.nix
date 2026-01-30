{
  description = "ExoMonad - Development environment for ExoMonad LLM agent framework";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };
        wasmPkgs = ghc-wasm-meta.packages.${system};

        # Common packages shared across shells
        commonPkgs = with pkgs; [
          # Dev utilities
          jq
          just
          curl
          git
        ];

        # Haskell toolchain
        haskellPkgs = ghcVersion: with pkgs; [
          haskell.compiler.${ghcVersion}
          cabal-install
          haskell-language-server
          zlib
          zlib.dev
          pkg-config
        ];

        # Rust toolchain (latest stable for edition 2024 support)
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
        };
        rustPkgs = [ rustToolchain ];

        # Node.js toolchain
        nodePkgs = with pkgs; [
          nodejs_20
          pnpm
        ];

        # Orchestration tools
        orchestrationPkgs = with pkgs; [
          zellij
        ];

      in {
        devShells = {
          # Default: Full development environment
          default = pkgs.mkShell {
            packages = commonPkgs
              ++ haskellPkgs "ghc912"
              ++ rustPkgs
              ++ nodePkgs
              ++ orchestrationPkgs
              ++ [ pkgs.sqlite ];

            shellHook = ''
              export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
              export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
              export EXOMONAD_ROOT="$PWD"

              echo "╔═══════════════════════════════════════════════════════════╗"
              echo "║              ExoMonad Development Shell                   ║"
              echo "╚═══════════════════════════════════════════════════════════╝"
              echo ""
              echo "  GHC:      $(ghc --numeric-version)"
              echo "  Cabal:    $(cabal --numeric-version)"
              echo "  Cargo:    $(cargo --version | cut -d' ' -f2)"
              echo "  Node.js:  $(node --version)"
              echo "  pnpm:     $(pnpm --version)"
              echo ""
              echo "Paths:"
              echo "  EXOMONAD_ROOT: $EXOMONAD_ROOT"
              echo ""
              echo "Commands:"
              echo "  cabal build all      Build Haskell packages"
              echo "  ./ide              Start Claude Code++ session"
              echo ""
              echo "Other shells:"
              echo "  nix develop .#worker            Cloudflare Worker dev"
              echo "  nix develop .#wasm              WASM cross-compilation"
              echo ""
            '';
          };

          # Worker: Cloudflare Worker development (TypeScript only)
          worker = pkgs.mkShell {
            packages = commonPkgs ++ nodePkgs;

            shellHook = ''
              echo "╔═══════════════════════════════════════════════════════════╗"
              echo "║         Cloudflare Worker Development Shell               ║"
              echo "╚═══════════════════════════════════════════════════════════╝"
              echo ""
              echo "  Node.js: $(node --version)"
              echo "  pnpm:    $(pnpm --version)"
              echo ""
              echo "Setup:"
              echo "  cd repo/deploy && pnpm install"
              echo ""
              echo "Development:"
              echo "  pnpm dev      # wrangler dev (local)"
              echo "  pnpm deploy   # wrangler deploy (production)"
              echo ""
            '';
          };

          # WASM: Cross-compilation to WebAssembly
          wasm = pkgs.mkShell {
            packages = [
              wasmPkgs.all_9_12  # GHC 9.12 WASM toolchain (matches native)
              pkgs.wizer
            ] ++ commonPkgs ++ nodePkgs;

            shellHook = ''
              echo "╔═══════════════════════════════════════════════════════════╗"
              echo "║            WASM Cross-Compilation Shell (GHC 9.12)        ║"
              echo "╚═══════════════════════════════════════════════════════════╝"
              echo ""
              echo "Tools available:"
              echo "  wasm32-wasi-ghc --version"
              echo "  wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest"
              echo ""
              echo "Build wasm-guest:"
              echo "  wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest"
              echo ""
              echo "Generate WASM freeze file:"
              echo "  wasm32-wasi-cabal freeze --project-file=cabal.project.wasm"
              echo ""
            '';
          };

          # Minimal: Just Haskell + HLS for quick edits
          minimal = pkgs.mkShell {
            packages = commonPkgs ++ haskellPkgs "ghc912";

            shellHook = ''
              export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
              export NIX_GHC_LIBDIR="$(ghc --print-libdir)"

              echo "ExoMonad Minimal Shell (Haskell only)"
              echo "  GHC: $(ghc --numeric-version)"
              echo "  HLS: available"
              echo ""
            '';
          };
        };

        # Allow `nix build` to produce a simple check
        packages.default = pkgs.writeShellScriptBin "exomonad-env-check" ''
          echo "ExoMonad environment check"
          echo "Run 'nix develop' to enter the development shell"
        '';
      }
    );
}
