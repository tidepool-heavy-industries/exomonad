{
  description = "ExoMonad - Development environment for ExoMonad LLM agent framework";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    tidepool.url = "path:../tidepool";
    tidepool.inputs.nixpkgs.follows = "nixpkgs";
    tidepool.inputs.rust-overlay.follows = "rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta, rust-overlay, tidepool }:
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
          nodejs_20 # For quicktype codegen
          protobuf  # For proto codegen (prost/proto3-suite)
        ];

        # Proto3-suite code generator for Haskell
        # Note: GHC 9.12 version has broken deps in nixpkgs, use GHC 9.6 for codegen
        # Generated code is pure Haskell and works with any GHC version
        proto3SuitePkg = pkgs.haskell.packages.ghc96.proto3-suite;

        # Haskell toolchain
        haskellPkgs = ghcVersion: with pkgs; [
          haskell.compiler.${ghcVersion}
          cabal-install
          haskell-language-server
          ormolu
          zlib
          zlib.dev
          pkg-config
        ];

        # Rust toolchain (latest stable for edition 2024 support)
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
          targets = [ "wasm32-wasip1" ];
        };
        rustPkgs = [ rustToolchain ];

        # Orchestration tools
        orchestrationPkgs = with pkgs; [
          zellij
          jujutsu  # jj
        ];

        # Tidepool: Haskell Core → Cranelift compiler
        tidepoolPkgs = [
          tidepool.packages.${system}.tidepool-extract
        ];

      in {
        devShells = {
          # Default: Full development environment
          default = pkgs.mkShell {
            packages = commonPkgs
              ++ haskellPkgs "ghc912"
              ++ rustPkgs
              ++ orchestrationPkgs
              ++ tidepoolPkgs
              ++ [ pkgs.sqlite ]
              ++ [ proto3SuitePkg ];  # For compile-proto-file (Haskell proto codegen)

            shellHook = ''
              # nix mkShell overrides TMPDIR to /tmp/nix-shell.*, which breaks
              # Zellij socket discovery (sockets live under macOS native TMPDIR).
              # Restore native TMPDIR — devShells are interactive, not hermetic builds.
              if [[ "$TMPDIR" == /tmp/nix-shell.* ]]; then
                export TMPDIR="$(getconf DARWIN_USER_TEMP_DIR 2>/dev/null || echo /tmp)"
              fi

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
              echo ""
              echo "Paths:"
              echo "  EXOMONAD_ROOT: $EXOMONAD_ROOT"
              echo ""
              echo "Commands:"
              echo "  just install-all-dev   Full build (WASM + Rust + install)"
              echo "  just wasm-all          Build WASM plugins only"
              echo "  just proto-gen         Generate proto code (Rust + Haskell)"
              echo ""
              echo "First time? Run: just wasm-setup"
              echo ""
            '';
          };

          # WASM: Cross-compilation to WebAssembly
          wasm = pkgs.mkShell {
            packages = [
              wasmPkgs.all_9_12  # GHC 9.12 WASM toolchain (matches native)
              pkgs.wizer
            ] ++ commonPkgs;

            shellHook = ''
              echo "╔═══════════════════════════════════════════════════════════╗"
              echo "║            WASM Cross-Compilation Shell (GHC 9.12)        ║"
              echo "╚═══════════════════════════════════════════════════════════╝"
              echo ""
              echo "Tools available:"
              echo "  wasm32-wasi-ghc --version"
              echo "  wasm32-wasi-cabal build --project-file=cabal.project.wasm wasm-guest"
              echo ""
              echo "Build all roles:"
              echo "  just wasm-all"
              echo ""
              echo "First time? Run: just wasm-setup"
              echo ""
            '';
          };

        };

        packages = {
          default = pkgs.writeShellScriptBin "exomonad-env-check" ''
            echo "ExoMonad environment check"
            echo "Run 'nix develop' to enter the development shell"
          '';
        };
      }
    );
}
