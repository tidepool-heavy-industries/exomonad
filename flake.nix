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
          nodejs_20 # For quicktype codegen
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

        # Orchestration tools
        orchestrationPkgs = with pkgs; [
          zellij
        ];

        # Shared cabal config setup
        cabalConfig = ''
          export HOME=$TMPDIR
          export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt

          mkdir -p $HOME/.ghc-wasm/.cabal
          cat > $HOME/.ghc-wasm/.cabal/config <<EOF
repository hackage.haskell.org
  url: https://hackage.haskell.org/
  secure: True

library-profiling: False
executable-profiling: False
shared: True
executable-dynamic: False
EOF
        '';

        # 1. Dependency Derivation (Fixed-Output)
        # Only rebuilds when .cabal or cabal.project files change.
        mkWasmDeps = hash: pkgs.stdenv.mkDerivation {
          pname = "wasm-guest-deps";
          version = "0.1.0.0";

          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let 
                base = baseNameOf path;
                relPath = pkgs.lib.removePrefix (toString ./.) (toString path);
              in
              base == "cabal.project.wasm" ||
              base == "cabal.project" ||
              base == "cabal.project.freeze" ||
              pkgs.lib.hasSuffix ".cabal" base ||
              # Keep directory structure for local packages
              (type == "directory" && (
                base == "haskell" || 
                base == "wasm-guest" || 
                base == "vendor" || 
                base == "polysemy"
              ));
          };

          nativeBuildInputs = [ wasmPkgs.all_9_12 pkgs.cacert ];

          outputHashAlgo = "sha256";
          outputHashMode = "recursive";
          outputHash = hash;

          # Disable fixup to prevent patching shebangs with store paths,
          # which would create illegal references in a fixed-output derivation.
          dontFixup = true;
          dontPatchShebangs = true;

          buildPhase = ''
            ${cabalConfig}

            # Remove potential conflicting native project files
            rm -f cabal.project cabal.project.freeze

            # Setup project files
            cp cabal.project.wasm cabal.project.offline
            sed -i '/repository head.hackage/,/secure: True/d' cabal.project.offline

            echo "Updating package index..."
            # Update index using the project file so it learns about head.hackage
            wasm32-wasi-cabal update --project-file=cabal.project.offline

            echo "Fetching dependencies..."
            # Fetch dependencies so they are cached in the FOD
            # Use build --only-download to be compatible with the wrapper's injected flags
            wasm32-wasi-cabal build --project-file=cabal.project.offline --only-download all -j
          '';

          installPhase = ''
            mkdir -p $out
            cp -r $HOME/.ghc-wasm $out/ghc-wasm
          '';
        };

      in {
        devShells = {
          # Default: Full development environment
          default = pkgs.mkShell {
            packages = commonPkgs
              ++ haskellPkgs "ghc912"
              ++ rustPkgs
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
              echo ""
              echo "Paths:"
              echo "  EXOMONAD_ROOT: $EXOMONAD_ROOT"
              echo ""
              echo "Commands:"
              echo "  cabal build all      Build Haskell packages"
              echo "  ./ide              Start Claude Code++ session"
              echo ""
              echo "Other shells:"
              echo "  nix develop .#wasm              WASM cross-compilation"
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

        # Build Outputs
        packages = let
          # Hash for dependencies ONLY. Update this if you change .cabal files.
          # Nix will tell you the new hash if it mismatches.
          deps = mkWasmDeps "sha256-RnjX41sAfU2dcynOHMCI+XulTnEB4FRJMehQZD5cgtA=";
        in {
          default = pkgs.writeShellScriptBin "exomonad-env-check" ''
            echo "ExoMonad environment check"
            echo "Run 'nix develop' to enter the development shell"
          '';
          
          # Expose deps derivation so we can build it directly to check hash
          wasm-deps = deps;
        };
      }
    );
}
