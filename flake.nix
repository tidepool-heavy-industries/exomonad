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

        # WASM build infrastructure
        makeWasmGuest = role: hash: pkgs.stdenv.mkDerivation {
          pname = "wasm-guest-${role}";
          version = "0.1.0.0";
          src = pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let base = baseNameOf path; in
              type == "directory" ||
              base == "cabal.project.wasm" ||
              base == "cabal.project.wasm.freeze" ||
              pkgs.lib.hasSuffix ".cabal" base ||
              pkgs.lib.hasSuffix ".hs" base ||
              pkgs.lib.hasSuffix ".hs-boot" base ||
              base == "LICENSE" ||
              base == "Setup.hs";
          };
          nativeBuildInputs = [ wasmPkgs.all_9_12 pkgs.wizer pkgs.cacert pkgs.curl ];

          outputHashAlgo = "sha256";
          outputHashMode = "recursive";
          outputHash = hash;

          buildPhase = ''
            export HOME=$TMPDIR
            export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt

            mkdir -p $HOME/.ghc-wasm/.cabal
            cat > $HOME/.ghc-wasm/.cabal/config <<EOF
repository hackage.haskell.org
  url: http://hackage.haskell.org/
  secure: True

library-profiling: False
executable-profiling: False
shared: True
executable-dynamic: False
EOF

            # Rename cabal.project temporarily to avoid interference
            if [ -f cabal.project ]; then mv cabal.project cabal.project.native; fi
            if [ -f cabal.project.freeze ]; then mv cabal.project.freeze cabal.project.freeze.native; fi

            # Remove head.hackage from the project file to avoid network errors
            cp cabal.project.wasm cabal.project.offline
            sed -i '/repository head.hackage/,/secure: True/d' cabal.project.offline

            wasm32-wasi-cabal update
            wasm32-wasi-cabal build -v --project-file=cabal.project.offline wasm-guest-${role} --only-dependencies --enable-shared --enable-static --disable-tests --disable-benchmarks
            wasm32-wasi-cabal build -v --project-file=cabal.project.offline wasm-guest-${role} --enable-shared --enable-static --disable-tests --disable-benchmarks

            # Restore cabal.project
            if [ -f cabal.project.native ]; then mv cabal.project.native cabal.project; fi
            if [ -f cabal.project.freeze.native ]; then mv cabal.project.freeze.native cabal.project.freeze; fi
          '';

          installPhase = ''
            mkdir -p $out
            find dist-newstyle -name "wasm-guest-${role}.wasm" -exec cp {} $out/wasm-guest-${role}.wasm \;
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

        # Allow `nix build` to produce a simple check
        packages = {
          default = pkgs.writeShellScriptBin "exomonad-env-check" ''
            echo "ExoMonad environment check"
            echo "Run 'nix develop' to enter the development shell"
          '';
          wasm-guest-tl = makeWasmGuest "tl" "sha256-A46JDaQk96P66u/EWNQTe/u/T8UH8Y9jtsRce0bkYmI=";
          wasm-guest-dev = makeWasmGuest "dev" "sha256-I2wvYxVz4WCbiDU1QbEl7rUBAeLYoK5m6/YZDSpP+8w=";
        };
      }
    );
}