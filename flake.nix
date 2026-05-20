{
  description = "ExoMonad - Development environment for ExoMonad LLM agent framework";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta, rust-overlay, crane }:
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

        # Crane for incremental Rust builds
        craneLib = crane.mkLib pkgs;

        # Rust toolchain (latest stable for edition 2024 support)
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rust-analyzer" ];
          targets = [ "wasm32-wasip1" ];
        };
        rustPkgs = [ rustToolchain ];

        # Orchestration tools
        orchestrationPkgs = with pkgs; [
          tmux
          jujutsu  # jj
        ];

        # Source with proto files included
        # Note: crane's buildDepsOnly applies cleanCargoSource which removes non-Cargo files.
        # We work around this by including proto files as a separate derivation.
        exomonadSource = builtins.path {
          path = ./.;
          name = "exomonad-source";
          filter = path: type:
            let
              baseName = builtins.baseNameOf path;
              relPath = pkgs.lib.removePrefix (toString ./.) (toString path);
            in
            baseName == "Cargo.toml" || baseName == "Cargo.lock"
            || pkgs.lib.hasPrefix "/rust" relPath
            || pkgs.lib.hasPrefix "/proto" relPath
            || pkgs.lib.hasPrefix "/vendor/acp-rust-sdk" relPath
            || type == "directory";
        };

        # Proto files as a separate derivation (to copy into build)
        protoFiles = builtins.path {
          path = ./proto;
          name = "exomonad-proto";
        };

        # Common crane configuration
        commonCraneArgs = {
          src = exomonadSource;
          cargoLock = ./Cargo.lock;

          nativeBuildInputs = with pkgs; [
            protobuf
            pkg-config
          ];

          buildInputs = with pkgs; [
            openssl
          ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
            Security
            SystemConfiguration
          ]);

          preBuild = ''
            # Copy proto files from separate derivation (crane removes them from src)
            echo "Copying proto files from ${protoFiles}..."
            mkdir -p rust/exomonad-proto/proto
            cp -r ${protoFiles}/exomonad rust/exomonad-proto/proto/exomonad
            cp -r ${protoFiles}/effects rust/exomonad-proto/proto/effects
          '';
        };

        # Exomonad Rust binary (built with crane)
        # Using buildPackage directly provides incremental compilation
        exomonad = craneLib.buildPackage (commonCraneArgs // {
          pname = "exomonad";
          cargoBuildFlags = [ "-p" "exomonad" ];
          cargoTestFlags = [ "-p" "exomonad" ];

          meta = with pkgs.lib; {
            description = "Type-safe LLM agent orchestration";
            homepage = "https://github.com/tidepool-heavy-industries/exomonad";
            license = licenses.bsd3;
            mainProgram = "exomonad";
          };
        });

        # NotebookLM MCP server (vendored, optional)
        notebooklm-mcp = pkgs.buildNpmPackage {
          pname = "notebooklm-mcp";
          version = "1.2.1";
          src = ./vendor/notebooklm-mcp;
          npmDepsHash = "sha256-z55OeM4lrGhjsJutmXHfD82Lt5NIoeUoAfC+0FMwZYQ=";

          nativeBuildInputs = [ pkgs.makeWrapper ];

          # Patchright tries to download browsers at install time — skip it
          npmFlags = [ "--ignore-scripts" ];
          dontNpmBuild = false;

          postInstall = ''
            wrapProgram $out/bin/notebooklm-mcp \
              --set PLAYWRIGHT_BROWSERS_PATH ${pkgs.playwright-driver.browsers} \
              --set PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS true
          '';

          meta = {
            description = "MCP server for NotebookLM via browser automation (patchright)";
            license = pkgs.lib.licenses.mit;
          };
        };

      in {
        devShells = {
          # Default: Full development environment
          default = pkgs.mkShell {
            packages = commonPkgs
              ++ haskellPkgs "ghc912"
              ++ rustPkgs
              ++ orchestrationPkgs
              ++ [ pkgs.sqlite pkgs.duckdb ]
              ++ [ proto3SuitePkg ]  # For compile-proto-file (Haskell proto codegen)
              ++ [ notebooklm-mcp ]; # NotebookLM MCP server (opt-in via config.toml)

            shellHook = ''
              # nix mkShell overrides TMPDIR to /tmp/nix-shell.*, which breaks
              # tmux and other tools socket discovery.
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
          default = exomonad;
          inherit exomonad;
          inherit notebooklm-mcp;
        };
      }
    );
}
