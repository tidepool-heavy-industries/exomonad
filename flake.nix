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
                base == "polysemy" ||
                base == "exomonad-pdk"
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

        # Hash for dependencies ONLY. Update this if you change .cabal files.
        deps = mkWasmDeps "sha256-RnjX41sAfU2dcynOHMCI+XulTnEB4FRJMehQZD5cgtA=";

        # 2. User Role Builder
        # Builds role WASM from user-defined Role.hs in .exomonad/roles/.
        # mkWasmGuest (internal role builder) was removed per issue #508.
        mkWasmRole = { name, src, libSrc }: pkgs.stdenv.mkDerivation {
          pname = "wasm-guest-${name}";
          version = "0.1.0.0";
          
          dontUnpack = true;
          
          nativeBuildInputs = [ wasmPkgs.all_9_12 pkgs.wizer ];
          
          buildPhase = ''
            ${cabalConfig}
            
            # 1. Setup Exomonad sources (libs)
            mkdir -p build
            cd build
            
            # Copy exomonad haskell sources
            # We use the same filtering as mkWasmGuest to avoid unnecessary rebuilds
            cp -r ${pkgs.lib.cleanSourceWith {
                src = ./.;
                filter = path: type:
                  let base = baseNameOf path; in
                  type == "directory" ||
                  base == "cabal.project.wasm" ||
                  pkgs.lib.hasSuffix ".cabal" base ||
                  pkgs.lib.hasSuffix ".hs" base ||
                  pkgs.lib.hasSuffix ".hs-boot" base ||
                  pkgs.lib.hasSuffix ".c" base ||
                  base == "LICENSE";
              }}/* .
            
            chmod -R u+w .

            # 2. Setup User sources
            mkdir -p roles/${name}
            cp -r ${src}/* roles/${name}/

            # Setup Lib sources (shared across roles)
            mkdir -p lib
            cp -r ${libSrc}/* lib/

            # 3. Generate Main.hs with WASM exports
            cat > roles/${name}/Main.hs <<'MAIN_EOF'
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Foreign.C.Types (CInt(..))
import ExoMonad.Guest.Tool.Runtime (hookHandler, listHandlerRecord, mcpHandlerRecord, wrapHandler)
-- AsSchema not needed when using @Tools type application
import Role (config, Tools)
import ExoMonad.Types (RoleConfig(..))

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ mcpHandlerRecord (tools config)

handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ listHandlerRecord @Tools

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler $ hookHandler (hooks config)

main :: IO ()
main = pure ()
MAIN_EOF

            # 4. Generate .cabal file for the role
            cat > roles/${name}/${name}.cabal <<EOF
cabal-version: 3.4
name: ${name}
version: 0.1.0.0
build-type: Simple

common shared
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
        base >= 4.16 && < 5,
        bytestring,
        text,
        aeson,
        polysemy >= 1.9,
        unordered-containers,
        time,
        directory,
        filepath

    if !arch(wasm32)
        build-depends: polysemy-plugin >= 0.4

executable wasm-guest-${name}
    import: shared
    hs-source-dirs: . ../../lib
    main-is: Main.hs
    other-modules: Role
    ghc-options: -no-hs-main -optl-mexec-model=reactor -optl-Wl,--export=hs_init -optl-Wl,--export=handle_mcp_call -optl-Wl,--export=handle_pre_tool_use -optl-Wl,--export=handle_list_tools -optl-Wl,--allow-undefined
    build-depends:
        base,
        wasm-guest:wasm-guest-internal,
        exomonad-pdk
EOF

            # 5. Generate cabal.project
            # We need to append the new package to the project
            cp cabal.project.wasm cabal.project.offline
            
            sed -i '/repository head.hackage/,/secure: True/d' cabal.project.offline
            echo "" >> cabal.project.offline
            echo "packages: roles/${name}/${name}.cabal" >> cabal.project.offline
            
            echo "Restoring dependencies from store..."
            rm -rf $HOME/.ghc-wasm
            cp -r ${deps}/ghc-wasm $HOME/.ghc-wasm
            chmod -R u+w $HOME/.ghc-wasm
            
            echo "Building ${name}..."
            wasm32-wasi-cabal build --project-file=cabal.project.offline wasm-guest-${name}
          '';
          
          installPhase = ''
            mkdir -p $out
            wasm_file=$(find dist-newstyle -name "wasm-guest-${name}.wasm" -print -quit)
            if [ -z "$wasm_file" ]; then
              echo "Error: wasm-guest-${name}.wasm not found in dist-newstyle" >&2
              exit 1
            fi
            cp "$wasm_file" "$out/wasm-guest-${name}.wasm"
          '';
        };

      in {
        lib = {
          inherit mkWasmRole;
        };
        devShells = {
          # Default: Full development environment
          default = pkgs.mkShell {
            packages = commonPkgs
              ++ haskellPkgs "ghc912"
              ++ rustPkgs
              ++ orchestrationPkgs
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
              echo "  cabal build all      Build Haskell packages"
              echo "  just proto-gen       Generate proto code (Rust + Haskell)"
              echo ""
              echo "Other shells:"
              echo "  nix develop .#wasm   WASM cross-compilation"
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

        };

        # Build Outputs
        packages = {
          default = pkgs.writeShellScriptBin "exomonad-env-check" ''
            echo "ExoMonad environment check"
            echo "Run 'nix develop' to enter the development shell"
          '';

          # Expose deps derivation so we can build it directly to check hash
          wasm-deps = deps;

          # Role WASM binaries built from .exomonad/roles/ using mkWasmRole
          tl = mkWasmRole {
            name = "tl";
            src = ./.exomonad/roles/tl;
            libSrc = ./.exomonad/lib;
          };
          dev = mkWasmRole {
            name = "dev";
            src = ./.exomonad/roles/dev;
            libSrc = ./.exomonad/lib;
          };
        };
      }
    );
}
