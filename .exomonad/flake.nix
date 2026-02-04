{
  description = "ExoMonad Role Definitions";

  inputs = {
    exomonad.url = "path:..";
    nixpkgs.follows = "exomonad/nixpkgs";
    flake-utils.follows = "exomonad/flake-utils";
    ghc-wasm-meta.follows = "exomonad/ghc-wasm-meta";
  };

  outputs = { self, exomonad, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Access deps from main flake
        deps = exomonad.packages.${system}.wasm-deps;
        
        mkWasmRole = roleName: rolePath: pkgs.stdenv.mkDerivation {
          pname = "wasm-guest-${roleName}";
          version = "0.1.0.0";
          
          # Use the root of the repo as source
          src = ../.;
          
          nativeBuildInputs = [ 
             ghc-wasm-meta.packages.${system}.all_9_12
             pkgs.wizer
          ];

          buildPhase = ''
             # Setup environment
             export HOME=$TMPDIR
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

            echo "Restoring dependencies..."
            rm -rf $HOME/.ghc-wasm
            cp -r ${deps}/ghc-wasm $HOME/.ghc-wasm
            chmod -R u+w $HOME/.ghc-wasm
            
            # Copy role file
            echo "Copying role from ${rolePath}..."
            cp ${rolePath} haskell/wasm-guest/src/Role.hs
            
            # Generate Main.hs
            echo "Generating Main.hs..."
            cat > haskell/wasm-guest/src/RoleMain.hs <<EOF
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import ExoMonad.Guest.Tool.Runtime (hookHandler, listHandlerRecord, mcpHandlerRecord, wrapHandler, testHandler)
import Foreign.C.Types (CInt (..))
import Role (config, Tools) 
import ExoMonad (RoleConfig(..))

-- WASM exports
foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall handle_test_call :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler \$ mcpHandlerRecord (tools config)

handle_list_tools :: IO CInt
handle_list_tools = wrapHandler \$ listHandlerRecord @Tools

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler hookHandler

handle_test_call :: IO CInt
handle_test_call = wrapHandler testHandler

main :: IO ()
main = pure ()
EOF

            # Patch .cabal file to add new executable
            # We use a unique name 'wasm-role-${roleName}' to avoid conflict with existing targets
            echo "Patching cabal file..."
            cat >> haskell/wasm-guest/wasm-guest.cabal <<EOF

executable wasm-role-${roleName}
    import:           shared
    hs-source-dirs:   src
    main-is:          RoleMain.hs
    other-modules:    Role
    build-depends:
        wasm-guest:wasm-guest-internal,
        extism-pdk ^>= 1.2.0.0
    
    if arch(wasm32)
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

            # Prepare project file
            cp cabal.project.wasm cabal.project.offline
            sed -i '/repository head.hackage/,/secure: True/d' cabal.project.offline
            
            echo "Building..."
            wasm32-wasi-cabal build --project-file=cabal.project.offline wasm-role-${roleName}
          '';
          
          installPhase = ''
            mkdir -p $out
            # Find the binary (it might be named wasm-role-${roleName}.wasm)
            wasm_file=$(find dist-newstyle -name "wasm-role-${roleName}.wasm" -print -quit)
            
            if [ -z "$wasm_file" ]; then
              echo "Error: WASM file not found!" >&2
              find dist-newstyle -name "*.wasm"
              exit 1
            fi
            
            # Install as wasm-guest-${roleName}.wasm as expected by sidecar
            cp "$wasm_file" "$out/wasm-guest-${roleName}.wasm"
          '';
        };

      in {
        packages = {
          dev = mkWasmRole "dev" ./roles/dev/Role.hs;
          tl = mkWasmRole "tl" ./roles/tl/Role.hs;
        };
      }
    );
}