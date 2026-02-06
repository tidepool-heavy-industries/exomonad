{
  description = "Test external consumer of exomonad";

  # NOTE: For local testing, run from the repo root:
  #   nix build ./tests/integration/external-consumer#test --override-input exomonad .
  #
  # For real external repos, use:
  #   exomonad.url = "github:anthropics/exomonad";
  inputs = {
    exomonad.url = "github:anthropics/exomonad";  # Override for local testing
    nixpkgs.follows = "exomonad/nixpkgs";
    flake-utils.follows = "exomonad/flake-utils";
  };

  outputs = { self, exomonad, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        mkWasmRole = exomonad.lib.${system}.mkWasmRole;
      in {
        packages = {
          # Build WASM for the "test" role
          test = mkWasmRole {
            name = "test";
            src = ./.exomonad/roles/test;
            libSrc = ./.exomonad/lib;
          };

          default = self.packages.${system}.test;
        };

        # For the exomonad binary, use cargo install:
        #   cargo install --git https://github.com/anthropics/exomonad exomonad
      }
    );
}
