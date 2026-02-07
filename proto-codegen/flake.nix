{
  description = "Proto codegen tool (isolated from main GHC stack)";

  inputs = {
    # Pin to nixpkgs where ghc96.proto3-suite is cached
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          # Use GHC 9.6 proto3-suite - known to be cached and working
          # Build without swagger to avoid ToSchema instances
          proto3-suite-no-swagger = pkgs.haskell.lib.compose.disableCabalFlag "swagger"
            pkgs.haskell.packages.ghc96.proto3-suite;
        in {
          default = pkgs.haskell.lib.justStaticExecutables proto3-suite-no-swagger;
          compile-proto-file = self.packages.${system}.default;
        });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/compile-proto-file";
        };
      });
    };
}
