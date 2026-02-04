{
  description = "Test Role for mkWasmRole";

  inputs = {
    exomonad.url = "path:../../";
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.follows = "exomonad/flake-utils";
  };

  outputs = { self, exomonad, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: {
      packages.default = exomonad.lib.${system}.mkWasmRole {
        name = "test-role";
        src = ./.;
      };
    });
}
