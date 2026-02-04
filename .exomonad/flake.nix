{
  description = "ExoMonad Role Definitions";

  inputs = {
    exomonad.url = "path:..";
  };

  outputs = { self, exomonad }:
    let
      systems = [ "aarch64-darwin" "x86_64-linux" "x86_64-darwin" "aarch64-linux" ];
      forAllSystems = f: builtins.listToAttrs (map (system: { name = system; value = f system; }) systems);
    in {
      packages = forAllSystems (system:
        let
          mkWasmRole = exomonad.lib.${system}.mkWasmRole;
        in {
          # TL role: supervisor with agent tools, no stop checks
          tl = mkWasmRole {
            name = "tl";
            src = ./roles/tl;
            libSrc = ./lib;
          };

          # Dev role: worker with stop hook validation
          dev = mkWasmRole {
            name = "dev";
            src = ./roles/dev;
            libSrc = ./lib;
          };
        }
      );
    };
}
