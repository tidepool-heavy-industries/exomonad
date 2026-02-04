{
  description = "ExoMonad roles for this project";

  inputs.exomonad.url = "path:..";

  outputs = { self, exomonad }: let
    systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
    forAllSystems = f: builtins.listToAttrs 
      (map (s: { name = s; value = f s; }) systems);
  in {
    packages = forAllSystems (system: {
      dev = exomonad.lib.${system}.mkWasmRole {
        name = "dev";
        src = ./roles/dev;
      };
      tl = exomonad.lib.${system}.mkWasmRole {
        name = "tl";
        src = ./roles/tl;
      };
    });
  };
}