# Legacy nix-shell support
# For flakes, use: nix develop
#
# This shell provides the default development environment.
# For WASM or worker-specific shells, use the flake:
#   nix develop .#wasm
#   nix develop .#worker

let
  # Fetch rust-overlay for latest stable Rust (edition 2024 support)
  rustOverlay = import (builtins.fetchTarball {
    url = "https://github.com/oxalica/rust-overlay/archive/master.tar.gz";
  });

  pkgs = import <nixpkgs> {
    overlays = [ rustOverlay ];
  };

  # Common packages
  commonPkgs = with pkgs; [
    jq
    just
    curl
    git
  ];

  # Haskell toolchain
  haskellPkgs = with pkgs; [
    haskell.compiler.ghc910
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

  # Node.js toolchain
  nodePkgs = with pkgs; [
    nodejs_20
    pnpm
  ];

  # Orchestration
  orchestrationPkgs = with pkgs; [
    process-compose
    zellij
  ];

in pkgs.mkShell {
  buildInputs = commonPkgs
    ++ haskellPkgs
    ++ rustPkgs
    ++ nodePkgs
    ++ orchestrationPkgs
    ++ [ pkgs.sqlite ];

  shellHook = ''
    export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"
    export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
    export HANGAR_ROOT="$PWD"
    export TIDEPOOL_REPO="$PWD/repo"

    echo "╔═══════════════════════════════════════════════════════════╗"
    echo "║           Tidepool Hangar Development Shell               ║"
    echo "╚═══════════════════════════════════════════════════════════╝"
    echo ""
    echo "  GHC:      $(ghc --numeric-version)"
    echo "  Cabal:    $(cabal --numeric-version)"
    echo "  Cargo:    $(cargo --version | cut -d' ' -f2)"
    echo "  Node.js:  $(node --version)"
    echo "  pnpm:     $(pnpm --version)"
    echo ""
    echo "Paths:"
    echo "  HANGAR_ROOT:   $HANGAR_ROOT"
    echo "  TIDEPOOL_REPO: $TIDEPOOL_REPO"
    echo ""
    echo "Commands:"
    echo "  cd repo && cabal build all      Build Haskell packages"
    echo "  cd repo && just native          Run native server"
    echo "  cd repo && ./start-augmented.sh Start Claude Code++ session"
    echo ""
    echo "NOTE: For flakes with more shells, use: nix develop"
    echo ""
  '';
}
