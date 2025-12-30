{ pkgs ? import <nixpkgs> {} }:

let
  lib = pkgs.lib;

  # Helper functions for compatibility
  hasPackage = name: builtins.hasAttr name pkgs && !(pkgs.${name}.meta.broken or false);

  # Haskell packages - use the same package set for consistency
  hsPkgs = pkgs.haskellPackages;

  # Node.js with fallback (for Claude CLI)
  nodePackage =
    if hasPackage "nodejs_20" then pkgs.nodejs_20
    else if hasPackage "nodejs_18" then pkgs.nodejs_18
    else if hasPackage "nodejs" then pkgs.nodejs
    else throw "No Node.js package found in nixpkgs";

  # Haskell tools with availability checks
  haskellTools = with hsPkgs; [
    ghc
    cabal-install
  ] ++ lib.optional (builtins.hasAttr "haskell-language-server" hsPkgs) haskell-language-server
    ++ lib.optional (builtins.hasAttr "fourmolu" hsPkgs) fourmolu
    ++ lib.optional (builtins.hasAttr "hlint" hsPkgs) hlint;

in
pkgs.mkShell {
  buildInputs = haskellTools ++ [
    # System dependencies for threepenny-gui/zlib/etc
    pkgs.zlib
    pkgs.zlib.dev
    pkgs.pkg-config

    # Node.js ecosystem (for Claude CLI)
    nodePackage
    pkgs.nodePackages.npm

    # Useful tools
    pkgs.curl
    pkgs.jq
  ];

  shellHook = ''
    export PKG_CONFIG_PATH="${pkgs.zlib.dev}/lib/pkgconfig:$PKG_CONFIG_PATH"

    # Set up Haskell environment
    export NIX_GHC_LIBDIR="$(ghc --print-libdir)"

    # Set up local npm environment (for Claude CLI)
    mkdir -p .npm-packages
    export NPM_CONFIG_PREFIX="$PWD/.npm-packages"
    export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"

    echo "═══════════════════════════════════════════════════════════"
    echo "  Tidepool Development Environment"
    echo "═══════════════════════════════════════════════════════════"
    echo ""
    echo "GHC: $(ghc --numeric-version)"
    echo "Cabal: $(cabal --numeric-version)"
    echo "Node.js: $(node --version)"
    echo ""

    # Report available Haskell tools
    command -v hlint >/dev/null && echo "✓ hlint available" || echo "✗ hlint not available"
    command -v fourmolu >/dev/null && echo "✓ fourmolu available" || echo "✗ fourmolu not available"
    command -v haskell-language-server >/dev/null && echo "✓ HLS available" || echo "✗ HLS not available"
    echo ""

    # Install Claude CLI if not already available
    if ! command -v claude >/dev/null 2>&1; then
      echo "Installing Claude Code CLI..."
      npm install -g @anthropic-ai/claude-code 2>/dev/null || echo "Note: Claude CLI installation requires manual setup"
    fi

    command -v claude >/dev/null && echo "✓ Claude Code CLI available" || echo "✗ Claude Code CLI not available (run: npm install -g @anthropic-ai/claude-code)"

    echo ""
    echo "───────────────────────────────────────────────────────────"
    echo "Commands:"
    echo "  cabal build                    - Build all targets"
    echo "  cabal run tidepool-tidy-gui    - Run tidying agent GUI"
    echo "  cabal run tidepool-dm-gui      - Run DM agent GUI"
    echo "  claude                         - Start Claude Code"
    echo ""
    echo "Environment:"
    echo "  ANTHROPIC_API_KEY must be set for LLM calls"
    echo "───────────────────────────────────────────────────────────"
  '';
}
