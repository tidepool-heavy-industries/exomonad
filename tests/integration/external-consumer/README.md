# External Consumer Integration Test

This directory simulates an external repo consuming exomonad as infrastructure.

## Structure

```
.
├── flake.nix                    # Consumes exomonad.lib.mkWasmRole
├── .mcp.json                    # MCP server config (managed by `claude mcp add`)
├── .exo/
│   ├── config.toml              # Role config (default_role = "test")
│   ├── lib/
│   │   └── Placeholder.hs       # Shared types (empty for this test)
│   ├── wasm/                    # WASM output (created by build)
│   │   └── wasm-guest-test.wasm
│   └── roles/
│       └── test/
│           └── Role.hs          # Minimal role with ping tool
└── README.md                    # This file
```

## Running the Test

```bash
# 1. Install exomonad binary (incremental, fast)
cargo install --git https://github.com/anthropics/exomonad exomonad
# Or for local dev:
cd /path/to/tidepool/repo/rust && cargo install --path exomonad

# 2. Build WASM for the test role (requires nix for GHC WASM toolchain)
# From repo root:
nix build ./tests/integration/external-consumer#test \
  --override-input exomonad .

# 3. Install WASM and test
mkdir -p tests/integration/external-consumer/.exo/wasm
cp result/wasm-guest-test.wasm tests/integration/external-consumer/.exo/wasm/
cd tests/integration/external-consumer
echo '{"jsonrpc":"2.0","method":"tools/list","id":1}' | exomonad mcp-stdio
```

## Manual Agent Testing

To test with a real Claude agent session:

```bash
# From repo root:

# 1. Build WASM (with local override for development)
nix build ./tests/integration/external-consumer#test --override-input exomonad .

# 2. Install WASM to the test directory
mkdir -p tests/integration/external-consumer/.exo/wasm
cp result/wasm-guest-test.wasm tests/integration/external-consumer/.exo/wasm/

# 3. Launch Claude agent
cd tests/integration/external-consumer
claude

# 4. In Claude session:
#    - Check MCP tools are available (ping should appear)
#    - Try calling the ping tool to verify the pipeline works
```

MCP config (via `claude mcp add exomonad -- exomonad mcp-stdio`) tells Claude to use exomonad as an MCP server. The `ping` tool is a minimal proof that the full pipeline works: Claude → MCP → Rust sidecar → WASM → Haskell handler.

## Quick Validation

```bash
# Verify flake syntax
nix-instantiate --parse flake.nix

# Verify binary works
exomonad --version
```

## Real External Repo Usage

External repos need two things:

**1. Binary (via cargo):**
```bash
cargo install --git https://github.com/anthropics/exomonad exomonad
```

**2. WASM build (via nix flake):**
```nix
# flake.nix
inputs.exo.url = "github:anthropics/exomonad";

outputs = { self, exo, ... }: {
  packages.myRole = exo.lib.${system}.mkWasmRole {
    name = "myRole";
    src = ./.exo/roles/myRole;
    libSrc = ./.exo/lib;
  };
};
```

Then define your roles in `.exo/roles/<role>/Role.hs`.

## Role.hs Format

```haskell
module Role (config, Tools) where

import ExoMonad.Guest.Records.Ping (PingTools(..), pingToolsHandler)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Types (RoleConfig(..), defaultHooks)

type Tools = PingTools  -- Or your custom tools record

config :: RoleConfig (Tools AsHandler)
config = RoleConfig
  { roleName = "your-role"
  , tools = pingToolsHandler  -- Or your handler
  , hooks = defaultHooks
  }
```
