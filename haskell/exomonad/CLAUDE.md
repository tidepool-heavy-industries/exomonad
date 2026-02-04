# ExoMonad Haskell Library

The core framework library for building ExoMonad agents and roles in Haskell.

## Architecture

This library provides the building blocks for creating "IO-blind" agents that communicate with the ExoMonad host (Rust) via FFI/WASM.

### Core Modules

- **`ExoMonad`**: Main entry point re-exporting all primary functionality.
- **`ExoMonad.Types`**: Core types like `RoleConfig` and `HookConfig`.
- **`ExoMonad.Tools.*`**: Pre-built nested tool records (`GitTools`, `AgentTools`, etc.).
- **`ExoMonad.Effects.*`**: Polysemy effects for capabilities (`AgentControl`, `FileSystem`).
- **`ExoMonad.Tool.*`**: Low-level infrastructure for defining tools and handling MCP calls.
- **`ExoMonad.HostCall`**: FFI bindings to the Rust host.

## Usage

Role authors should import `ExoMonad` and define their role configuration:

```haskell
module MyRole where

import ExoMonad
import GHC.Generics (Generic)

-- 1. Define Tool Set (Nested)
data MyTools mode = MyTools
  { git :: GitTools mode
  , fs :: FileTools mode
  } deriving Generic

-- 2. Define Configuration
config :: RoleConfig MyTools
config = RoleConfig
  { roleName = "my-role"
  , tools = MyTools
      { git = gitTools
      , fs = fileTools
      }
  , hooks = defaultHooks
  }
```

## Developing

- **Build**: `cabal build exomonad`
- **Dependencies**: `extism-pdk` (for WASM), `polysemy` (for effects).
