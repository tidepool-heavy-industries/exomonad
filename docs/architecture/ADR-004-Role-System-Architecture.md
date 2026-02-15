# ADR 004: Role System Architecture

## Status

Accepted

## Context

We need a flexible way to define agent roles (Dev, TL, PM) that allows users to customize:
1.  **Tools**: Which tools are available to the agent.
2.  **Hooks**: Lifecycle hooks (e.g., pre-tool-use, on-stop).
3.  **Behavior**: Potentially custom graph logic (future).

The previous approach of hardcoded roles or purely configuration-based roles lacks the expressiveness of a code-as-configuration approach (like xmonad).

## Decision

We will implement an **xmonad-inspired role system** where users define roles in Haskell, which are then compiled to WASM modules loaded by the ExoMonad runtime.

### 1. Role Configuration Type

Users define a `RoleConfig` record:

```haskell
data RoleConfig tools = RoleConfig
  { roleName :: Text
  , tools :: tools
  , hooks :: HookConfig
  }
```

### 2. Tool Record Pattern

Tools are composed using a Servant-style record pattern `mode :- Tool`. This allows the same record to be used for:
*   **Schema Generation** (`AsSchema` mode): Generating MCP tool definitions.
*   **Handler Dispatch** (`AsHandler` mode): Executing tool logic.
*   **User Selection**: Users pick tools in their role definition.

```haskell
data GitTools mode = GitTools
  { branch :: mode :- GitBranch
  , status :: mode :- GitStatus
  }
```

### 3. User Experience

Users create role definitions in `.exo/roles/<role>/Role.hs`:

```haskell
module Role (config) where
import ExoMonad

-- User defines their tool composition
data Tools mode = Tools
  { git :: GitTools mode
  , github :: GitHubTools mode
  } deriving Generic

config :: RoleConfig (Tools AsHandler)
config = RoleConfig
  { roleName = "dev"
  , tools = Tools
      { git = gitTools      -- Pre-built handler record
      , github = githubTools
      }
  , hooks = defaultHooks
  }
```

### 4. Shared Code

A shared library directory `.exo/lib/` allows code reuse across roles.
*   Roles can import modules from `lib/` (e.g., `import Common`).
*   This is useful for shared hooks or custom tool logic.

### 5. Directory Structure

```
.exo/
├── flake.nix              # Git-tracked: builds roles
├── lib/                   # Git-tracked: shared code
├── roles/                 # Git-tracked: role definitions
├── config.toml            # Git-tracked: project defaults
├── config.local.toml      # Gitignored: local overrides
├── wasm/                  # Gitignored: build outputs (in main repo only)
└── .gitignore
```

### 6. Secrets Management

Secrets are managed entirely by the Rust host (ExoMonad binary), not the Haskell WASM guest.
*   Secrets file: `~/.exo/secrets/` (global to user).
*   Haskell code does not handle secrets directly; they are injected by the host into tool execution where necessary (transparently).

### 7. Runtime Loading

The ExoMonad binary (`exomonad`) will:
1.  Read `config.local.toml` to determine the active `role` and `wasm_path`.
    *   `wasm_path` must be absolute (pointing to the main repo's `wasm/` dir).
2.  Load the corresponding WASM module (e.g., `<wasm_path>/wasm-guest-<role>.wasm`).
3.  Initialize the role via FFI.

## Consequences

### Positive
*   **Type Safety**: Role definitions are type-checked.
*   **Flexibility**: Users can write arbitrary logic in hooks and tool composition.
*   **Parallelism**: Implementation of specific tools and the runtime loader can proceed in parallel.
*   **Isolation**: WASM provides a sandbox for user code.

### Negative
*   **Complexity**: Requires Haskell toolchain (or Nix) to build roles.
*   **Compilation Time**: Changing a role requires recompilation via `exomonad --recompile`.

## Alternatives Considered

*   **JSON/YAML Config**: Too limited for complex logic (hooks).
*   **Lua/Python Scripting**: Good flexibility, but lacks the type safety and integration with the Haskell-based DSL of ExoMonad.