# ExoMonad DSL Reference (LLM Ingestion)

Type-safe MCP tool authoring in Haskell WASM. This document covers how to define your own tools, hooks, event handlers, and roles using the exomonad guest SDK — independent of the devswarm orchestration system.

---

## Architecture in 30 Seconds

```
Your Haskell code (pure logic, no I/O)
    ↓ compiled to WASM32-WASI
    ↓ loaded by exomonad server at runtime
    ↓ yields typed effects (git, github, filesystem, etc.)
    ↓ Rust host executes all I/O
    ↓ results returned to your Haskell handlers
```

All tool schemas, argument parsing, dispatch logic, hooks, and event handlers live in Haskell. Rust is the I/O runtime — it never defines tools or parses tool arguments.

---

## Quick Start: Define a Tool

```haskell
-- 1. Define argument type
data MyToolArgs = MyToolArgs
  { mtaFilePath :: Text
  , mtaContent  :: Text
  , mtaOverwrite :: Maybe Bool
  } deriving (Generic, FromJSON)

-- 2. Create phantom type and implement MCPTool
data MyTool
instance MCPTool MyTool where
  type ToolArgs MyTool = MyToolArgs
  toolName = "my_tool"
  toolDescription = "Write content to a file"
  toolSchema = genericToolSchemaWith @MyToolArgs
    [ ("file_path", "Absolute path to target file")
    , ("content", "Content to write")
    , ("overwrite", "Overwrite if exists (default: false)")
    ]
  toolHandlerEff args = do
    result <- suspendEffect @FsWriteFile $ WriteFileRequest
      { wfPath = mtaFilePath args
      , wfContent = mtaContent args
      }
    case result of
      Right _ -> pure $ successResult (object ["written" .= True])
      Left err -> pure $ errorResult (effectErrorMessage err)

-- 3. Wire into a role's tool record
data Tools mode = Tools
  { myTool :: mode :- MyTool
  } deriving Generic
```

That's it. The SDK handles JSON Schema generation, argument parsing, dispatch routing, and WASM FFI.

---

## Core Concepts

### MCPTool Typeclass

Every tool is a phantom type with an `MCPTool` instance:

```haskell
class MCPTool (t :: Type) where
  type ToolArgs t :: Type              -- Argument record (needs FromJSON)
  toolName        :: Text              -- snake_case MCP tool name
  toolDescription :: Text              -- Human-readable description
  toolSchema      :: Aeson.Object      -- JSON Schema for input
  toolHandlerEff  :: ToolArgs t -> Eff Effects MCPCallOutput
```

**Module:** `ExoMonad.Guest.Tool.Class`

### Effects

Handlers run in `Eff Effects`, which supports suspension/resumption via freer-simple coroutines. When a handler needs I/O, it suspends with a typed effect request, the Rust host executes it, and the handler resumes with the result.

```haskell
type Effects = '[AgentControl, FileSystem, SuspendYield, IO]
```

**Suspending for I/O:**

```haskell
suspendEffect :: req -> Eff Effects (Either EffectError resp)
```

Each effect type maps to a Rust handler namespace:

| Namespace | Effect Type | Operations |
|-----------|-------------|------------|
| `git.*` | `ExoMonad.Effects.Git` | get_branch, get_status, get_recent_commits, get_worktree, has_unpushed_commits |
| `github.*` | `ExoMonad.Effects.GitHub` | list_issues, get_issue, create_pr, list_prs, get_pr_for_branch |
| `log.*` | `ExoMonad.Effects.Log` | info, error, emit_event |
| `agent.*` | `ExoMonad.Effects.Agent` | spawn_subtree, spawn_leaf_subtree, spawn_workers |
| `fs.*` | `ExoMonad.Effects.FileSystem` | read_file, write_file |
| `file_pr.*` | `ExoMonad.Effects.FilePR` | file_pr |
| `merge_pr.*` | `ExoMonad.Effects.MergePR` | merge_pr |
| `events.*` | `ExoMonad.Effects.Events` | notify_parent, send_message |
| `session.*` | `ExoMonad.Effects.Session` | register_claude_id, register_team |
| `tasks.*` | `ExoMonad.Effects.Tasks` | list_tasks, get_task, update_task |
| `kv.*` | `ExoMonad.Effects.KV` | get, set |
| `coordination.*` | `ExoMonad.Effects.Coordination` | acquire_mutex, release_mutex |
| `process.*` | `ExoMonad.Effects.Process` | run (execute arbitrary command) |

### Tool Results

```haskell
data MCPCallOutput = MCPCallOutput
  { success :: Bool
  , result  :: Maybe Value
  , mcpError :: Maybe Text
  }

successResult :: Value -> MCPCallOutput
errorResult   :: Text  -> MCPCallOutput
```

---

## JSON Schema Generation

Derive schemas automatically from Generic records:

```haskell
-- Basic: field names auto-converted from camelCase to snake_case
toolSchema = genericToolSchema @MyToolArgs

-- With descriptions per field
toolSchema = genericToolSchemaWith @MyToolArgs
  [ ("file_path", "Absolute path to target file")
  , ("content", "Content to write")
  ]
```

**Type mappings:**

| Haskell Type | JSON Schema |
|---|---|
| `Text`, `String` | `"type": "string"` |
| `Int`, `Integer` | `"type": "integer"` |
| `Bool` | `"type": "boolean"` |
| `Double` | `"type": "number"` |
| `[a]` | `"type": "array", "items": {...}` |
| `Maybe a` | Field omitted from `required` |
| Record | Object with properties per field |

**Field name convention:** Common prefix stripped, then camelCase → snake_case. `mtaFilePath` → `file_path`.

**Module:** `ExoMonad.Guest.Tool.Schema`

---

## Mode System: Schema + Dispatch from One Record

A single record type serves both tool listing (schema mode) and call dispatch (handler mode):

```haskell
data Tools mode = Tools
  { gitBranch :: mode :- GitBranch
  , gitStatus :: mode :- GitStatus
  , myCustom  :: mode :- MyCustomTool
  } deriving Generic
```

In schema mode (`AsSchema`), each field produces a `ToolDefinition`. In handler mode (`AsHandler`), each field produces a handler function. Dispatch routes `"git_branch"` → `gitBranch` field automatically via Generic.

**Typeclasses (auto-derived via Generic):**

```haskell
class DispatchRecord (tools :: Type -> Type) where
  dispatchRecord :: tools AsHandler -> Text -> Value -> IO (WasmResult MCPCallOutput)

class ReifyRecord (tools :: Type -> Type) where
  reifyToolDefs :: Proxy tools -> [ToolDefinition]
```

**Module:** `ExoMonad.Guest.Tool.Mode`, `ExoMonad.Guest.Tool.Record`

---

## Hooks

Lifecycle hooks intercept Claude Code events:

```haskell
data HookConfig = HookConfig
  { preToolUse    :: HookInput -> Eff Effects HookOutput
  , postToolUse   :: HookInput -> Eff Effects HookOutput
  , onStop        :: HookInput -> Eff Effects StopHookOutput
  , onSubagentStop :: HookInput -> Eff Effects StopHookOutput
  , onSessionStart :: HookInput -> Eff Effects HookOutput
  }
```

### HookInput

```haskell
data HookInput = HookInput
  { hiSessionId         :: Text
  , hiHookEventName     :: HookEventType   -- SessionStart | PreToolUse | PostToolUse | Stop | SubagentStop
  , hiToolName          :: Maybe Text       -- PreToolUse/PostToolUse only
  , hiToolInput         :: Maybe Value      -- PreToolUse only
  , hiToolResponse      :: Maybe Value      -- PostToolUse only
  , hiAgentId           :: Maybe Text
  , hiExomonadSessionId :: Maybe Text       -- Parent session ID
  , hiRuntime           :: Maybe Runtime    -- Claude or Gemini
  , hiCwd               :: Maybe Text
  , hiTranscriptPath    :: Maybe Text
  }
```

### HookOutput

```haskell
-- Allow the tool call to proceed
allowResponse :: Maybe Text -> HookOutput

-- Block the tool call
denyResponse :: Text -> HookOutput

-- Post-tool-use acknowledgment
postToolUseResponse :: Maybe Text -> HookOutput

-- Stop hook: allow agent to exit
allowStopResponse :: StopHookOutput

-- Stop hook: block agent from exiting
blockStopResponse :: Text -> StopHookOutput
```

### SessionStart: Injecting Context

Use `additionalContext` in `hookSpecificOutput` to inject text into the model's conversation:

```haskell
onSessionStart input = do
  let ctx = "You are working on project X. Follow these rules: ..."
  pure $ HookOutput
    { continue_ = True
    , stopReason = Nothing
    , suppressOutput = Nothing
    , systemMessage = Nothing  -- TUI only, never reaches the model
    , hookSpecificOutput = Just $ HookSpecificOutput
        { additionalContext = Just ctx  -- This reaches the model
        }
    }
```

---

## Event Handlers

React to world events (GitHub PR reviews, CI status, timeouts, sibling merges):

```haskell
data EventHandlerConfig = EventHandlerConfig
  { onPRReview      :: PRReviewEvent -> Eff Effects EventAction
  , onCIStatus      :: CIStatusEvent -> Eff Effects EventAction
  , onTimeout       :: TimeoutEvent -> Eff Effects EventAction
  , onSiblingMerged :: SiblingMergedEvent -> Eff Effects EventAction
  }

defaultEventHandlers :: EventHandlerConfig  -- All return NoAction
```

### Event Types

```haskell
data PRReviewEvent
  = ReviewReceived  { prNumber :: Int, comments :: Text }
  | ReviewApproved  { prNumber :: Int }
  | ReviewTimeout   { prNumber :: Int, minutesElapsed :: Int }
  | FixesPushed     { prNumber :: Int, fpCiStatus :: Text }
  | CommitsPushed   { prNumber :: Int, cpCiStatus :: Text }

data CIStatusEvent = CIStatusEvent
  { ciPrNumber :: Int, ciStatus :: Text, ciBranch :: Text }

data SiblingMergedEvent = SiblingMergedEvent
  { mergedBranch :: Text, parentBranch :: Text, siblingPRNumber :: Int }
```

### Event Actions

```haskell
data EventAction
  = InjectMessage Text                                    -- Send text into agent's pane
  | NotifyParentAction { naMessage :: Text, naPrNumber :: Int }  -- Notify parent agent
  | NoAction
```

**Module:** `ExoMonad.Guest.Events`

---

## State Machines

Track agent lifecycle phases with typed state machines. Phases persist via KV storage, and the stop hook can block exit during critical phases.

```haskell
class (ToJSON phase, FromJSON phase, Show phase)
      => StateMachine phase event where
  transition  :: phase -> event -> TransitionResult phase
  canExit     :: phase -> StopCheckResult
  machineName :: Text  -- Scopes KV key as "phase-{name}"

data TransitionResult phase
  = Transitioned phase
  | InvalidTransition Text

data StopCheckResult
  = MustBlock Text     -- Agent cannot exit
  | ShouldNudge Text   -- Warn but allow
  | Clean              -- Exit freely
```

### Framework Functions

```haskell
getPhase   :: StateMachine phase event => Eff Effects (Maybe phase)
setPhase   :: StateMachine phase event => phase -> Eff Effects ()
applyEvent :: (StateMachine phase event, Show event)
           => phase -> event -> Eff Effects (Maybe phase)
checkExit  :: StateMachine phase event => phase -> Eff Effects StopCheckResult
```

### Example

```haskell
data BuildPhase = Idle | Building | Testing | Failed Text
  deriving (Generic, ToJSON, FromJSON, Show)

data BuildEvent = StartBuild | TestsPassed | TestsFailed Text

instance StateMachine BuildPhase BuildEvent where
  machineName = "build"

  transition Idle StartBuild = Transitioned Building
  transition Building TestsPassed = Transitioned Testing
  transition Building (TestsFailed e) = Transitioned (Failed e)
  transition _ _ = InvalidTransition "unexpected event"

  canExit (Building) = MustBlock "Build in progress"
  canExit (Failed _) = ShouldNudge "Build failed — consider fixing before exit"
  canExit _ = Clean
```

**Usage in a tool handler:**

```haskell
toolHandlerEff args = do
  void $ applyEvent @BuildPhase @BuildEvent Idle StartBuild
  result <- runBuild args
  case result of
    Right _ -> do
      void $ applyEvent @BuildPhase @BuildEvent Idle TestsPassed
      pure $ successResult (object ["status" .= ("ok" :: Text)])
    Left err -> do
      void $ applyEvent @BuildPhase @BuildEvent Idle (TestsFailed err)
      pure $ errorResult err
```

**Module:** `ExoMonad.Guest.StateMachine`

---

## Role Configuration

Roles compose tools, hooks, and event handlers into a named agent configuration:

```haskell
data RoleConfig tools = RoleConfig
  { roleName      :: Text
  , tools         :: tools               -- Tools record in AsHandler mode
  , hooks         :: HookConfig
  , eventHandlers :: EventHandlerConfig
  }
```

### Registering Roles

```haskell
-- AllRoles.hs — the role registry

allConfigs :: Map Text SomeRoleConfig
allConfigs = Map.fromList
  [ ("researcher", mkSomeRoleConfig ResearcherRole.config)
  , ("writer",     mkSomeRoleConfig WriterRole.config)
  ]

-- mkSomeRoleConfig wraps a typed RoleConfig into an existential
mkSomeRoleConfig :: (DispatchRecord tools, ReifyRecord tools)
                 => RoleConfig (tools AsHandler) -> SomeRoleConfig
```

### Minimal Role Example

```haskell
module ResearcherRole where

import ExoMonad
import ExoMonad.Guest.Tool.Class
import ExoMonad.Guest.Tool.Schema

-- Tool: search the web
data SearchArgs = SearchArgs { saQuery :: Text }
  deriving (Generic, FromJSON)

data SearchWeb
instance MCPTool SearchWeb where
  type ToolArgs SearchWeb = SearchArgs
  toolName = "search_web"
  toolDescription = "Search the web for information"
  toolSchema = genericToolSchemaWith @SearchArgs
    [("query", "Search query string")]
  toolHandlerEff args = do
    result <- suspendEffect @ProcessRun $ RunRequest
      { prCommand = "curl"
      , prArgs = ["-s", "https://api.example.com/search?q=" <> saQuery args]
      , prWorkingDir = "."
      , prEnv = []
      , prTimeoutMs = 10000
      }
    case result of
      Right resp -> pure $ successResult (object ["results" .= prStdout resp])
      Left err -> pure $ errorResult (effectErrorMessage err)

-- Tool record
data Tools mode = Tools
  { searchWeb :: mode :- SearchWeb
  } deriving Generic

-- Role config
config :: RoleConfig (Tools AsHandler)
config = RoleConfig
  { roleName = "researcher"
  , tools = Tools { searchWeb = mkHandler @SearchWeb }
  , hooks = HookConfig
      { preToolUse    = \_ -> pure (allowResponse Nothing)
      , postToolUse   = \_ -> pure (postToolUseResponse Nothing)
      , onStop        = \_ -> pure allowStopResponse
      , onSubagentStop = \_ -> pure allowStopResponse
      , onSessionStart = defaultSessionStartHook
      }
  , eventHandlers = defaultEventHandlers
  }
```

---

## Permissions DSL

Declare tool permissions for spawned Claude agents:

```haskell
data ToolPattern
  = ReadPat Text      -- Read(pattern)
  | EditPat Text      -- Edit(pattern)
  | BashPat Text      -- Bash(pattern)
  | CustomPat Text    -- Arbitrary pattern

data ClaudePermissions = ClaudePermissions
  { cpAllow :: [ToolPattern]
  , cpDeny  :: [ToolPattern]
  }

renderToolPattern :: ToolPattern -> Text
```

**Example:**

```haskell
permissions = ClaudePermissions
  { cpAllow = [ReadPat "src/**", EditPat "src/**", BashPat "cargo test*"]
  , cpDeny = [EditPat "../**", BashPat "rm *"]
  }
```

**Module:** `ExoMonad.Guest.Types.Permissions`

---

## FFI Boundary

The WASM module exports four entry points called by the Rust host:

| Export | Purpose |
|--------|---------|
| `handle_list_tools` | List available tools for a role |
| `handle_mcp_call` | Dispatch a tool call to its handler |
| `handle_pre_tool_use` | Dispatch hook events (PreToolUse, PostToolUse, SessionStart, Stop) |
| `handle_event` | Dispatch world events (PR review, CI, timeout, sibling merge) |
| `resume` | Resume a suspended continuation |

All communication uses JSON (tool calls) or protobuf (effects) over the Extism host function boundary. The single host function `yield_effect` carries all effect requests/responses as protobuf-encoded `EffectEnvelope` messages.

**Main.hs pattern:**

```haskell
module Main where

import AllRoles (allConfigs, lookupRole)
import ExoMonad.Guest.Dispatch (dispatchMCP, dispatchHook, dispatchEvent, resumeHandler)
import Extism.PDK (input, output)

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall handle_event :: IO CInt
foreign export ccall resume :: IO CInt

handle_mcp_call = do
  inp <- input @ByteString
  -- Parse role + tool name + args, look up role, dispatch
  ...

handle_list_tools = do
  inp <- input @ByteString
  -- Parse role, look up role, return tool definitions
  ...
```

---

## Project Layout

```
.exo/
  roles/
    myrole/               # Your role definitions
      AllRoles.hs         # Role registry (allConfigs)
      Main.hs             # FFI exports
      ResearcherRole.hs   # Role: tools + hooks + events
      WriterRole.hs
      myrole.cabal        # Cabal package for this role
  lib/                    # Shared hook/event implementations (optional)
  wasm/
    wasm-guest-myrole.wasm  # Compiled output (loaded by server)

haskell/
  wasm-guest/             # SDK (the library your roles depend on)
    src/ExoMonad/
      Guest/
        Tool/             # MCPTool, Schema, Mode, Record
        Tools/            # Built-in tool core functions (FilePR, Spawn, etc.)
        Effects/          # Effect GADTs (AgentControl, FileSystem)
        Events.hs         # Event handler types
        StateMachine.hs   # State machine framework
        Types.hs          # HookInput, HookOutput, MCPCallOutput
        Types/
          Permissions.hs  # Permission DSL
      Effects/            # Namespace effect types (Git, GitHub, Log, etc.)
      Types.hs            # RoleConfig, HookConfig, Effects type alias
```

---

## Building

```bash
# Build WASM for your role
exomonad recompile --role myrole

# Or build all roles via nix
just wasm-all

# Output: .exo/wasm/wasm-guest-myrole.wasm

# Hot reload: server checks WASM mtime per tool call
# Or force: exomonad reload
```

**Cabal project file:** `cabal.project.wasm` lists your role package alongside `wasm-guest` SDK. Build via `nix develop .#wasm -c wasm32-wasi-cabal build`.

---

## SDK Module Index

| Module | Purpose |
|--------|---------|
| `ExoMonad` | Top-level re-export |
| `ExoMonad.Guest.Tool.Class` | `MCPTool` typeclass, `ToolDefinition`, `MCPCallOutput` |
| `ExoMonad.Guest.Tool.Schema` | `genericToolSchema`, `genericToolSchemaWith` |
| `ExoMonad.Guest.Tool.Mode` | `AsSchema`, `AsHandler`, `ToolMode`, `:-` |
| `ExoMonad.Guest.Tool.Record` | `DispatchRecord`, `ReifyRecord` (Generic derivation) |
| `ExoMonad.Guest.Tool.SuspendEffect` | `suspendEffect`, `suspendEffect_` |
| `ExoMonad.Guest.Tools.*` | Built-in tool core functions |
| `ExoMonad.Guest.StateMachine` | `StateMachine` typeclass, `applyEvent`, `checkExit` |
| `ExoMonad.Guest.Events` | `EventHandlerConfig`, event types, `EventAction` |
| `ExoMonad.Guest.Types` | `HookInput`, `HookOutput`, `StopHookOutput` |
| `ExoMonad.Guest.Types.Permissions` | `ClaudePermissions`, `ToolPattern` |
| `ExoMonad.Types` | `RoleConfig`, `HookConfig`, `Effects` |
| `ExoMonad.Effects.*` | Namespace effect types (Git, GitHub, Log, Agent, etc.) |

---

## Key Constraints

1. **No direct I/O.** Tool handlers yield effects; Rust executes them. Never shell out from WASM.
2. **WASM lazy concat bug.** Avoid deep lazy list concatenation or large `mappend` chains in WASM handlers (GHC #25213 stack overflow). Assemble prompts as raw strict `Text`.
3. **freer-simple stack pressure.** Deep coroutine chains consume the 1MB WASM STG stack. Keep effect chains shallow; avoid lazy accumulation.
4. **Field name convention.** Record fields must use a common prefix (stripped) + camelCase. `saQuery` → `query`, `mtaFilePath` → `file_path`.
5. **One WASM per role set.** All roles for a project compile into a single `.wasm` file. The server dispatches by role name at runtime.
