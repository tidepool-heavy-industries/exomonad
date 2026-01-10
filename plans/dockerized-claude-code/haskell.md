# Dockerized Claude Code - Haskell Integration Plan

**Status**: Deferred until Haskell codebase refactor settles.

## Overview

Haskell orchestrates Claude Code sessions by spawning `mantle session` processes. Each mantle process runs one session turn (spawns container, waits, returns JSON result, exits). Concurrency = multiple mantle processes, not one mantle managing many.

## Key Decisions

| Decision | Choice |
|----------|--------|
| Effect naming | `Session` (not Container - sessions are the persistent unit) |
| Replaces | `ClaudeCodeExec` - all Claude Code runs through sessions |
| Execution model | Each effect operation spawns `mantle session ...`, blocks for result |
| Orchestration | Haskell handles fork detection, parallel sessions, parent notification |
| State | Mantle persists to `.mantle/sessions.json`, Haskell is stateless |

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    HASKELL (orchestrator)                    │
│                                                              │
│  - Spawns `mantle session` processes (parallel or sequential)│
│  - Parses JSON results                                       │
│  - Detects fork signals in result.interrupts                 │
│  - Manages session lifecycle (start → continue → fork → done)│
└─────────────────────────────────────────────────────────────┘
        │               │               │
        │ spawn         │ spawn         │ spawn
        ▼               ▼               ▼
   ┌─────────┐     ┌─────────┐     ┌─────────┐
   │ mantle  │     │ mantle  │     │ mantle  │
   │ session │     │ session │     │ session │
   │ start A │     │ cont. B │     │ fork C  │
   └─────────┘     └─────────┘     └─────────┘
        │               │               │
        │ runs          │ runs          │ runs
        ▼               ▼               ▼
   ┌─────────┐     ┌─────────┐     ┌─────────┐
   │Container│     │Container│     │Container│
   │ (1 turn)│     │ (1 turn)│     │ (1 turn)│
   └─────────┘     └─────────┘     └─────────┘
        │               │               │
        └───────────────┴───────────────┘
                        │
                 JSON results back to Haskell
```

**Unix philosophy**: `mantle session` is a simple tool. Composition happens in Haskell.

---

## CLI Contract with Mantle

### `mantle session start`

```bash
mantle session start \
  --branch feat-x \
  --prompt "Implement feature X" \
  --model sonnet
```

### `mantle session continue`

```bash
mantle session continue <session-id> \
  --prompt "Now add tests"
```

### `mantle session fork`

```bash
mantle session fork <parent-session-id> \
  --child-branch feat-x-subtask \
  --child-prompt "SUBTASK: Handle edge case"
```

### `mantle session info`

```bash
mantle session info <session-id>
```

---

## Effect Type: Session

### Location (post-refactor)
TBD based on refactor structure

### JSON Output Schema (from Mantle)

All session commands (`start`, `continue`, `fork`) return this JSON to stdout:

```json
{
  "session_id": "abc-123-def",
  "branch": "feat-x",
  "worktree": "/repo/.mantle/worktrees/feat-x",
  "exit_code": 0,
  "is_error": false,
  "result_text": "I've implemented feature X...",
  "total_cost_usd": 0.05,
  "num_turns": 3,
  "interrupts": [
    {
      "signal_type": "fork",
      "state": "feat-x-subtask",
      "reason": "Handle edge case where input is empty"
    }
  ],
  "duration_secs": 45.2,
  "error": null
}
```

### Effect Definition

```haskell
{-# LANGUAGE DerivingVia, DataKinds, DeriveGeneric #-}

module Tidepool.Effects.Session
  ( Session(..)
  , SessionId(..)
  , SessionOutput(..)
  , SessionMetadata(..)
  , InterruptSignal(..)
  , startSession
  , continueSession
  , forkSession
  , sessionInfo
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Deriving.Aeson
import GHC.Generics (Generic)

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Result from a session turn (parsed from mantle JSON stdout)
-- Uses "so" prefix (SessionOutput) stripped via deriving-aeson
data SessionOutput = SessionOutput
  { soSessionId    :: SessionId      -- ^ Claude Code session ID
  , soBranch       :: Text           -- ^ Git branch name
  , soWorktree     :: FilePath       -- ^ Absolute path to worktree
  , soExitCode     :: Int            -- ^ Container exit code
  , soIsError      :: Bool           -- ^ Whether Claude reported an error
  , soResultText   :: Maybe Text     -- ^ Final output text
  , soTotalCostUsd :: Double         -- ^ API cost for this turn
  , soNumTurns     :: Int            -- ^ Number of turns in this run
  , soInterrupts   :: [InterruptSignal] -- ^ Fork requests, escalations, etc.
  , soDurationSecs :: Double         -- ^ Wall-clock duration
  , soError        :: Maybe Text     -- ^ Error if failed before Claude ran
  } deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
      via CustomJSON '[FieldLabelModifier '[StripPrefix "so", CamelToSnake]] SessionOutput

-- | Session metadata from `mantle session info`
data SessionMetadata = SessionMetadata
  { smSessionId     :: SessionId
  , smBranch        :: Text
  , smWorktree      :: FilePath
  , smParentSession :: Maybe SessionId
  , smChildSessions :: [SessionId]
  , smStatus        :: Text           -- ^ "idle", "active", "completed", "failed"
  , smCreatedAt     :: UTCTime
  , smUpdatedAt     :: UTCTime
  , smLastExitCode  :: Int
  , smTotalCostUsd  :: Double
  } deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
      via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SessionMetadata

-- | Signal from Claude (via `mantle signal` bash command)
data InterruptSignal = InterruptSignal
  { isSignalType :: Text           -- ^ "fork", "escalate", "transition", etc.
  , isState      :: Maybe Text     -- ^ e.g., child branch name for fork
  , isReason     :: Maybe Text     -- ^ e.g., child prompt for fork
  } deriving (Show, Generic)
    deriving (FromJSON, ToJSON)
      via CustomJSON '[FieldLabelModifier '[StripPrefix "is", CamelToSnake]] InterruptSignal

-- | Session effects - each operation spawns a mantle process
data Session r where
  -- | Start a new session (first turn)
  StartSession
    :: Text           -- ^ Branch name
    -> Text           -- ^ Initial prompt
    -> ModelChoice    -- ^ Model to use
    -> Session SessionOutput

  -- | Continue an existing session (subsequent turn)
  ContinueSession
    :: SessionId      -- ^ Session to continue
    -> Text           -- ^ Prompt for this turn
    -> Session SessionOutput

  -- | Fork a session (creates child session from parent's history)
  ForkSession
    :: SessionId      -- ^ Parent session
    -> Text           -- ^ Child branch name
    -> Text           -- ^ Child prompt
    -> Session SessionOutput  -- Returns child's first turn result

  -- | Get session info from .mantle/sessions.json
  SessionInfo
    :: SessionId
    -> Session (Maybe SessionMetadata)

-- Smart constructors
startSession :: Member Session effs => Text -> Text -> ModelChoice -> Eff effs SessionOutput
startSession branch prompt model = send $ StartSession branch prompt model

continueSession :: Member Session effs => SessionId -> Text -> Eff effs SessionOutput
continueSession sid prompt = send $ ContinueSession sid prompt

forkSession :: Member Session effs => SessionId -> Text -> Text -> Eff effs SessionOutput
forkSession parent childBranch childPrompt = send $ ForkSession parent childBranch childPrompt
```

---

## Effect Interpreter

### Implementation

```haskell
module Tidepool.Session.Interpreter
  ( runSession
  , SessionConfig(..)
  ) where

import System.Process (readProcessWithExitCode)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

data SessionConfig = SessionConfig
  { scMantlePath   :: FilePath    -- Path to mantle binary
  , scRepoRoot     :: FilePath    -- Repo root (for .mantle/ state)
  , scDefaultModel :: ModelChoice
  }

-- | Run Session effects by spawning mantle processes
runSession :: SessionConfig -> Session a -> IO a
runSession cfg = \case

  StartSession branch prompt model -> do
    let args =
          [ "session", "start"
          , "--branch", T.unpack branch
          , "--prompt", T.unpack prompt
          , "--model", modelToString model
          ]
    runMantle cfg args

  ContinueSession (SessionId sid) prompt -> do
    let args =
          [ "session", "continue", T.unpack sid
          , "--prompt", T.unpack prompt
          ]
    runMantle cfg args

  ForkSession (SessionId parentId) childBranch childPrompt -> do
    let args =
          [ "session", "fork", T.unpack parentId
          , "--child-branch", T.unpack childBranch
          , "--child-prompt", T.unpack childPrompt
          ]
    runMantle cfg args

  SessionInfo (SessionId sid) -> do
    let args = ["session", "info", T.unpack sid]
    (code, stdout, _) <- readProcessWithExitCode (scMantlePath cfg) args ""
    case code of
      ExitSuccess -> pure $ decode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout)
      ExitFailure _ -> pure Nothing

-- | Spawn mantle, wait for completion, parse JSON result
runMantle :: SessionConfig -> [String] -> IO SessionOutput
runMantle cfg args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode (scMantlePath cfg) args ""
  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
    Right result -> pure result
    Left err -> throwIO $ MantleParseError err stdout stderr
```

---

## Fork Signal Handling

Haskell detects fork requests by inspecting `soInterrupts` in the result:

```haskell
-- | Check if session requested a fork
detectForkRequest :: SessionOutput -> Maybe (Text, Text)  -- (childBranch, childPrompt)
detectForkRequest result =
  case find isForkSignal (soInterrupts result) of
    Just sig -> (,) <$> isState sig <*> isReason sig
    Nothing  -> Nothing
  where
    isForkSignal sig = isSignalType sig == "fork"

-- | Orchestration loop with fork handling
runSessionLoop :: SessionId -> Eff '[Session, ...] FinalResult
runSessionLoop sid = do
  result <- continueSession sid "Continue your work"

  case detectForkRequest result of
    Just (childBranch, childPrompt) -> do
      -- Fork detected: spawn child session
      childResult <- forkSession sid childBranch childPrompt

      -- Run child to completion (recursive)
      finalChildResult <- runChildToCompletion (soSessionId childResult)

      -- Notify parent and continue
      let summary = summarizeChildWork finalChildResult
      continueSession sid $ "Child task completed:\n\n" <> summary

      -- Continue parent loop
      runSessionLoop sid

    Nothing | isComplete result -> pure (extractFinalResult result)
    Nothing -> runSessionLoop sid  -- Continue
```

---

## Parallel Session Execution

For parallel worktrees, spawn multiple mantle processes:

```haskell
import Control.Concurrent.Async (mapConcurrently)

-- | Run multiple sessions in parallel
runParallelSessions :: [(Text, Text, ModelChoice)] -> IO [SessionOutput]
runParallelSessions tasks =
  mapConcurrently runOne tasks
  where
    runOne (branch, prompt, model) =
      runSession defaultConfig $ StartSession branch prompt model
```

---

## Usage Example

```haskell
-- Type-driven development workflow
typeDrivenWorkflow :: Task -> Eff '[Session, Log, ...] Result
typeDrivenWorkflow task = do
  -- Start main session
  result <- startSession (taskBranch task) (taskPrompt task) Sonnet
  let mainSession = soSessionId result

  -- Run to completion, handling forks
  loop mainSession result
  where
    loop sid result
      | isComplete result = pure (extractResult result)
      | Just (branch, prompt) <- detectForkRequest result = do
          -- Handle fork
          log $ "Fork requested: " <> branch
          childResult <- forkSession sid branch prompt
          finalChild <- runToCompletion (soSessionId childResult)

          -- Notify parent
          parentResult <- continueSession sid $
            "Subtask on " <> branch <> " completed:\n" <> summarize finalChild
          loop sid parentResult

      | otherwise = do
          -- Continue session
          nextResult <- continueSession sid "Continue"
          loop sid nextResult
```

---

## File Locations

Based on existing codebase structure:

### New Files

```
haskell/dsl/core/src/Tidepool/Effect/Session.hs    -- Effect type (replaces ClaudeCode.hs)

haskell/effects/session-executor/
├── tidepool-session-executor.cabal
└── src/Tidepool/Session/
    ├── Executor.hs         -- Spawns mantle processes
    ├── Types.hs            -- SessionOutput, InterruptSignal, SessionMetadata
    ├── Config.hs           -- SessionConfig
    └── Orchestration.hs    -- Fork detection, parallel helpers
```

### Files to Modify

```
haskell/dsl/core/src/Tidepool/Graph/Execute.hs     -- Update executeClaudeCodeHandler
haskell/dsl/core/src/Tidepool/Graph/Goto.hs        -- Update ClaudeCodeLLMHandler
haskell/native-server/src/Tidepool/Server/EffectRunner.hs  -- Replace ClaudeCodeExec with Session
cabal.project                                       -- Add session-executor package
```

### Files to Remove (After Migration)

```
haskell/dsl/core/src/Tidepool/Effect/ClaudeCode.hs -- Replaced by Session.hs
haskell/effects/claude-code-executor/              -- Entire package replaced
```

---

## Code Paths to Replace

### 1. Effect Type: `ClaudeCodeExec` → `Session`

**Current** (`Effect/ClaudeCode.hs`):
```haskell
-- Single operation with optional resume/fork as parameters
ClaudeCodeExecOp
  :: ModelChoice -> Maybe FilePath -> Text -> Maybe Value -> Maybe Text
  -> Maybe Text    -- ^ Session ID to resume (optional)
  -> Bool          -- ^ Fork session (optional)
  -> ClaudeCodeExec (Value, Maybe Text)
```

**New** (`Effect/Session.hs`):
```haskell
-- First-class operations for session lifecycle
data Session r where
  StartSession    :: Text -> Text -> ModelChoice -> Session SessionOutput
  ContinueSession :: SessionId -> Text -> Session SessionOutput
  ForkSession     :: SessionId -> Text -> Text -> Session SessionOutput
```

### 2. Executor Package: `claude-code-executor` → `session-executor`

| Old File | Status | Notes |
|----------|--------|-------|
| `Executor.hs` | **REPLACE** | Calls `mantle run` → new calls `mantle session start/continue/fork` |
| `Session.hs` | **REMOVE** | STM session tracking → now in mantle's `.mantle/sessions.json` |
| `SessionState.hs` | **REMOVE** | Same |
| `ControlSocket.hs` | **EVALUATE** | May reuse for hook events from container |
| `Hooks.hs` | **EVALUATE** | Hook handling may still be needed |
| `Types.hs` | **REPLACE** | New types from mantle JSON output |
| `Config.hs` | **REPLACE** | New config for session commands |

### 3. Graph Dispatch: `executeClaudeCodeHandler`

**Current** (`Graph/Execute.hs:256-319`):
```haskell
executeClaudeCodeHandler :: Member ClaudeCodeExec es => ...
  -- Calls execClaudeCode with forkSession=False
  (outputVal, sessionId) <- execClaudeCode model cwd fullPrompt schemaVal Nothing Nothing False
```

**New**: Two options:

**Option A**: Keep `ClaudeCode` annotation, change underlying effect
```haskell
executeClaudeCodeHandler :: Member Session es => ...
  -- Need to track session ID across self-loops
  result <- startSession branch fullPrompt model  -- or continueSession if resuming
```

**Option B**: New annotation type (breaking change)
```haskell
-- Graph annotation becomes Session-aware
gWork :: mode :- G.LLMNode :@ SessionNode 'Sonnet "feat-x"
```

**Recommendation**: Option A for backwards compatibility.

### 4. Effect Runner Composition

**Current** (`EffectRunner.hs:320-347`):
```haskell
runEffects :: ... -> Eff '[UI, Habitica, LLMComplete, ClaudeCodeExec, DevLog, Observability, IO] a
  ...
  . runClaudeCodeExecIO (ecClaudeCodeConfig $ eeConfig env)
```

**New**:
```haskell
runEffects :: ... -> Eff '[UI, Habitica, LLMComplete, Session, DevLog, Observability, IO] a
  ...
  . runSessionIO (ecSessionConfig $ eeConfig env)
```

### 5. Worktree Effect (Partial Overlap)

**Key insight**: Mantle now manages worktrees at `.mantle/worktrees/<branch>/`. The `Worktree` effect has overlap:

| Operation | Status | Rationale |
|-----------|--------|-----------|
| `CreateWorktree` | **OVERLAP** | Mantle creates worktrees internally |
| `DeleteWorktree` | **OVERLAP** | Mantle `cleanup` handles this |
| `ListWorktrees` | **OVERLAP** | Can query `mantle session list` |
| `MergeWorktree` | **KEEP** | Still need to merge child work to parent branch |
| `CherryPickFiles` | **KEEP** | Still need selective file operations |

**Decision**: Keep `Worktree` effect for merge/cherry-pick operations that happen *after* session completes. Mantle handles worktree lifecycle *during* sessions.

---

## Migration Strategy

### Phase 1: Add Session Effect (Non-Breaking)

- [ ] Create `Effect/Session.hs` with new effect type
- [ ] Create `session-executor` package
- [ ] Add to `cabal.project`
- [ ] Both `ClaudeCodeExec` and `Session` available in effect stack

### Phase 2: Migrate Graph Dispatch

- [ ] Update `executeClaudeCodeHandler` to use `Session` effect
- [ ] Add session ID tracking for self-loops (graph-level state)
- [ ] Keep `ClaudeCode` annotation working, just different underlying effect
- [ ] Update tests

### Phase 3: Update Effect Runner

- [ ] Replace `ClaudeCodeExec` with `Session` in `runEffects` type signature
- [ ] Update `ExecutorConfig` with `SessionConfig`
- [ ] Update environment variable loading

### Phase 4: Remove Deprecated Code

- [ ] Delete `Effect/ClaudeCode.hs`
- [ ] Delete `claude-code-executor` package
- [ ] Remove from `cabal.project`
- [ ] Update CLAUDE.md documentation

---

## Implementation Tasks

### Prerequisites
- [ ] Wait for Haskell refactor to settle
- [ ] Mantle `session` subcommand complete (start/continue/fork/info/list/cleanup)

### Phase 1: Add Session Effect
- [ ] Create `haskell/dsl/core/src/Tidepool/Effect/Session.hs`
- [ ] Create `haskell/effects/session-executor/` package structure
- [ ] Implement `SessionConfig` and `runSessionIO` interpreter
- [ ] Add `tidepool-session-executor` to `cabal.project`
- [ ] Unit tests for JSON parsing (SessionOutput, SessionMetadata, InterruptSignal)

### Phase 2: Migrate Graph Dispatch
- [ ] Update `executeClaudeCodeHandler` to use `Session` effect
- [ ] Handle session ID tracking for self-loops (graph Memory or separate state)
- [ ] Update `ClaudeCodeLLMHandler` and `ClaudeCodeResult` types
- [ ] Integration tests with mock mantle

### Phase 3: Update Effect Runner
- [ ] Add `Session` to `runEffects` effect stack
- [ ] Add `ecSessionConfig` to `ExecutorConfig`
- [ ] Add env vars: `MANTLE_PATH`, `MANTLE_REPO_ROOT`
- [ ] Update native server CLAUDE.md

### Phase 4: Remove Deprecated Code
- [ ] Delete `haskell/dsl/core/src/Tidepool/Effect/ClaudeCode.hs`
- [ ] Delete `haskell/effects/claude-code-executor/` package
- [ ] Remove `ClaudeCodeExec` from `EffectRunner.hs`
- [ ] Update `haskell/effects/CLAUDE.md` effect listing
- [ ] Update consuming repos (anemone)

---

## Dependencies

### Haskell Packages
- `deriving-aeson` - Type-level JSON customization (StripPrefix, CamelToSnake)
- `aeson` - JSON parsing
- `time` - UTCTime for timestamps

### External
- Mantle `session` subcommand complete (start/continue/fork/info)
- Mantle outputs JSON to stdout (SessionOutput format)
- `.mantle/sessions.json` for session metadata queries

---

## Notes

- **Haskell is the orchestrator**: Detects forks, manages parallel sessions, notifies parents
- **Mantle is the executor**: One process per turn, spawns container, returns result, exits
- **No async in interpreter**: Just `readProcessWithExitCode`, blocks until mantle exits
- **State lives in mantle**: `.mantle/sessions.json` persists across processes
- **Haskell is stateless**: All state queries go through `mantle session info`
