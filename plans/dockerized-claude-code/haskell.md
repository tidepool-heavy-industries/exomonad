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
module Tidepool.Effects.Session
  ( Session(..)
  , SessionId(..)
  , SessionOutput(..)
  , InterruptSignal(..)
  , startSession
  , continueSession
  , forkSession
  , sessionInfo
  ) where

import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype SessionId = SessionId { unSessionId :: Text }
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Result from a session turn (parsed from mantle JSON stdout)
-- Field names match mantle's snake_case output
data SessionOutput = SessionOutput
  { sessionId     :: SessionId      -- ^ Claude Code session ID
  , branch        :: Text           -- ^ Git branch name
  , worktree      :: FilePath       -- ^ Absolute path to worktree
  , exitCode      :: Int            -- ^ Container exit code
  , isError       :: Bool           -- ^ Whether Claude reported an error
  , resultText    :: Maybe Text     -- ^ Final output text
  , totalCostUsd  :: Double         -- ^ API cost for this turn
  , numTurns      :: Int            -- ^ Number of turns in this run
  , interrupts    :: [InterruptSignal] -- ^ Fork requests, escalations, etc.
  , durationSecs  :: Double         -- ^ Wall-clock duration
  , outputError   :: Maybe Text     -- ^ Error if failed before Claude ran
  } deriving (Show, Generic)

instance FromJSON SessionOutput where
  -- Use aeson options to convert snake_case JSON to camelCase Haskell
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    }

-- | Signal from Claude (via `mantle signal` bash command)
data InterruptSignal = InterruptSignal
  { signalType :: Text           -- ^ "fork", "escalate", "transition", etc.
  , state      :: Maybe Text     -- ^ e.g., child branch name for fork
  , reason     :: Maybe Text     -- ^ e.g., child prompt for fork
  } deriving (Show, Generic, FromJSON)

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

Haskell detects fork requests by inspecting `interrupts` in the result:

```haskell
-- | Check if session requested a fork
detectForkRequest :: SessionOutput -> Maybe (Text, Text)  -- (childBranch, childPrompt)
detectForkRequest result =
  case find isForkSignal (interrupts result) of
    Just sig -> (,) <$> state sig <*> reason sig
    Nothing  -> Nothing
  where
    isForkSignal sig = signalType sig == "fork"

-- | Orchestration loop with fork handling
runSessionLoop :: SessionId -> Eff '[Session, ...] FinalResult
runSessionLoop sid = do
  result <- continueSession sid "Continue your work"

  case detectForkRequest result of
    Just (childBranch, childPrompt) -> do
      -- Fork detected: spawn child session
      childResult <- forkSession sid childBranch childPrompt

      -- Run child to completion (recursive)
      finalChildResult <- runChildToCompletion (sessionId childResult)

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
  let mainSession = sessionId result

  -- Run to completion, handling forks
  loop mainSession result
  where
    loop sid result
      | isComplete result = pure (extractResult result)
      | Just (branch, prompt) <- detectForkRequest result = do
          -- Handle fork
          log $ "Fork requested: " <> branch
          childResult <- forkSession sid branch prompt
          finalChild <- runToCompletion (sessionId childResult)

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

## Files to Create (Post-Refactor)

```
<location TBD>/
├── Session.hs              -- Effect type definition
├── Session/
│   ├── Interpreter.hs      -- Spawns mantle processes
│   ├── Types.hs            -- SessionOutput, InterruptSignal, etc.
│   └── Orchestration.hs    -- Fork handling, parallel execution helpers
```

---

## Implementation Tasks

- [ ] Wait for Haskell refactor to settle
- [ ] Create `Session` effect type (location TBD)
- [ ] Create interpreter (spawn mantle, parse JSON)
- [ ] Implement fork signal detection
- [ ] Add parallel execution helpers
- [ ] Deprecate/remove `ClaudeCodeExec`
- [ ] Integration tests with mantle session commands
- [ ] Update consuming repos (anemone)

---

## Dependencies

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
