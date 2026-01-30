# Haskell Guest Architecture

Pure decision logic compiled to WASM. Yields effects, receives results.

## Package Structure

```
haskell/wasm-guest/
├── wasm-guest.cabal
├── src/
│   ├── Main.hs              # Entry points (foreign exports)
│   ├── Dispatch.hs          # Route to handlers
│   │
│   ├── ExoMonad/
│   │   ├── Guest/
│   │   │   ├── Extism.hs    # Extism PDK wrappers
│   │   │   ├── HostCall.hs  # Host function FFI
│   │   │   └── Effects.hs   # Effect → host call interpreter
│   │   │
│   │   └── Generated/
│   │       └── Effects.hs   # quicktype output
│   │
│   └── Handler/
│       ├── MCP.hs           # MCP tool dispatch
│       ├── Hook/
│       │   ├── PreToolUse.hs
│       │   ├── PostToolUse.hs
│       │   ├── SessionStart.hs
│       │   └── SessionEnd.hs
│       └── Role/
│           ├── TL.hs
│           ├── PM.hs
│           └── Dev.hs
```

## Cabal Configuration

```cabal
cabal-version: 3.0
name:          wasm-guest
version:       0.1.0.0

executable wasm-guest
  main-is:          Main.hs
  hs-source-dirs:   src
  default-language: GHC2021

  build-depends:
    , base >= 4.16 && < 5
    , extism-pdk
    , freer-simple
    , aeson
    , bytestring
    , text

  -- WASM-specific flags
  if arch(wasm32)
    ghc-options:
      -no-hs-main
      -optl-mexec-model=reactor
      -optl-Wl,--export=hs_init
      -optl-Wl,--export=handle_mcp_call
      -optl-Wl,--export=handle_pre_tool_use
      -optl-Wl,--export=handle_post_tool_use
      -optl-Wl,--export=handle_session_start
      -optl-Wl,--export=handle_session_end
      -optl-Wl,--allow-undefined
```

## Entry Points

```haskell
-- Main.hs
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Extism.PDK
import ExoMonad.Guest.Dispatch
import ExoMonad.Generated.Effects

-- No main function (reactor model)
-- main = pure ()

-- Foreign exports for Rust to call
foreign export ccall handle_mcp_call :: IO Int32
foreign export ccall handle_pre_tool_use :: IO Int32
foreign export ccall handle_post_tool_use :: IO Int32
foreign export ccall handle_session_start :: IO Int32
foreign export ccall handle_session_end :: IO Int32

handle_mcp_call :: IO Int32
handle_mcp_call = do
  input <- inputJSON @McpCallInput
  case input of
    Left err -> do
      outputJSON $ McpError ("JSON decode error: " <> err)
      pure 1
    Right req -> do
      result <- dispatchMcpCall req
      outputJSON result
      pure 0

handle_pre_tool_use :: IO Int32
handle_pre_tool_use = do
  input <- inputJSON @HookInput
  case input of
    Left err -> do
      outputJSON $ PreToolUseError err
      pure 1
    Right req -> do
      result <- dispatchPreToolUse req
      outputJSON result
      pure 0

-- Similar patterns for other entry points...
```

## Host Function FFI

```haskell
-- ExoMonad/Guest/HostCall.hs
{-# LANGUAGE ForeignFunctionInterface #-}

module ExoMonad.Guest.HostCall where

import Data.Word (Word64)
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.ByteString.Lazy qualified as BSL
import Extism.PDK.Memory

-- Foreign imports for host functions
foreign import ccall "git_get_branch" host_git_get_branch :: Word64 -> IO Word64
foreign import ccall "git_get_worktree" host_git_get_worktree :: Word64 -> IO Word64
foreign import ccall "github_list_issues" host_github_list_issues :: Word64 -> IO Word64
foreign import ccall "github_create_pr" host_github_create_pr :: Word64 -> IO Word64
foreign import ccall "docker_exec" host_docker_exec :: Word64 -> IO Word64
foreign import ccall "log_info" host_log_info :: Word64 -> IO ()
foreign import ccall "emit_event" host_emit_event :: Word64 -> IO ()

-- Generic caller for request/response pattern
callHost :: (ToJSON req, FromJSON resp) => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost hostFn request = do
  -- Allocate input in WASM memory
  let inputBytes = BSL.toStrict $ encode request
  inputOffset <- alloc inputBytes

  -- Call host function
  resultOffset <- hostFn (unMemoryOffset inputOffset)

  -- Read result from returned offset
  resultBytes <- load (MemoryOffset resultOffset)

  -- Free input memory (host manages result memory)
  free inputOffset

  -- Decode result
  case resultBytes of
    Left err -> pure $ Left err
    Right bytes -> case decode (BSL.fromStrict bytes) of
      Nothing -> pure $ Left "Failed to decode host response"
      Just r -> pure $ Right r

-- Fire-and-forget caller (no response)
callHostVoid :: ToJSON req => (Word64 -> IO ()) -> req -> IO ()
callHostVoid hostFn request = do
  let inputBytes = BSL.toStrict $ encode request
  inputOffset <- alloc inputBytes
  hostFn (unMemoryOffset inputOffset)
  free inputOffset
```

## Effect Interpreter

Bridge freer-simple effects to host calls.

```haskell
-- ExoMonad/Guest/Effects.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Guest.Effects where

import Control.Monad.Freer
import ExoMonad.Guest.HostCall
import ExoMonad.Generated.Effects

-- Effect algebra (matches schema)
data HostEffect r where
  GitGetBranch :: Text -> HostEffect Text
  GitGetWorktree :: Text -> HostEffect WorktreeInfo
  GitHubListIssues :: Repo -> Maybe IssueFilter -> HostEffect [Issue]
  GitHubCreatePR :: Repo -> CreatePRSpec -> HostEffect PullRequest
  DockerExec :: Text -> [Text] -> Maybe Text -> HostEffect ExecResult
  Log :: LogLevel -> Text -> HostEffect ()

-- Smart constructors
gitGetBranch :: Member HostEffect effs => Text -> Eff effs Text
gitGetBranch dir = send $ GitGetBranch dir

gitGetWorktree :: Member HostEffect effs => Text -> Eff effs WorktreeInfo
gitGetWorktree dir = send $ GitGetWorktree dir

logInfo :: Member HostEffect effs => Text -> Eff effs ()
logInfo msg = send $ Log Info msg

-- Interpreter: effects → host calls
runHostEffects :: Eff '[HostEffect, IO] a -> IO a
runHostEffects = runM . interpretM go
  where
    go :: HostEffect x -> IO x
    go (GitGetBranch dir) = do
      result <- callHost host_git_get_branch (GitGetBranchPayload dir)
      case result of
        Left err -> error $ "GitGetBranch failed: " <> err
        Right (EffectSuccess val) -> pure val
        Right (EffectError msg) -> error $ "GitGetBranch error: " <> msg

    go (GitGetWorktree dir) = do
      result <- callHost host_git_get_worktree (GitGetWorktreePayload dir)
      case result of
        Left err -> error $ "GitGetWorktree failed: " <> err
        Right (EffectSuccess val) -> pure val
        Right (EffectError msg) -> error $ "GitGetWorktree error: " <> msg

    go (GitHubListIssues repo filter_) = do
      result <- callHost host_github_list_issues (GitHubListIssuesPayload repo filter_)
      case result of
        Left err -> error $ "GitHubListIssues failed: " <> err
        Right (EffectSuccess val) -> pure val
        Right (EffectError msg) -> error $ "GitHubListIssues error: " <> msg

    go (Log level msg) = do
      callHostVoid host_log_info (LogPayload level msg)
```

## Handler Example

```haskell
-- Handler/MCP.hs
module Handler.MCP where

import Control.Monad.Freer
import ExoMonad.Guest.Effects
import ExoMonad.Generated.Effects
import Handler.Role.TL qualified as TL
import Handler.Role.PM qualified as PM
import Handler.Role.Dev qualified as Dev

dispatchMcpCall :: McpCallInput -> IO McpResponse
dispatchMcpCall input = runHostEffects $ do
  logInfo $ "MCP call: " <> input.toolName <> " for role " <> input.role

  case input.role of
    "tl" -> TL.handleMcpTool input.toolName input.arguments
    "pm" -> PM.handleMcpTool input.toolName input.arguments
    "dev" -> Dev.handleMcpTool input.toolName input.arguments
    other -> pure $ McpError $ "Unknown role: " <> other
```

```haskell
-- Handler/Role/TL.hs
module Handler.Role.TL where

import Control.Monad.Freer
import ExoMonad.Guest.Effects

handleMcpTool :: Member HostEffect effs => Text -> Value -> Eff effs McpResponse
handleMcpTool "file_pr" args = do
  -- Get current branch
  branch <- gitGetBranch "."
  worktree <- gitGetWorktree "."

  logInfo $ "Filing PR from branch: " <> branch

  -- Check if PR-able
  if "gh-" `isPrefixOf` branch
    then do
      let prSpec = CreatePRSpec
            { title = fromMaybe branch (args ^? key "title" . _String)
            , body = fromMaybe "" (args ^? key "body" . _String)
            , head = branch
            , base = "main"
            }
      pr <- gitHubCreatePR (Repo "tidepool-heavy-industries" "exomonad") prSpec
      pure $ McpSuccess $ object ["pr_url" .= pr.url]
    else
      pure $ McpError "Not on a PR-able branch (expected gh-* prefix)"

handleMcpTool tool _ =
  pure $ McpError $ "Unknown tool: " <> tool
```

## Build Process

```bash
# Build with wasm32-wasi-ghc
wasm32-wasi-cabal build wasm-guest

# Output: dist-newstyle/.../wasm-guest.wasm

# Copy to runtime
cp dist-newstyle/.../wasm-guest.wasm rust/exomonad-runtime/plugin.wasm
```

## Existing Code Migration

Most of current `control-server` logic can move with minimal changes:

| Current Location | New Location | Changes |
|------------------|--------------|---------|
| `Handler/Hook.hs` | `Handler/Hook/*.hs` | Remove IO interpreters |
| `Handler.hs` (MCP) | `Handler/MCP.hs` | Remove IO interpreters |
| `Role/Handlers.hs` | `Handler/Role/*.hs` | Pure logic stays same |
| `Runtime.hs` | Gone | Interpreters move to Rust |

The key change: instead of `runGitHubIO`, `runZellijIO`, etc., we call host functions.

## Testing

Can test in native mode (not WASM) by providing mock interpreters:

```haskell
-- test/Main.hs
runHostEffectsMock :: MockEnv -> Eff '[HostEffect, IO] a -> IO a
runHostEffectsMock env = runM . interpretM (mockGo env)
  where
    mockGo env (GitGetBranch _) = pure env.mockBranch
    mockGo env (GitGetWorktree _) = pure env.mockWorktree
    -- ...

spec :: Spec
spec = describe "TL.handleMcpTool" $ do
  it "files PR from gh- branch" $ do
    let env = MockEnv { mockBranch = "gh-123/test", mockWorktree = ... }
    result <- runHostEffectsMock env $ TL.handleMcpTool "file_pr" emptyObject
    result `shouldSatisfy` isSuccess
```

## Open Questions

1. **Error handling**: `error` calls in interpreter are ugly. Better pattern?
2. **State effect**: Include state in HostEffect or separate?
3. **Streaming logs**: Accumulate and return, or fire-and-forget to host?
