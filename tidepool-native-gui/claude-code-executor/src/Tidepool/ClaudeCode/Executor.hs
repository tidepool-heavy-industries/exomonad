-- | Low-level executor for ClaudeCode - shells out to mantle.
--
-- This module provides the IO functions to run mantle commands.
-- The Effect module builds on this to provide an effect-based interface.
--
-- == Hook Support
--
-- When 'HookCallbacks' are provided, the executor starts a control socket
-- server that receives hook events from mantle and routes them to your
-- callbacks. This enables:
--
-- * Intercepting tool calls before execution (PreToolUse)
-- * Adding context after tool execution (PostToolUse)
-- * Controlling permission prompts (PermissionRequest)
-- * Preventing unwanted stops (Stop)
--
-- @
-- -- Example: Deny writes to /etc
-- let callbacks = defaultHookCallbacks
--       { hcOnPreToolUse = \\toolName input -> do
--           case toolName of
--             "Write" | isEtcPath input -> pure $ HookDeny "Cannot write to /etc"
--             _ -> pure HookAllow
--       }
-- result <- runClaudeCodeRequest cfg Sonnet Nothing "Do something" Nothing Nothing Nothing False (Just callbacks)
-- @
module Tidepool.ClaudeCode.Executor
  ( -- * Executor
    runClaudeCodeRequest
  , runClaudeCodeRequestWithHooks

    -- * Helpers
  , modelToString
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (Value, eitherDecode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Tidepool.Graph.Types (ModelChoice(..))
import Tidepool.ClaudeCode.Config (ClaudeCodeConfig(..))
import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ClaudeCodeError(..))
import Tidepool.ClaudeCode.Hooks (HookCallbacks)
import Tidepool.ClaudeCode.ControlSocket (withControlSocket)


-- | Run a ClaudeCode request by shelling out to mantle.
--
-- This is the simple version without hook callbacks. For hook support,
-- use 'runClaudeCodeRequestWithHooks'.
--
-- @
-- result <- runClaudeCodeRequest config Sonnet (Just "/my/project") "Analyze this code" schema tools Nothing False
-- case result of
--   Left err -> handleError err
--   Right ccr -> use ccr.ccrStructuredOutput
-- @
runClaudeCodeRequest
  :: ClaudeCodeConfig
  -> ModelChoice      -- ^ Model: Haiku, Sonnet, or Opus
  -> Maybe FilePath   -- ^ Working directory (Nothing inherits from runner)
  -> Text             -- ^ Rendered prompt
  -> Maybe Value      -- ^ JSON schema for structured output
  -> Maybe Text       -- ^ Tools to allow (comma-separated, e.g. "Glob,Read")
  -> Maybe Text       -- ^ Session ID to resume (for conversation continuity)
  -> Bool             -- ^ Fork session (read-only resume, doesn't modify original)
  -> IO (Either ClaudeCodeError ClaudeCodeResult)
runClaudeCodeRequest cfg model cwd prompt schema tools resumeSession forkSession =
  runClaudeCodeImpl cfg model cwd prompt schema tools resumeSession forkSession Nothing


-- | Run a ClaudeCode request with hook callbacks.
--
-- This version starts a control socket server that receives hook events
-- from mantle and routes them to your callbacks. Use this when you need
-- to intercept tool calls, add context, or control Claude Code behavior.
--
-- @
-- let callbacks = defaultHookCallbacks
--       { hcOnPreToolUse = \\toolName input -> do
--           putStrLn $ "Tool: " <> T.unpack toolName
--           pure HookAllow
--       }
-- result <- runClaudeCodeRequestWithHooks config callbacks Sonnet Nothing prompt schema tools Nothing False
-- @
runClaudeCodeRequestWithHooks
  :: ClaudeCodeConfig
  -> HookCallbacks    -- ^ Callbacks for hook events
  -> ModelChoice      -- ^ Model: Haiku, Sonnet, or Opus
  -> Maybe FilePath   -- ^ Working directory (Nothing inherits from runner)
  -> Text             -- ^ Rendered prompt
  -> Maybe Value      -- ^ JSON schema for structured output
  -> Maybe Text       -- ^ Tools to allow (comma-separated, e.g. "Glob,Read")
  -> Maybe Text       -- ^ Session ID to resume (for conversation continuity)
  -> Bool             -- ^ Fork session (read-only resume, doesn't modify original)
  -> IO (Either ClaudeCodeError ClaudeCodeResult)
runClaudeCodeRequestWithHooks cfg callbacks model cwd prompt schema tools resumeSession forkSession =
  withControlSocket callbacks $ \socketPath ->
    runClaudeCodeImpl cfg model cwd prompt schema tools resumeSession forkSession (Just socketPath)


-- | Internal implementation that runs mantle with optional socket path.
runClaudeCodeImpl
  :: ClaudeCodeConfig
  -> ModelChoice
  -> Maybe FilePath
  -> Text
  -> Maybe Value
  -> Maybe Text
  -> Maybe Text
  -> Bool
  -> Maybe FilePath   -- ^ Control socket path (for hook support)
  -> IO (Either ClaudeCodeError ClaudeCodeResult)
runClaudeCodeImpl cfg model cwd prompt schema tools resumeSession forkSession socketPath = do
  -- Build arguments
  let args = buildArgs cfg model cwd prompt schema tools resumeSession forkSession socketPath

  -- Run mantle
  result <- try $ readProcessWithExitCode cfg.ccZellijCcPath args ""
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ ClaudeCodeProcessError $
        "mantle failed to start: " <> T.pack (show e)

    Right (exitCode, stdout, stderr) ->
      case exitCode of
        ExitFailure code ->
          pure $ Left $ ClaudeCodeProcessError $
            "mantle exited with code " <> T.pack (show code) <> ": " <> T.pack stderr

        ExitSuccess -> do
          -- mantle prints JSON result to stdout
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
            Left parseErr ->
              pure $ Left $ ClaudeCodeParseError $
                "Failed to parse mantle output: " <> T.pack parseErr
                <> "\n--- Raw stdout (first 2000 chars) ---\n"
                <> T.pack (take 2000 stdout)
                <> if length stdout > 2000 then "\n... (truncated)" else ""

            Right ccResult ->
              pure $ Right ccResult


-- | Build command-line arguments for mantle.
buildArgs
  :: ClaudeCodeConfig
  -> ModelChoice
  -> Maybe FilePath
  -> Text
  -> Maybe Value
  -> Maybe Text
  -> Maybe Text       -- ^ Session ID to resume
  -> Bool             -- ^ Fork session
  -> Maybe FilePath   -- ^ Control socket path (for hook support)
  -> [String]
buildArgs cfg model cwd prompt schema tools resumeSession forkSession socketPath =
  [ "run"
  , "--session", T.unpack cfg.ccZellijSession
  , "--name", "cc-node"  -- Could make this configurable
  , "--model", modelToString model
  , "--prompt", T.unpack prompt
  , "--timeout", show cfg.ccDefaultTimeout
  ]
  ++ cwdArgs
  ++ schemaArgs
  ++ toolsArgs
  ++ resumeArgs
  ++ forkArgs
  ++ socketArgs
  where
    cwdArgs = case cwd of
      Nothing -> []
      Just dir -> ["--cwd", dir]

    schemaArgs = case schema of
      Nothing -> []
      Just s -> ["--json-schema", T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ encode s]

    toolsArgs = case tools of
      Nothing -> []
      Just t -> ["--tools", T.unpack t]

    resumeArgs = case resumeSession of
      Nothing -> []
      Just sid -> ["--resume", T.unpack sid]

    forkArgs = if forkSession then ["--fork-session"] else []

    socketArgs = case socketPath of
      Nothing -> []
      Just path -> ["--control-socket", path]


-- | Convert ModelChoice to mantle model string.
modelToString :: ModelChoice -> String
modelToString Haiku  = "haiku"
modelToString Sonnet = "sonnet"
modelToString Opus   = "opus"
