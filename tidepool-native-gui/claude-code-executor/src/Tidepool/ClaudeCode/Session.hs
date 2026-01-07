-- | Session tracking for ClaudeCode effect.
--
-- This module provides a stateful interpreter that tracks session IDs per node,
-- enabling conversation continuity across self-loop iterations in the graph.
--
-- = How It Works
--
-- @
-- Node: "review-code" (first iteration)
--   → execClaudeCode ... Nothing      -- No session to resume
--   → Claude Code returns session_id: "abc123"
--   → Store: {"review-code" → "abc123"}
--
-- Node: "review-code" (second iteration, self-loop)
--   → Lookup: {"review-code" → "abc123"}
--   → execClaudeCode ... (Just "abc123")  -- Resume session
--   → Claude Code continues conversation
-- @
--
-- = Usage
--
-- @
-- import Tidepool.ClaudeCode.Session
--
-- main = do
--   sessionStore <- newSessionStore
--   runClaudeCodeWithSession config sessionStore "my-node" $
--     execClaudeCode Sonnet cwd prompt schema tools Nothing
-- @
module Tidepool.ClaudeCode.Session
  ( -- * Session Store
    SessionStore
  , newSessionStore

    -- * Session-Aware Interpreter
  , runClaudeCodeWithSession
  ) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Tidepool.ClaudeCode.Config (ClaudeCodeConfig)
import Tidepool.ClaudeCode.Effect (ClaudeCodeExec(..))
import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ClaudeCodeError(..))
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest)


-- | Session store mapping node names to session IDs.
--
-- Thread-safe via STM. Shared across all graph executions.
type SessionStore = TVar (Map Text Text)


-- | Create a new empty session store.
newSessionStore :: IO SessionStore
newSessionStore = newTVarIO Map.empty


-- | Run ClaudeCodeExec effect with session tracking.
--
-- This interpreter:
-- 1. Looks up any existing session ID for the given node name
-- 2. Uses it as the resumeSession parameter (overriding any caller-provided value)
-- 3. Stores the returned session ID for future iterations
--
-- The nodeName should be stable across self-loop iterations.
runClaudeCodeWithSession
  :: LastMember IO effs
  => ClaudeCodeConfig
  -> SessionStore       -- ^ Session store (shared across calls)
  -> Text               -- ^ Current node name (key for session lookup)
  -> Eff (ClaudeCodeExec ': effs) a
  -> Eff effs a
runClaudeCodeWithSession cfg sessionStore nodeName = interpret $ \case
  ClaudeCodeExecOp model cwd prompt schema tools _callerSession _forkSession -> sendM $ do
    -- Look up existing session for this node (overrides caller-provided session)
    -- Note: We ignore forkSession here because session tracking always mutates
    existingSession <- atomically $ do
      store <- readTVar sessionStore
      pure $ Map.lookup nodeName store

    -- Run the request with session resumption if available (forkSession=False for tracking)
    result <- runClaudeCodeRequest cfg model cwd prompt schema tools existingSession False
    case result of
      Left err ->
        error $ "ClaudeCode execution failed: " <> formatError err

      Right ccr
        | ccr.ccrIsError ->
            error $ "ClaudeCode reported error: " <>
              maybe "(no message)" T.unpack ccr.ccrResult

        | Just val <- ccr.ccrStructuredOutput -> do
            -- Store the session ID for future iterations
            -- (always present in stream-json format)
            atomically $ modifyTVar' sessionStore (Map.insert nodeName ccr.ccrSessionId)
            pure (val, Just ccr.ccrSessionId)

        | otherwise ->
            error "ClaudeCode returned no structured output (schema validation may have failed)"


-- | Format ClaudeCodeError for error messages.
formatError :: ClaudeCodeError -> String
formatError = \case
  ClaudeCodeProcessError msg ->
    "Process error: " <> T.unpack msg
  ClaudeCodeParseError msg ->
    "Parse error: " <> T.unpack msg
  ClaudeCodeExecutionError msg ->
    "Execution error: " <> T.unpack msg
  ClaudeCodeNoOutput ->
    "No structured output returned"
