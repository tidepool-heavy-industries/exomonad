-- | IO interpreter for ClaudeCodeExec effect.
--
-- This module provides the IO-based interpreter that shells out to zellij-cc.
-- The effect type itself is defined in tidepool-core (Tidepool.Effect.ClaudeCode).
--
-- = Usage
--
-- @
-- import Tidepool.Effect.ClaudeCode (ClaudeCodeExec, execClaudeCode)
-- import Tidepool.ClaudeCode.Effect (runClaudeCodeExecIO)
--
-- result <- runClaudeCodeExecIO config $ do
--   execClaudeCode Sonnet (Just "/my/project") prompt schema tools Nothing
-- @
module Tidepool.ClaudeCode.Effect
  ( -- * Effect (re-exported from tidepool-core)
    ClaudeCodeExec(..)
  , execClaudeCode
  , runClaudeCodeExec

    -- * IO Interpreter
  , runClaudeCodeExecIO
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text qualified as T

-- Re-export effect type from core
import Tidepool.Effect.ClaudeCode
  ( ClaudeCodeExec(..)
  , execClaudeCode
  , runClaudeCodeExec
  )

import Tidepool.ClaudeCode.Config (ClaudeCodeConfig)
import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ClaudeCodeError(..))
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest)


-- | Run ClaudeCodeExec effect using zellij-cc.
--
-- This is the main interpreter that shells out to the zellij-cc binary.
-- Fails explicitly if:
-- - zellij-cc process fails
-- - Claude Code reports an error (is_error = true)
-- - No structured output is returned
--
-- Returns @(structuredOutput, sessionId)@ on success.
runClaudeCodeExecIO
  :: LastMember IO effs
  => ClaudeCodeConfig
  -> Eff (ClaudeCodeExec ': effs) a
  -> Eff effs a
runClaudeCodeExecIO cfg = interpret $ \case
  ClaudeCodeExecOp model cwd prompt schema tools resumeSession -> sendM $ do
    -- Pass resumeSession, forkSession=False (we always mutate the session)
    result <- runClaudeCodeRequest cfg model cwd prompt schema tools resumeSession False
    case result of
      Left err ->
        error $ "ClaudeCode execution failed: " <> formatError err

      Right ccr
        | ccr.ccrIsError ->
            error $ "ClaudeCode reported error: " <>
              maybe "(no message)" T.unpack ccr.ccrResult

        | Just val <- ccr.ccrStructuredOutput ->
            pure (val, ccr.ccrSessionId)

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
