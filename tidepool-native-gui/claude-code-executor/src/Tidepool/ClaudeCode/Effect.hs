-- | ClaudeCodeExec effect for executing LLM nodes via Claude Code subprocess.
--
-- This effect is a sibling to the LLM effect. The graph's NodeHandler type
-- family determines at compile time which effect to use based on whether
-- the node has a ClaudeCode annotation.
--
-- = Architecture
--
-- @
-- Graph Definition (type level)
--          │
--          ▼
-- HasClaudeCode type family
--          │
--     ┌────┴────┐
--     │         │
--     ▼         ▼
--  'False     'True
--     │         │
--     ▼         ▼
--   LLM      ClaudeCodeExec
-- @
--
-- = Usage
--
-- Handlers for ClaudeCode-annotated nodes use this effect:
--
-- @
-- myHandler :: ClaudeCodeExec :> es => ... -> Eff es Value
-- myHandler input = do
--   result <- execClaudeCode Sonnet (Just "/my/project") prompt schema tools
--   ...
-- @
--
-- The interpreter runs zellij-cc which spawns Claude Code in a zellij pane.
module Tidepool.ClaudeCode.Effect
  ( -- * Effect
    ClaudeCodeExec(..)

    -- * Smart Constructors
  , execClaudeCode

    -- * Interpreter
  , runClaudeCodeExec
  , runClaudeCodeExecIO
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, interpret, send, sendM)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T

import Tidepool.Graph.Types (ModelChoice)
import Tidepool.ClaudeCode.Config (ClaudeCodeConfig)
import Tidepool.ClaudeCode.Types (ClaudeCodeResult(..), ClaudeCodeError(..))
import Tidepool.ClaudeCode.Executor (runClaudeCodeRequest)


-- | Effect for executing ClaudeCode subprocess.
--
-- This is a sibling effect to LLM, not a wrapper. The type system determines
-- at compile time which effect a handler uses based on HasClaudeCode.
data ClaudeCodeExec r where
  ClaudeCodeExecOp
    :: ModelChoice      -- ^ Model: Haiku, Sonnet, Opus
    -> Maybe FilePath   -- ^ cwd (from annotation or Nothing to inherit)
    -> Text             -- ^ Rendered prompt
    -> Maybe Value      -- ^ JSON schema for structured output
    -> Maybe Text       -- ^ Tools to allow
    -> ClaudeCodeExec Value


-- | Execute a ClaudeCode request.
--
-- Returns the structured output from Claude Code.
-- Fails if Claude Code reports an error or returns no structured output.
execClaudeCode
  :: Member ClaudeCodeExec effs
  => ModelChoice      -- ^ Model: Haiku, Sonnet, Opus
  -> Maybe FilePath   -- ^ Working directory
  -> Text             -- ^ Rendered prompt
  -> Maybe Value      -- ^ JSON schema for structured output
  -> Maybe Text       -- ^ Tools to allow
  -> Eff effs Value
execClaudeCode model cwd prompt schema tools =
  send $ ClaudeCodeExecOp model cwd prompt schema tools


-- | Run ClaudeCodeExec effect with a pure handler.
--
-- Used for testing or alternative implementations.
runClaudeCodeExec
  :: (ModelChoice -> Maybe FilePath -> Text -> Maybe Value -> Maybe Text -> Eff effs Value)
  -> Eff (ClaudeCodeExec ': effs) a
  -> Eff effs a
runClaudeCodeExec handler = interpret $ \case
  ClaudeCodeExecOp model cwd prompt schema tools ->
    handler model cwd prompt schema tools


-- | Run ClaudeCodeExec effect using zellij-cc.
--
-- This is the main interpreter that shells out to the zellij-cc binary.
-- Fails explicitly if:
-- - zellij-cc process fails
-- - Claude Code reports an error (is_error = true)
-- - No structured output is returned
runClaudeCodeExecIO
  :: LastMember IO effs
  => ClaudeCodeConfig
  -> Eff (ClaudeCodeExec ': effs) a
  -> Eff effs a
runClaudeCodeExecIO cfg = interpret $ \case
  ClaudeCodeExecOp model cwd prompt schema tools -> sendM $ do
    result <- runClaudeCodeRequest cfg model cwd prompt schema tools
    case result of
      Left err ->
        error $ "ClaudeCode execution failed: " <> formatError err

      Right ccr
        | ccr.ccrIsError ->
            error $ "ClaudeCode reported error: " <>
              maybe "(no message)" T.unpack ccr.ccrResult

        | Just val <- ccr.ccrStructuredOutput ->
            pure val

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
