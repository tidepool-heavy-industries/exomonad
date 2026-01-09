-- | ClaudeCodeExec effect type for executing LLM nodes via Claude Code subprocess.
--
-- This effect is a sibling to the LLM effect. The graph's dispatch system
-- uses the ClaudeCode annotation to determine which effect to use.
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
-- myHandler :: Member ClaudeCodeExec es => ... -> Eff es Value
-- myHandler input = do
--   result <- execClaudeCode Sonnet (Just "/my/project") prompt schema tools Nothing
--   ...
-- @
--
-- The interpreter (in tidepool-claude-code-executor) runs zellij-cc which
-- spawns Claude Code in a zellij pane.
module Tidepool.Effect.ClaudeCode
  ( -- * Effect
    ClaudeCodeExec(..)

    -- * Smart Constructors
  , execClaudeCode

    -- * Pure Interpreter (for testing)
  , runClaudeCodeExec
  ) where

import Control.Monad.Freer (Eff, Member, interpret, send)
import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.Graph.Types (ModelChoice)


-- | Effect for executing ClaudeCode subprocess.
--
-- This is a sibling effect to LLM, not a wrapper. The type system determines
-- at compile time which effect a handler uses based on HasClaudeCode.
--
-- Returns @(structuredOutput, sessionId)@ where sessionId can be passed back
-- via resumeSession parameter to continue the conversation.
data ClaudeCodeExec r where
  ClaudeCodeExecOp
    :: ModelChoice      -- ^ Model: Haiku, Sonnet, Opus
    -> Maybe FilePath   -- ^ cwd (from annotation or Nothing to inherit)
    -> Text             -- ^ Rendered prompt
    -> Maybe Value      -- ^ JSON schema for structured output
    -> Maybe Text       -- ^ Tools to allow
    -> Maybe Text       -- ^ Session ID to resume (for conversation continuity)
    -> Bool             -- ^ Fork session (read-only resume, doesn't modify original)
    -> ClaudeCodeExec (Value, Maybe Text)  -- ^ (output, sessionId)


-- | Execute a ClaudeCode request.
--
-- Returns @(structuredOutput, sessionId)@ from Claude Code.
-- The sessionId can be passed to a subsequent call to continue the conversation.
-- Fails if Claude Code reports an error or returns no structured output.
execClaudeCode
  :: Member ClaudeCodeExec effs
  => ModelChoice      -- ^ Model: Haiku, Sonnet, Opus
  -> Maybe FilePath   -- ^ Working directory
  -> Text             -- ^ Rendered prompt
  -> Maybe Value      -- ^ JSON schema for structured output
  -> Maybe Text       -- ^ Tools to allow
  -> Maybe Text       -- ^ Session ID to resume (Nothing for new session)
  -> Bool             -- ^ Fork session (read-only resume for parallel agents)
  -> Eff effs (Value, Maybe Text)  -- ^ (output, sessionId)
execClaudeCode model cwd prompt schema tools resumeSession forkSession =
  send $ ClaudeCodeExecOp model cwd prompt schema tools resumeSession forkSession


-- | Run ClaudeCodeExec effect with a pure handler.
--
-- Used for testing or alternative implementations.
-- For IO-based execution, see @runClaudeCodeExecIO@ in tidepool-claude-code-executor.
runClaudeCodeExec
  :: (ModelChoice -> Maybe FilePath -> Text -> Maybe Value -> Maybe Text -> Maybe Text -> Bool -> Eff effs (Value, Maybe Text))
  -> Eff (ClaudeCodeExec ': effs) a
  -> Eff effs a
runClaudeCodeExec handler = interpret $ \case
  ClaudeCodeExecOp model cwd prompt schema tools resumeSession forkSession ->
    handler model cwd prompt schema tools resumeSession forkSession
