-- | Hook callback types for Claude Code control envelope.
--
-- Provides a record of callbacks that are invoked when Claude Code
-- triggers various hook events. Handlers return 'HookDecision' which
-- is converted to the appropriate response for Claude Code.
--
-- @
-- -- Example: Log all tool calls and deny writes to sensitive paths
-- myCallbacks = defaultHookCallbacks
--   { hcOnPreToolUse = \\toolName input -> do
--       putStrLn $ "Tool: " <> show toolName
--       if toolName == "Write" && isSensitivePath input
--         then pure $ HookDeny "Cannot write to sensitive path"
--         else pure HookAllow
--   }
-- @
module Tidepool.ClaudeCode.Hooks
  ( -- * Callback Record
    HookCallbacks(..)
  , defaultHookCallbacks

    -- * Re-exports
  , HookDecision(..)
  , HookInput(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.ClaudeCode.Types
  ( HookDecision(..)
  , HookInput(..)
  )


-- | Callbacks for hook events from Claude Code.
--
-- All callbacks run in IO and return a 'HookDecision' indicating
-- whether to allow, deny, modify, or block the hook.
--
-- Default behavior (via 'defaultHookCallbacks') is to allow everything.
data HookCallbacks = HookCallbacks
  { hcOnPreToolUse :: Text -> Value -> IO HookDecision
    -- ^ Called before tool execution.
    -- Args: tool name, tool input.
    -- Can return 'HookAllow', 'HookDeny', or 'HookModify'.

  , hcOnPostToolUse :: Text -> Value -> Value -> IO HookDecision
    -- ^ Called after tool execution.
    -- Args: tool name, tool input, tool response.
    -- Typically returns 'HookAllow' (can add context but not deny).

  , hcOnPermissionRequest :: Text -> Value -> IO HookDecision
    -- ^ Called when permission dialog would be shown.
    -- Args: tool name, tool input.
    -- Can return 'HookAllow', 'HookDeny', or 'HookModify'.

  , hcOnNotification :: Text -> Maybe Text -> IO HookDecision
    -- ^ Called when Claude Code sends a notification.
    -- Args: notification type, optional message.

  , hcOnStop :: IO HookDecision
    -- ^ Called when Claude Code wants to stop.
    -- Return 'HookBlock' with a reason to force continuation.

  , hcOnSubagentStop :: IO HookDecision
    -- ^ Called when a subagent (Task tool) finishes.

  , hcOnPreCompact :: Text -> IO HookDecision
    -- ^ Called before compact operation.
    -- Arg: trigger ("manual" or "auto").

  , hcOnSessionStart :: Text -> IO HookDecision
    -- ^ Called when session starts/resumes.
    -- Arg: source ("startup", "resume", "clear", "compact").

  , hcOnSessionEnd :: Text -> IO HookDecision
    -- ^ Called when session ends.
    -- Arg: reason.

  , hcOnUserPromptSubmit :: Text -> IO HookDecision
    -- ^ Called when user submits a prompt.
    -- Arg: prompt text.
  }


-- | Default callbacks that allow everything.
--
-- Use this as a base and override specific callbacks:
--
-- @
-- myCallbacks = defaultHookCallbacks
--   { hcOnPreToolUse = myPreToolUseHandler
--   }
-- @
defaultHookCallbacks :: HookCallbacks
defaultHookCallbacks = HookCallbacks
  { hcOnPreToolUse = \_ _ -> pure HookAllow
  , hcOnPostToolUse = \_ _ _ -> pure HookAllow
  , hcOnPermissionRequest = \_ _ -> pure HookAllow
  , hcOnNotification = \_ _ -> pure HookAllow
  , hcOnStop = pure HookAllow
  , hcOnSubagentStop = pure HookAllow
  , hcOnPreCompact = \_ -> pure HookAllow
  , hcOnSessionStart = \_ -> pure HookAllow
  , hcOnSessionEnd = \_ -> pure HookAllow
  , hcOnUserPromptSubmit = \_ -> pure HookAllow
  }
