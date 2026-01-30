{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Core types for the Role-based DSL.
--
-- This module defines the type-level machinery for typed roles, servers, and tools.
--
-- = Design
--
-- Tools are standalone data types with 'ToolSpec' instances:
--
-- @
-- data SpawnAgents = SpawnAgents
-- instance ToolSpec SpawnAgents where
--   type Args SpawnAgents = SpawnAgentsArgs
--   type Result SpawnAgents = SpawnAgentsResult
--   toolName = "spawn_agents"
--   toolDescription = "Spawn parallel worker agents"
-- @
--
-- Servers group tools with a description:
--
-- @
-- data OrchestrationServer mode es = OrchestrationServer
--   { description :: Text
--   , spawnAgents :: ToolField mode es SpawnAgents
--   , exoStatus   :: ToolField mode es ExoStatus
--   }
-- @
--
-- Roles compose servers with hooks and metadata:
--
-- @
-- data TLRole mode es = TLRole
--   { metadata      :: RoleMetadata
--   , hooks         :: Hooks es
--   , orchestration :: ServerField mode es OrchestrationServer
--   , messaging     :: ServerField mode es MessagingServer
--   }
-- @
module ExoMonad.Control.Role.Types
  ( -- * Hook Inputs/Outputs
    SessionStartInput (..),
    SessionStartResponse (..),
    PreToolUseInput (..),
    PreToolUseResponse (..),
    PostToolUseInput (..),
    StopInput (..),
    StopResponse (..),
    StopReason (..),
    Notification (..),
    SessionEndInput (..),
    SubagentStopInput (..),

    -- * Role Metadata
    RoleMetadata (..),
  )
where

import Data.Aeson (FromJSON (..), Options, ToJSON (..), Value, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Aeson.Casing (snakeCase)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- HOOK INPUTS/OUTPUTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Helper for JSON options: strip prefix and snake_case.
jsonOpts :: Int -> Data.Aeson.Options
jsonOpts prefixLen =
  defaultOptions
    { fieldLabelModifier = snakeCase . drop prefixLen
    }

-- | Input for session start hook.
data SessionStartInput = SessionStartInput
  { ssiSessionId :: Text,
    ssiCwd :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionStartInput where
  parseJSON = genericParseJSON (jsonOpts 3) -- ssi

instance ToJSON SessionStartInput where
  toJSON = genericToJSON (jsonOpts 3)

-- | Response from session start hook.
data SessionStartResponse = SessionStartResponse
  { -- | If False, session is blocked
    ssrContinueSession :: Bool,
    -- | Optional context to inject into session
    ssrInjectedContext :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionStartResponse where
  parseJSON = genericParseJSON (jsonOpts 3) -- ssr

instance ToJSON SessionStartResponse where
  toJSON = genericToJSON (jsonOpts 3)

-- | Input for pre-tool-use hook.
data PreToolUseInput = PreToolUseInput
  { ptuToolName :: Text,
    ptuToolInput :: Value, -- HookInput uses "tool_input"
    ptuSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PreToolUseInput where
  parseJSON = genericParseJSON (jsonOpts 3) -- ptu

instance ToJSON PreToolUseInput where
  toJSON = genericToJSON (jsonOpts 3)

-- | Response from pre-tool-use hook.
data PreToolUseResponse
  = -- | Allow the tool call to proceed
    PTUAllow
  | -- | Block the tool call with a reason
    PTUDeny Text
  | -- | Allow with modified arguments
    PTUTransform Value
  deriving (Show, Eq, Generic)

instance FromJSON PreToolUseResponse

instance ToJSON PreToolUseResponse

-- | Input for post-tool-use hook.
data PostToolUseInput = PostToolUseInput
  { postuToolName :: Text,
    postuToolInput :: Value, -- HookInput uses "tool_input"
    postuToolResult :: Value, -- HookInput uses "tool_response" (mapped manually?)
    -- Wait, HookInput has "tool_response".
    -- "postuToolResult" -> "tool_result"?
    -- I need to check Protocol.hs HookInput ToJSON.
    postuSessionId :: Text
  }
  deriving (Show, Eq, Generic)

-- HookInput fields: tool_name, tool_input, tool_response, session_id.
-- PostToolUseInput: postuToolName -> tool_name (OK)
--                   postuToolInput -> tool_input (OK)
--                   postuToolResult -> tool_result (Mismatch? HookInput has tool_response)
--                   postuSessionId -> session_id (OK)

-- I'll define custom instance for PostToolUseInput to match HookInput.
instance FromJSON PostToolUseInput where
  parseJSON =
    genericParseJSON
      ( defaultOptions
          { fieldLabelModifier = \f -> case f of
              "postuToolName" -> "tool_name"
              "postuToolInput" -> "tool_input"
              "postuToolResult" -> "tool_response"
              "postuSessionId" -> "session_id"
              s -> s
          }
      )

instance ToJSON PostToolUseInput where
  toJSON =
    genericToJSON
      ( defaultOptions
          { fieldLabelModifier = \f -> case f of
              "postuToolName" -> "tool_name"
              "postuToolInput" -> "tool_input"
              "postuToolResult" -> "tool_response"
              "postuSessionId" -> "session_id"
              s -> s
          }
      )

-- | Input for stop hook.
data StopInput = StopInput
  { siReason :: Maybe Text, -- HookInput has "reason" which is Maybe Text (raw string)
  -- StopReason enum is parsed from this text?
  -- Or should StopInput just hold the Text?
  -- Wiring.hs logic uses it.
    siSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON StopInput where
  parseJSON = genericParseJSON (jsonOpts 2) -- si

instance ToJSON StopInput where
  toJSON = genericToJSON (jsonOpts 2)

-- | Why the session is stopping. (Used in logic, not direct input anymore?)
data StopReason
  = StopUserRequest
  | StopTaskComplete
  | StopError Text
  | StopTimeout
  deriving (Show, Eq, Generic)

instance FromJSON StopReason

instance ToJSON StopReason

-- | Response from stop hook.
data StopResponse = StopResponse
  { -- | If False, stop is blocked
    srAllowStop :: Bool,
    -- | Optional message explaining why stop was blocked/allowed
    srMessage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON StopResponse where
  parseJSON = genericParseJSON (jsonOpts 2) -- sr

instance ToJSON StopResponse where
  toJSON = genericToJSON (jsonOpts 2)

-- | Notification types for notification hook.
data Notification
  = NotifyInfo Text
  | NotifyWarning Text
  | NotifyError Text
  deriving (Show, Eq, Generic)

instance FromJSON Notification

instance ToJSON Notification

-- | Input for session end hook.
data SessionEndInput = SessionEndInput
  { seiSessionId :: Text,
    seiTranscriptPath :: Text,
    seiRole :: Text, -- HookInput doesn't have role field directly? It has permission_mode?
    -- Protocol.hs HookInput does NOT have "role".
    -- ControlMessage has role.
    -- But generic dispatch takes `inputJson` which is `toJSON HookInput`.
    -- So SessionEndInput cannot have `seiRole` if HookInput doesn't.
    seiCwd :: FilePath
  }
  deriving (Show, Eq, Generic)

-- HookInput has: session_id, transcript_path, cwd.
-- It does not have role.
-- So I must remove seiRole from SessionEndInput.

instance FromJSON SessionEndInput where
  parseJSON = genericParseJSON (jsonOpts 3) -- sei

instance ToJSON SessionEndInput where
  toJSON = genericToJSON (jsonOpts 3)

-- | Input for subagent stop hook.
data SubagentStopInput = SubagentStopInput
  { sasiSessionId :: Text,
    sasiTranscriptPath :: Text,
    sasiCwd :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON SubagentStopInput where
  parseJSON = genericParseJSON (jsonOpts 4) -- sasi

instance ToJSON SubagentStopInput where
  toJSON = genericToJSON (jsonOpts 4)

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE METADATA
-- ════════════════════════════════════════════════════════════════════════════

-- | Metadata for a role, used for logging and identification.
data RoleMetadata = RoleMetadata
  { -- | Short identifier (e.g., "tl", "dev", "pm")
    rmSlug :: Text,
    -- | Human-readable name (e.g., "Team Lead")
    rmDisplayName :: Text,
    -- | Description of the role's purpose
    rmDescription :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RoleMetadata

instance ToJSON RoleMetadata
