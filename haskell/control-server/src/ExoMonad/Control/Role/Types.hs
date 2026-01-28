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
  ( -- * Tool Specification
    ToolSpec(..)

    -- * Mode Types
  , AsHandler
  , AsSchema

    -- * Handler Wrapper
  , ToolHandler(..)

    -- * Field Type Families
  , ToolField
  , ServerField

    -- * Hooks
  , Hooks(..)
  , emptyHooks
  , SessionStartInput(..)
  , SessionStartResponse(..)
  , PreToolUseInput(..)
  , PreToolUseResponse(..)
  , PostToolUseInput(..)
  , StopInput(..)
  , StopResponse(..)
  , StopReason(..)
  , Notification(..)

    -- * Role Metadata
  , RoleMetadata(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (Value, FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)

import ExoMonad.Graph.MCPReify (MCPToolInfo)

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL SPECIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for tool specifications.
--
-- Each tool is a standalone data type (often a unit type) that carries
-- metadata via this typeclass. The actual handler is provided when
-- constructing a server value.
--
-- @
-- data SpawnAgents = SpawnAgents
--
-- instance ToolSpec SpawnAgents where
--   type Args SpawnAgents = SpawnAgentsArgs
--   type Result SpawnAgents = SpawnAgentsResult
--   toolName = "spawn_agents"
--   toolDescription = "Spawn parallel worker agents"
-- @
class ToolSpec tool where
  -- | Input type for this tool (parsed from MCP call arguments)
  type Args tool :: Type

  -- | Output type for this tool (serialized to MCP call result)
  type Result tool :: Type

  -- | MCP tool name (snake_case by convention)
  toolName :: Text

  -- | Human-readable description for MCP tool listing
  toolDescription :: Text

-- ════════════════════════════════════════════════════════════════════════════
-- MODE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler mode: tools become handler functions.
--
-- @
-- spawnAgents :: ToolField AsHandler es SpawnAgents
-- -- resolves to: ToolHandler es SpawnAgents
-- @
data AsHandler (es :: [Type -> Type])

-- | Schema mode: tools become MCP tool info for discovery.
--
-- @
-- spawnAgents :: ToolField AsSchema es SpawnAgents
-- -- resolves to: MCPToolInfo
-- @
data AsSchema

-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER WRAPPER
-- ════════════════════════════════════════════════════════════════════════════

-- | GADT wrapper for tool handlers.
--
-- This wrapper captures the tool type at the value level, allowing
-- Generic-based dispatch to match on the handler type.
--
-- @
-- spawnAgentsHandler :: ToolHandler es SpawnAgents
-- spawnAgentsHandler = MkToolHandler $ \args -> do
--   -- implementation
--   pure result
-- @
newtype ToolHandler es tool = MkToolHandler
  { runToolHandler :: Args tool -> Eff es (Result tool)
  }

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD TYPE FAMILIES
-- ════════════════════════════════════════════════════════════════════════════

-- | Interpret a tool field based on the mode.
--
-- In handler mode, the field is a ToolHandler wrapper.
-- In schema mode, the field is MCPToolInfo for registration.
type family ToolField mode es tool where
  ToolField (AsHandler es) es' tool = ToolHandler es tool
  ToolField AsSchema es tool = MCPToolInfo

-- | Interpret a server field based on the mode.
--
-- Servers are themselves mode-parameterized, so this just passes through.
type family ServerField mode es server where
  ServerField mode es server = server mode es

-- ════════════════════════════════════════════════════════════════════════════
-- HOOKS
-- ════════════════════════════════════════════════════════════════════════════

-- | Lifecycle hooks for a role.
--
-- Each hook is optional (Maybe). The runtime invokes these at the appropriate
-- lifecycle points. Circuit breaker logic for 'stop' is handled by the runtime,
-- not the hook itself.
data Hooks es = Hooks
  { hooksSessionStart :: Maybe (SessionStartInput -> Eff es SessionStartResponse)
    -- ^ Called when a Claude session starts
  , hooksPreToolUse   :: Maybe (PreToolUseInput -> Eff es PreToolUseResponse)
    -- ^ Called before each tool invocation
  , hooksPostToolUse  :: Maybe (PostToolUseInput -> Eff es ())
    -- ^ Called after each tool invocation
  , hooksStop         :: Maybe (StopInput -> Eff es StopResponse)
    -- ^ Called when Claude wants to stop (runtime wraps with circuit breaker)
  , hooksNotification :: Maybe (Notification -> Eff es ())
    -- ^ Called for various notifications
  }

-- | Empty hooks (all Nothing).
emptyHooks :: Hooks es
emptyHooks = Hooks
  { hooksSessionStart = Nothing
  , hooksPreToolUse = Nothing
  , hooksPostToolUse = Nothing
  , hooksStop = Nothing
  , hooksNotification = Nothing
  }

-- | Input for session start hook.
data SessionStartInput = SessionStartInput
  { ssiSessionId :: Text
  , ssiCwd :: FilePath
  , ssiEnv :: [(Text, Text)]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionStartInput
instance ToJSON SessionStartInput

-- | Response from session start hook.
data SessionStartResponse = SessionStartResponse
  { ssrContinueSession :: Bool
    -- ^ If False, session is blocked
  , ssrInjectedContext :: Maybe Text
    -- ^ Optional context to inject into session
  }
  deriving (Show, Eq, Generic)

instance FromJSON SessionStartResponse
instance ToJSON SessionStartResponse

-- | Input for pre-tool-use hook.
data PreToolUseInput = PreToolUseInput
  { ptuToolName :: Text
  , ptuToolArgs :: Value
  , ptuSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PreToolUseInput
instance ToJSON PreToolUseInput

-- | Response from pre-tool-use hook.
data PreToolUseResponse
  = PTUAllow
    -- ^ Allow the tool call to proceed
  | PTUDeny Text
    -- ^ Block the tool call with a reason
  | PTUTransform Value
    -- ^ Allow with modified arguments
  deriving (Show, Eq, Generic)

instance FromJSON PreToolUseResponse
instance ToJSON PreToolUseResponse

-- | Input for post-tool-use hook.
data PostToolUseInput = PostToolUseInput
  { postuToolName :: Text
  , postuToolArgs :: Value
  , postuToolResult :: Value
  , postuSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PostToolUseInput
instance ToJSON PostToolUseInput

-- | Input for stop hook.
data StopInput = StopInput
  { siReason :: StopReason
  , siSessionId :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON StopInput
instance ToJSON StopInput

-- | Why the session is stopping.
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
  { srAllowStop :: Bool
    -- ^ If False, stop is blocked
  , srMessage :: Maybe Text
    -- ^ Optional message explaining why stop was blocked/allowed
  }
  deriving (Show, Eq, Generic)

instance FromJSON StopResponse
instance ToJSON StopResponse

-- | Notification types for notification hook.
data Notification
  = NotifyInfo Text
  | NotifyWarning Text
  | NotifyError Text
  deriving (Show, Eq, Generic)

instance FromJSON Notification
instance ToJSON Notification

-- ════════════════════════════════════════════════════════════════════════════
-- ROLE METADATA
-- ════════════════════════════════════════════════════════════════════════════

-- | Metadata for a role, used for logging and identification.
data RoleMetadata = RoleMetadata
  { rmSlug        :: Text
    -- ^ Short identifier (e.g., "tl", "dev", "pm")
  , rmDisplayName :: Text
    -- ^ Human-readable name (e.g., "Team Lead")
  , rmDescription :: Text
    -- ^ Description of the role's purpose
  }
  deriving (Show, Eq, Generic)

instance FromJSON RoleMetadata
instance ToJSON RoleMetadata
