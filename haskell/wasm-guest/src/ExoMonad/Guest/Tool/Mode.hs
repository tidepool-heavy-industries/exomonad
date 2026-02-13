-- | Mode-parameterized tool infrastructure.
--
-- This module provides a Servant-style mode system for tool records:
--
-- @
-- data GitTools mode = GitTools
--   { gitBranch :: mode :- GitBranch
--   , gitStatus :: mode :- GitStatus
--   }
--   deriving Generic
-- @
--
-- Two modes are provided:
--
-- * 'AsSchema' - Fields become 'ToolDefinition' (for MCP discovery)
-- * 'AsHandler' - Fields become handler functions (for dispatch)
module ExoMonad.Guest.Tool.Mode
  ( -- * Mode class
    ToolMode (..),

    -- * Modes
    AsSchema,
    AsHandler,

    -- * Schema wrapper
    Schema (..),
    mkSchema,

    -- * Handler wrapper
    Handler (..),

    -- * Handler construction
    mkHandler,
  )
where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), ToolDefinition, WasmResult (..), mkToolDef)
import ExoMonad.Guest.Tool.Suspend (runToolEff)

-- ============================================================================
-- Mode Class
-- ============================================================================

-- | Mode class for interpreting tool fields.
--
-- The @(:-)@ type family maps a mode and tool type to a concrete field type.
class ToolMode mode where
  type mode :- (tool :: Type) :: Type

-- Low precedence, left-associative (same as Servant's (:>))
infixl 0 :-

-- ============================================================================
-- AsSchema Mode
-- ============================================================================

-- | Schema mode: fields become schema wrappers.
--
-- Used for MCP discovery (listing available tools).
data AsSchema

instance ToolMode AsSchema where
  type AsSchema :- tool = Schema tool

-- | Schema wrapper that preserves the tool type.
--
-- This newtype preserves the phantom tool type, allowing Generic
-- traversal to access the MCPTool constraint for reification.
newtype Schema tool = Schema {getSchema :: ToolDefinition}

-- | Construct a schema from an MCPTool instance.
mkSchema :: forall t. (MCPTool t) => Schema t
mkSchema = Schema (mkToolDef @t)

-- ============================================================================
-- AsHandler Mode
-- ============================================================================

-- | Handler mode: fields become handler wrappers.
--
-- Used for dispatch (executing tool calls).
data AsHandler

instance ToolMode AsHandler where
  type AsHandler :- tool = Handler tool

-- ============================================================================
-- Handler Wrapper
-- ============================================================================

-- | Handler wrapper that preserves the tool type.
--
-- This newtype preserves the phantom tool type, allowing Generic
-- traversal to access the MCPTool constraint for dispatch.
newtype Handler tool = Handler {runHandler :: ToolArgs tool -> IO (WasmResult MCPCallOutput)}

-- ============================================================================
-- Handler Construction
-- ============================================================================

-- | Construct a handler from an MCPTool instance.
--
-- This bridges the MCPTool typeclass to the record field format.
--
-- @
-- gitToolsHandler :: GitTools AsHandler
-- gitToolsHandler = GitTools
--   { gitBranch = mkHandler @GitBranch
--   , gitStatus = mkHandler @GitStatus
--   }
-- @
mkHandler :: forall t. (MCPTool t, FromJSON (ToolArgs t)) => Handler t
mkHandler = Handler (\args -> runToolEff (toolHandlerEff @t args))
