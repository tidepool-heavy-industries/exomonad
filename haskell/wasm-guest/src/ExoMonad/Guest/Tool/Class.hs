-- | Type-level MCP tool infrastructure.
--
-- Tools are defined as types with MCPTool instances.
-- Role tool sets are type-level lists.
-- Dispatch is derived automatically from the list.
module ExoMonad.Guest.Tool.Class
  ( -- * Tool typeclass
    MCPTool (..),

    -- * Dispatch typeclass
    DispatchTools (..),

    -- * Tool definition (for serialization)
    ToolDefinition (..),
    toMCPFormat,
    mkToolDef,

    -- * Result types
    MCPCallOutput (..),
    successResult,
    errorResult,

    -- * Type-level list append
    type (:++),
  )
where

import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T

-- ============================================================================
-- Tool Definition (for MCP discovery)
-- ============================================================================

-- | Tool definition for MCP discovery.
data ToolDefinition = ToolDefinition
  { tdName :: Text,
    tdDescription :: Text,
    tdInputSchema :: Value
  }
  deriving (Show, Eq)

-- | Convert a tool definition to MCP JSON format.
toMCPFormat :: ToolDefinition -> Value
toMCPFormat t =
  object
    [ "name" .= tdName t,
      "description" .= tdDescription t,
      "inputSchema" .= tdInputSchema t
    ]

-- | Create a tool definition from an MCPTool instance.
mkToolDef :: forall t. (MCPTool t) => ToolDefinition
mkToolDef =
  ToolDefinition
    { tdName = toolName @t,
      tdDescription = toolDescription @t,
      tdInputSchema = toolSchema @t
    }

-- ============================================================================
-- MCP Call Output
-- ============================================================================

-- | Output from an MCP tool call.
data MCPCallOutput = MCPCallOutput
  { success :: Bool,
    result :: Maybe Value,
    mcpError :: Maybe Text
  }
  deriving (Show, Eq)

instance ToJSON MCPCallOutput where
  toJSON (MCPCallOutput s r e) =
    object
      [ "success" .= s,
        "result" .= r,
        "error" .= e
      ]

-- | Create a successful result.
successResult :: Value -> MCPCallOutput
successResult v = MCPCallOutput True (Just v) Nothing

-- | Create an error result.
errorResult :: Text -> MCPCallOutput
errorResult msg = MCPCallOutput False Nothing (Just msg)

-- ============================================================================
-- MCPTool Typeclass
-- ============================================================================

-- | Typeclass for MCP tools.
--
-- Each tool is a type with an associated argument type and handler.
class MCPTool (t :: Type) where
  -- | The argument type for this tool (must have FromJSON).
  type ToolArgs t :: Type

  -- | Tool name (snake_case, e.g., "git_branch").
  toolName :: Text

  -- | Human-readable description.
  toolDescription :: Text

  -- | JSON Schema for the input.
  toolSchema :: Value

  -- | Handler that executes the tool.
  toolHandler :: ToolArgs t -> IO MCPCallOutput

-- ============================================================================
-- DispatchTools Typeclass
-- ============================================================================

-- | Typeclass for dispatching to a list of tools.
--
-- Derived automatically from a type-level list of tools.
class DispatchTools (tools :: [Type]) where
  -- | Dispatch a tool call by name.
  dispatch :: Text -> Value -> IO MCPCallOutput

  -- | Get all tool definitions for MCP discovery.
  toolDefs :: [ToolDefinition]

-- Base case: empty list
instance DispatchTools '[] where
  dispatch name _ = pure $ errorResult $ "Unknown tool: " <> name
  toolDefs = []

-- Inductive case: cons
instance
  (MCPTool t, FromJSON (ToolArgs t), DispatchTools ts) =>
  DispatchTools (t ': ts)
  where
  dispatch name args
    | name == toolName @t =
        case Aeson.fromJSON args of
          Aeson.Success a -> toolHandler @t a
          Aeson.Error e -> pure $ errorResult $ "Parse error for " <> name <> ": " <> T.pack e
    | otherwise = dispatch @ts name args
  toolDefs = mkToolDef @t : toolDefs @ts

-- ============================================================================
-- Type-level list append
-- ============================================================================

-- | Append two type-level lists.
type family (:++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)
