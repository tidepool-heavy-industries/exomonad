-- | Type-level MCP tool infrastructure.
--
-- Tools are defined as types with MCPTool instances.
-- Role tool sets are type-level lists.
-- Dispatch is derived automatically from the list.
module ExoMonad.Guest.Tool.Class
  ( -- * Tool typeclass
    MCPTool (..),

    -- * Tool definition (for serialization)
    ToolDefinition (..),
    toMCPFormat,
    mkToolDef,

    -- * Result types
    MCPCallOutput (..),
    WasmResult (..),
    EffectRequest (..),
    successResult,
    errorResult,

    -- * Coroutine suspension
    SuspendYield,
    ToolEffects,
    suspend,

    -- * Type-level list append
    type (:++),
  )
where

import Control.Monad.Freer (Eff, Member, sendM)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Effects.AgentControl (AgentControl)
import ExoMonad.Guest.Effects.FileSystem (FileSystem)
import ExoMonad.Guest.Tool.Suspend.Types (EffectRequest (..), SuspendYield, suspend)
import GHC.Generics (Generic)

-- ============================================================================
-- Tool Definition (for MCP discovery)
-- ============================================================================

-- | Tool definition for MCP discovery.
data ToolDefinition = ToolDefinition
  { tdName :: Text,
    tdDescription :: Text,
    tdInputSchema :: Aeson.Object
  }
  deriving (Show, Eq)

-- | Convert a tool definition to MCP JSON format.
toMCPFormat :: ToolDefinition -> Value
toMCPFormat t =
  object
    [ "name" .= tdName t,
      "description" .= tdDescription t,
      "inputSchema" .= Aeson.Object (tdInputSchema t)
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
-- Coroutine Suspension
-- ============================================================================

-- | Effect row for tool handlers that can suspend.
type ToolEffects = '[AgentControl, FileSystem, SuspendYield, IO]

-- ============================================================================
-- Async / Suspension Types
-- ============================================================================

-- | Async result from WASM execution.
data WasmResult a
  = Done a
  | Suspend Text EffectRequest
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (WasmResult a) where
  toJSON (Done val) = object ["tag" .= ("done" :: Text), "result" .= val]
  toJSON (Suspend k eff) =
    object
      [ "tag" .= ("suspend" :: Text),
        "continuation_id" .= k,
        "effect" .= eff
      ]

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
  toolSchema :: Aeson.Object

  -- | Effectful handler that can suspend via coroutine yield.
  toolHandlerEff :: ToolArgs t -> Eff ToolEffects MCPCallOutput

-- ============================================================================
-- Type-level list append
-- ============================================================================

-- | Append two type-level lists.
type family (:++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[] :++ ys = ys
  (x ': xs) :++ ys = x ': (xs :++ ys)
