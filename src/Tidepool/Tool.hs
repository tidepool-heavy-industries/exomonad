{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tool types for mid-turn LLM capabilities
module Tidepool.Tool
  ( -- * Tool Class
    Tool(..)

    -- * Tool List (type-safe tool collection)
  , ToolList(..)
  , toolListToJSON

    -- * Tool Execution
  , ToolResult(..)
  , executeTools
  , toolToJSON
  ) where

import Data.Text (Text)
import Data.Aeson (Value, ToJSON, FromJSON)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Tidepool.Effect
import Effectful

-- | Tools are mid-turn capabilities the LLM can invoke
-- Tools can use State, Emit, RequestInput, and Random effects
class Tool t where
  type ToolInput t
  type ToolOutput t

  toolName :: Text
  toolDescription :: Text
  inputSchema :: Value  -- JSON Schema

  -- | Execute the tool with its input
  -- Tools have access to game state, events, input requests, and randomness
  executeTool
    :: (State s :> es, Emit event :> es, RequestInput :> es, Random :> es)
    => ToolInput t
    -> Eff es (ToolOutput t)

-- ══════════════════════════════════════════════════════════════
-- TOOL LIST
-- ══════════════════════════════════════════════════════════════

-- | Type-safe heterogeneous list of tools
-- Used to define which tools are available for a template
data ToolList (tools :: [Type]) where
  TNil  :: ToolList '[]
  TCons :: Tool t => Proxy t -> ToolList ts -> ToolList (t ': ts)

-- | Convert tool list to JSON array of tool definitions
toolListToJSON :: ToolList tools -> [Value]
toolListToJSON TNil = []
toolListToJSON (TCons p rest) = toolToJSONProxy p : toolListToJSON rest
  where
    toolToJSONProxy :: forall t. Tool t => Proxy t -> Value
    toolToJSONProxy _ = toolToJSON @t

-- | Result of tool execution for returning to LLM
data ToolResult = ToolResult
  { toolResultName :: Text
  , toolResultContent :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Execute a list of tool calls from LLM response
executeTools
  :: (State s :> es, Emit event :> es, RequestInput :> es, Random :> es)
  => [Value]
  -> Eff es [ToolResult]
executeTools _toolCalls = error "TODO: executeTools - dispatch tool calls by name, execute, collect results"

-- | Convert tool definition to JSON for API
toolToJSON :: forall t. Tool t => Value
toolToJSON = error "TODO: toolToJSON - build Anthropic tool format from Tool typeclass"
