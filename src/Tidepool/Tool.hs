-- | Tool types for mid-turn LLM capabilities
module Tidepool.Tool
  ( -- * Tool Class
    Tool(..)
  
    -- * Tool Execution
  , ToolResult(..)
  , executeTools
  , toolToJSON
  ) where

import Data.Text (Text)
import Data.Aeson (Value, ToJSON, FromJSON)
import GHC.Generics (Generic)
import Tidepool.Effect
import Effectful

-- | Tools are mid-turn capabilities the LLM can invoke
class Tool t where
  type ToolInput t
  type ToolOutput t
  
  toolName :: Text
  toolDescription :: Text
  inputSchema :: Value  -- JSON Schema
  
  executeTool 
    :: (State s :> es, Emit event :> es)
    => ToolInput t 
    -> Eff es (ToolOutput t)

-- | Result of tool execution for returning to LLM
data ToolResult = ToolResult
  { toolResultName :: Text
  , toolResultContent :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Execute a list of tool calls from LLM response
executeTools :: (State s :> es, Emit event :> es) => [Value] -> Eff es [ToolResult]
executeTools _toolCalls = error "TODO: executeTools - dispatch tool calls by name, execute, collect results"

-- | Convert tool definition to JSON for API
toolToJSON :: forall t. Tool t => Value
toolToJSON = error "TODO: toolToJSON - build Anthropic tool format from Tool typeclass"
