{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Tool types for mid-turn LLM capabilities
module Tidepool.Tool
  ( -- * Tool Class
    Tool(..)

    -- * Extra Effects Constraint
  , EffectsIn

    -- * Tool List (type-safe tool collection)
  , ToolList(..)
  , toolListToJSON

    -- * Tool Execution
  , ToolExecResult(..)
  , makeDispatcher
  , dispatchTool
  , executeTools
  , toolToJSON
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, ToJSON, FromJSON, object, (.=), fromJSON, toJSON, Result(..))
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Tidepool.Effect (Emit, RequestInput, Random, State, ToolDispatcher, ToolResult(..))
import Effectful

-- | Constraint that all effects in a list are available in the effect stack
type family EffectsIn (effs :: [Effect]) (es :: [Effect]) :: Constraint where
  EffectsIn '[] es = ()
  EffectsIn (e ': rest) es = (e :> es, EffectsIn rest es)

-- | Tools are mid-turn capabilities the LLM can invoke
-- Tools can use Emit, RequestInput, Random, and State effects
-- Plus any extra effects specified by the extraEs parameter
-- Parameters: t=tool type, event=event type, state=game state type, extraEs=extra effects
class (FromJSON (ToolInput t), ToJSON (ToolOutput t)) => Tool t event state (extraEs :: [Effect]) where
  type ToolInput t
  type ToolOutput t

  toolName :: Text
  toolDescription :: Text
  inputSchema :: Value  -- JSON Schema

  -- | Execute the tool with its input
  -- Tools have access to state, events, input requests, randomness,
  -- plus the extra effects from extraEs
  executeTool
    :: (State state :> es, Emit event :> es, RequestInput :> es, Random :> es, EffectsIn extraEs es)
    => ToolInput t
    -> Eff es (ToolOutput t)

-- ══════════════════════════════════════════════════════════════
-- TOOL LIST
-- ══════════════════════════════════════════════════════════════

-- | Type-safe heterogeneous list of tools
-- Used to define which tools are available for a template
-- All tools in the list share the same event, state, and extra effects types
data ToolList event state (extraEs :: [Effect]) (tools :: [Type]) where
  TNil  :: ToolList event state extraEs '[]
  TCons :: Tool t event state extraEs => Proxy t -> ToolList event state extraEs ts -> ToolList event state extraEs (t ': ts)

-- | Convert tool list to JSON array of tool definitions
toolListToJSON :: forall event state extraEs tools. ToolList event state extraEs tools -> [Value]
toolListToJSON TNil = []
toolListToJSON (TCons p rest) = toolToJSONProxy p : toolListToJSON rest
  where
    toolToJSONProxy :: forall t. Tool t event state extraEs => Proxy t -> Value
    toolToJSONProxy _ = toolToJSON @t @event @state @extraEs

-- | Result of tool execution for returning to LLM (used by executeTools)
data ToolExecResult = ToolExecResult
  { toolExecResultName :: Text
  , toolExecResultContent :: Value
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ══════════════════════════════════════════════════════════════
-- TOOL DISPATCHER
-- ══════════════════════════════════════════════════════════════

-- | Create a dispatcher from a tool list
makeDispatcher
  :: forall event state extraEs tools es.
     (State state :> es, Emit event :> es, RequestInput :> es, Random :> es, EffectsIn extraEs es)
  => ToolList event state extraEs tools
  -> ToolDispatcher event es
makeDispatcher tools name input = dispatchToList tools name input

-- | Dispatch to a tool list, finding the matching tool by name
-- Returns ToolSuccess on normal completion, Left on error
-- Note: For tools that break turns (state transitions), they should
-- return ToolBreak from a separate dispatcher implementation
dispatchToList
  :: forall event state extraEs tools es.
     (State state :> es, Emit event :> es, RequestInput :> es, Random :> es, EffectsIn extraEs es)
  => ToolList event state extraEs tools
  -> Text
  -> Value
  -> Eff es (Either Text ToolResult)
dispatchToList TNil name _ = pure $ Left $ "Unknown tool: " <> name
dispatchToList (TCons (_ :: Proxy t) rest) name input
  | toolName @t @event @state @extraEs == name = executeSingleTool @t @event @state @extraEs input
  | otherwise = dispatchToList rest name input

-- | Execute a single tool with JSON input/output
-- Returns ToolSuccess wrapping the output value
executeSingleTool
  :: forall t event state extraEs es.
     (Tool t event state extraEs, State state :> es, Emit event :> es, RequestInput :> es, Random :> es, EffectsIn extraEs es)
  => Value
  -> Eff es (Either Text ToolResult)
executeSingleTool input = do
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse tool input: " <> T.pack err
    Success parsedInput -> do
      result <- executeTool @t @event @state @extraEs parsedInput
      pure $ Right $ ToolSuccess $ toJSON result

-- | Dispatch a single tool call
dispatchTool
  :: (State state :> es, Emit event :> es, RequestInput :> es, Random :> es)
  => ToolDispatcher event es
  -> Text    -- Tool name
  -> Value   -- Input JSON
  -> Eff es (Either Text ToolResult)
dispatchTool dispatcher = dispatcher

-- | Execute a list of tool calls from LLM response
-- Stops early if any tool returns ToolBreak
executeTools
  :: (State state :> es, Emit event :> es, RequestInput :> es, Random :> es)
  => ToolDispatcher event es
  -> [(Text, Text, Value)]   -- [(toolUseId, toolName, input)]
  -> Eff es (Either Text [ToolExecResult])  -- Left = break, Right = all results
executeTools dispatcher calls = go calls []
  where
    go [] acc = pure $ Right (reverse acc)
    go ((_, name, input):rest) acc = do
      result <- dispatcher name input
      case result of
        Left err ->
          -- Tool error - include it and continue
          let res = ToolExecResult name (toJSON err)
          in go rest (res:acc)
        Right (ToolBreak reason) ->
          -- Tool broke the turn - stop immediately
          pure $ Left reason
        Right (ToolSuccess val) ->
          -- Tool succeeded - continue
          let res = ToolExecResult name val
          in go rest (res:acc)

-- | Convert tool definition to JSON for API (Anthropic tool format)
toolToJSON :: forall t event state (extraEs :: [Effect]). Tool t event state extraEs => Value
toolToJSON = object
  [ "name" .= toolName @t @event @state @extraEs
  , "description" .= toolDescription @t @event @state @extraEs
  , "input_schema" .= inputSchema @t @event @state @extraEs
  ]
