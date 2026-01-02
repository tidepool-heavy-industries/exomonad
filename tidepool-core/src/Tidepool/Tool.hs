{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tool types for mid-turn LLM capabilities
module Tidepool.Tool
  ( -- * Tool Class
    Tool(..)

    -- * Tool List (type-safe tool collection)
  , ToolList(..)
  , toolListToJSON

    -- * Type-Level Tool Reification
  , ReifyToolList(..)
  , toolsFromType
  , dispatcherFromType

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
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Tidepool.Effect (Emit, RequestInput, Random, State, ToolDispatcher, ToolResult(..))
import Tidepool.Schema (HasJSONSchema(..), schemaToValue)
import Control.Monad.Freer (Eff, Member)

-- | Tools are mid-turn capabilities the LLM can invoke
-- Tools can use Emit, RequestInput, Random, and State effects
-- Parameters: t=tool type, event=event type, state=game state type
class (FromJSON (ToolInput t), ToJSON (ToolOutput t)) => Tool t event state where
  type ToolInput t
  type ToolOutput t

  toolName :: Text
  toolDescription :: Text
  inputSchema :: Value  -- JSON Schema

  -- | Execute the tool with its input
  -- Tools have access to state, events, input requests, and randomness
  executeTool
    :: (Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
    => ToolInput t
    -> Eff effs (ToolOutput t)

-- ══════════════════════════════════════════════════════════════
-- TOOL LIST
-- ══════════════════════════════════════════════════════════════

-- | Type-safe heterogeneous list of tools
-- Used to define which tools are available for a template
-- All tools in the list share the same event and state types
data ToolList event state (tools :: [Type]) where
  TNil  :: ToolList event state '[]
  TCons :: Tool t event state => Proxy t -> ToolList event state ts -> ToolList event state (t ': ts)

-- | Convert tool list to JSON array of tool definitions
toolListToJSON :: forall event state tools. ToolList event state tools -> [Value]
toolListToJSON TNil = []
toolListToJSON (TCons p rest) = toolToJSONProxy p : toolListToJSON rest
  where
    toolToJSONProxy :: forall t. Tool t event state => Proxy t -> Value
    toolToJSONProxy _ = toolToJSON @t @event @state

-- ══════════════════════════════════════════════════════════════
-- TYPE-LEVEL TOOL REIFICATION
-- ══════════════════════════════════════════════════════════════

-- | Reify a type-level list of tools into a term-level ToolList
--
-- This lets you go from:
--   type instance PhaseTools 'Scene = '[ThinkAsDM, SpeakAsNPC]
--
-- To term-level values via:
--   toolsFromType @(PhaseTools 'Scene) @MyEvent @MyState
--
class ReifyToolList (tools :: [Type]) event state where
  reifyToolList :: ToolList event state tools

instance ReifyToolList '[] event state where
  reifyToolList = TNil

instance (Tool t event state, ReifyToolList ts event state)
      => ReifyToolList (t ': ts) event state where
  reifyToolList = TCons (Proxy @t) reifyToolList

-- | Get tool JSON definitions from a type-level tool list
--
-- Example:
--   type instance PhaseTools 'Scene = '[ThinkAsDM, SpeakAsNPC]
--   pcTools = toolsFromType @(PhaseTools 'Scene) @DMEvent @WorldState
--
toolsFromType :: forall tools event state. ReifyToolList tools event state => [Value]
toolsFromType = toolListToJSON (reifyToolList @tools @event @state)

-- | Create a dispatcher from a type-level tool list
--
-- Example:
--   dispatcher = dispatcherFromType @(PhaseTools 'Scene) @DMEvent @WorldState
--
dispatcherFromType
  :: forall tools event state effs.
     (ReifyToolList tools event state, Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => ToolDispatcher event effs
dispatcherFromType = makeDispatcher (reifyToolList @tools @event @state)

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
  :: forall event state tools effs.
     (Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => ToolList event state tools
  -> ToolDispatcher event effs
makeDispatcher = dispatchToList

-- | Dispatch to a tool list, finding the matching tool by name
-- Returns ToolSuccess on normal completion, Left on error
-- Note: For tools that break turns (state transitions), they should
-- return ToolBreak from a separate dispatcher implementation
dispatchToList
  :: forall event state tools effs.
     (Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => ToolList event state tools
  -> Text
  -> Value
  -> Eff effs (Either Text ToolResult)
dispatchToList TNil name _ = pure $ Left $ "Unknown tool: " <> name
dispatchToList (TCons (_ :: Proxy t) rest) name input
  | toolName @t @event @state == name = executeSingleTool @t @event @state input
  | otherwise = dispatchToList rest name input

-- | Execute a single tool with JSON input/output
-- Returns ToolSuccess wrapping the output value
executeSingleTool
  :: forall t event state effs.
     (Tool t event state, Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => Value
  -> Eff effs (Either Text ToolResult)
executeSingleTool input = do
  case fromJSON input of
    Error err -> pure $ Left $ "Failed to parse tool input: " <> T.pack err
    Success parsedInput -> do
      result <- executeTool @t @event @state parsedInput
      pure $ Right $ ToolSuccess $ toJSON result

-- | Dispatch a single tool call
dispatchTool
  :: (Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => ToolDispatcher event effs
  -> Text    -- Tool name
  -> Value   -- Input JSON
  -> Eff effs (Either Text ToolResult)
dispatchTool dispatcher = dispatcher

-- | Execute a list of tool calls from LLM response
-- Stops early if any tool returns ToolBreak
executeTools
  :: (Member (State state) effs, Member (Emit event) effs, Member RequestInput effs, Member Random effs)
  => ToolDispatcher event effs
  -> [(Text, Text, Value)]   -- [(toolUseId, toolName, input)]
  -> Eff effs (Either Text [ToolExecResult])  -- Left = break, Right = all results
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
toolToJSON :: forall t event state. Tool t event state => Value
toolToJSON = object
  [ "name" .= toolName @t @event @state
  , "description" .= toolDescription @t @event @state
  , "input_schema" .= inputSchema @t @event @state
  ]
