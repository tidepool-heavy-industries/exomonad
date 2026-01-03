
-- | Unified tool definitions for the Graph DSL.
--
-- Tools are mid-turn capabilities that LLM nodes can invoke. This module
-- provides a schema-driven tool system where:
--
-- * Input/output schemas are derived from types via 'HasJSONSchema'
-- * Tool definitions use the unified 'ToolDef' typeclass
-- * The tool marker type @t@ is passed to methods (can carry config)
-- * Effect stacks are declared as associated types
--
-- = Design Principle
--
-- Everything hangs off the 'ToolDef' typeclass:
--
-- * __Type level__: @ToolInput@, @ToolOutput@, @ToolEffects@ (associated types)
-- * __Value level__: @toolName@, @toolDescription@, @toolExecute@ (methods taking @t@)
--
-- = Defining a Tool
--
-- @
-- -- 1. Define input/output types with HasJSONSchema (via TH or manual)
-- data SearchInput = SearchInput { query :: Text, maxResults :: Int }
-- data SearchOutput = SearchOutput { results :: [Text] }
--
-- \$(deriveHasJSONSchema ''SearchInput)
-- \$(deriveHasJSONSchema ''SearchOutput)
--
-- -- 2. Define tool type (can be unit or carry config)
-- data SearchTool = SearchTool  -- or: SearchTool { apiKey :: Text }
--
-- -- 3. Define ToolDef instance
-- instance ToolDef SearchTool where
--   type ToolInput SearchTool = SearchInput
--   type ToolOutput SearchTool = SearchOutput
--   type ToolEffects SearchTool = '[]  -- or '[Reader Config, IOE]
--
--   toolName _ = "search"
--   toolDescription _ = "Search the knowledge base"
--   toolExecute _tool input = pure SearchOutput { results = [...] }
-- @
--
-- = Using Tools in Graphs
--
-- @
-- type MyGraph = Graph
--   '[ Entry :~> Query
--    , "search" := LLM
--        :@ Needs '[Query]
--        :@ Schema Results
--        :@ Tools '[SearchTool]
--    , Exit :<~ Results
--    ]
-- @
module Tidepool.Graph.Tool
  ( -- * Tool Definition
    ToolDef(..)

    -- * Tool Constraints
  , ValidTool
  , ValidToolList
  , AllToolsValid

    -- * Tool Info (Runtime)
  , ToolInfo(..)
  , toolInfoToJSON
  , toolToInfo

    -- * Tool List Reification
  , ReifyToolList(..)
  ) where

import Data.Aeson (Value, ToJSON, FromJSON, object, (.=))
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Control.Monad.Freer (Eff)

import Tidepool.Schema (HasJSONSchema(..), JSONSchema, schemaToValue)

-- | Effect type alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Unified tool definition typeclass.
--
-- Everything hangs off this typeclass:
--
-- * __Type level__: Input/output types and effect stack (associated types)
-- * __Value level__: Name, description, and implementation (methods take @t@)
--
-- The @t@ parameter is passed to all methods, allowing it to carry
-- configuration or state if needed.
--
-- = Defining a Tool
--
-- @
-- data SearchTool = SearchTool { apiKey :: Text }
--
-- instance ToolDef SearchTool where
--   type ToolInput SearchTool = SearchInput
--   type ToolOutput SearchTool = SearchOutput
--   type ToolEffects SearchTool = '[Reader Config, IOE]
--
--   toolName _ = "search"
--   toolDescription _ = "Search the knowledge base"
--   toolExecute tool input = do
--     config <- ask
--     liftIO $ search (tool.apiKey) config input.query
-- @
--
-- = Minimal definition
--
-- All associated types and methods must be provided.
class ( HasJSONSchema (ToolInput t)
      , HasJSONSchema (ToolOutput t)
      , FromJSON (ToolInput t)
      , ToJSON (ToolOutput t)
      , Typeable t
      , Typeable (ToolInput t)
      , Typeable (ToolOutput t)
      ) => ToolDef t where

  -- | The input type for this tool (parsed from LLM JSON)
  type ToolInput t :: Type

  -- | The output type for this tool (serialized back to LLM)
  type ToolOutput t :: Type

  -- | The effect stack this tool requires.
  --
  -- The framework ensures these effects are available when the tool is called.
  -- Use @'[]@ for pure tools.
  type ToolEffects t :: [Effect]

  -- | Tool name (used in API calls).
  --
  -- The @t@ value is passed in case the tool marker carries config.
  toolName :: t -> Text

  -- | Human-readable description (shown to LLM).
  --
  -- The @t@ value is passed in case the tool marker carries config.
  toolDescription :: t -> Text

  -- | Tool implementation.
  --
  -- Takes the tool value (for config) and input, returns output in the
  -- tool's effect stack.
  toolExecute :: t -> ToolInput t -> Eff (ToolEffects t) (ToolOutput t)

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL CONSTRAINTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Constraint alias for a valid tool.
--
-- A tool is valid if it has a ToolDef instance, which guarantees:
--
-- * Input type has HasJSONSchema
-- * Output type has HasJSONSchema
-- * Both types are JSON-serializable
type ValidTool :: Type -> Constraint
type ValidTool t = ToolDef t

-- | Constraint that all tools in a list are valid.
type ValidToolList :: [Type] -> Constraint
type family ValidToolList tools where
  ValidToolList '[] = ()
  ValidToolList (t ': ts) = (ValidTool t, ValidToolList ts)

-- | Type family for Graph DSL validation - ensures all Tools annotations
-- contain valid tools.
type AllToolsValid :: [Type] -> Constraint
type family AllToolsValid tools where
  AllToolsValid '[] = ()
  AllToolsValid (t ': ts) = (ToolDef t, AllToolsValid ts)

-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME TOOL INFO
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime information about a tool.
--
-- Contains everything needed to:
--
-- * Generate the tool definition for the LLM API
-- * Dispatch tool calls at runtime
-- * Validate tool inputs/outputs
data ToolInfo = ToolInfo
  { tiName :: Text
  , tiDescription :: Text
  , tiInputSchema :: JSONSchema
  , tiOutputSchema :: JSONSchema
  , tiInputType :: TypeRep
  , tiOutputType :: TypeRep
  }
  deriving (Show, Eq)

-- | Convert tool info to Anthropic API format.
toolInfoToJSON :: ToolInfo -> Value
toolInfoToJSON ti = object
  [ "name" .= ti.tiName
  , "description" .= ti.tiDescription
  , "input_schema" .= schemaToValue ti.tiInputSchema
  ]

-- | Convert a tool instance to 'ToolInfo'.
--
-- This extracts the metadata (name, description, schemas) from a tool
-- instance using the 'ToolDef' typeclass methods.
--
-- @
-- data SearchTool = SearchTool
-- -- ... ToolDef instance ...
--
-- info = toolToInfo SearchTool
-- @
toolToInfo :: forall t. ToolDef t => t -> ToolInfo
toolToInfo tool = ToolInfo
  { tiName = toolName tool
  , tiDescription = toolDescription tool
  , tiInputSchema = jsonSchema @(ToolInput t)
  , tiOutputSchema = jsonSchema @(ToolOutput t)
  , tiInputType = typeRep (Proxy @(ToolInput t))
  , tiOutputType = typeRep (Proxy @(ToolOutput t))
  }

-- ════════════════════════════════════════════════════════════════════════════
-- TOOL LIST REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify a type-level list of tools to runtime info.
--
-- Requires a function to provide tool instances for each type.
-- This enables collecting tool metadata from a type-level list.
class ReifyToolList (tools :: [Type]) where
  -- | Reify all tools in the list to ToolInfo.
  --
  -- The caller must provide a way to get instances for each tool type.
  reifyToolList :: [ToolInfo]

instance ReifyToolList '[] where
  reifyToolList = []

-- Note: For ReifyToolList to work with ToolDef, we need a way to
-- obtain a value of type t. Options:
--
-- 1. Require Default t constraint
-- 2. Use undefined (works for name/description if they ignore the arg)
-- 3. Pass a tool registry
--
-- For now, we don't provide a general instance. Users should collect
-- tool values explicitly and map toolToInfo over them.
