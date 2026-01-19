{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Runtime reification of MCP tools from graph EntryNode points.
--
-- Graphs with MCPExport-annotated entries can be exposed as MCP servers.
-- This module provides the machinery to extract tool definitions at runtime.
--
-- @
-- tools <- reifyMCPTools (Proxy \@SemanticScoutGraph)
-- -- [MCPToolInfo "map_influence" "Find all types..." {...}, ...]
-- @
module Tidepool.Graph.MCPReify
  ( -- * Tool Definition
    MCPToolInfo(..)

    -- * Reification (Legacy - MCPExport annotation)
  , ReifyMCPTools(..)
  , GReifyMCPEntries(..)

    -- * Reification (New - GraphEntries type family)
  , ReifyGraphEntries(..)
  , ReifyEntryList(..)
  ) where

import Data.Aeson (Value)
import Data.Char (toLower, isUpper)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Tidepool.Schema (HasJSONSchema(..), schemaToValue)
import Tidepool.Graph.Types (type (:@), MCPExport, MCPToolDef, GraphEntry(..), GraphEntries)
import Tidepool.Graph.Generic.Core (AsGraph, EntryNode, type (:-))
import Tidepool.Graph.Edges (HasMCPExport, GetMCPToolDef)

-- | An MCP tool definition extracted from a graph EntryNode.
data MCPToolInfo = MCPToolInfo
  { mtdName :: Text
    -- ^ Tool name (snake_case, from MCPToolDef or field name)
  , mtdDescription :: Text
    -- ^ Tool description (from MCPToolDef)
  , mtdInputSchema :: Value
    -- ^ JSON Schema for tool parameters (from HasJSONSchema)
  , mtdEntryName :: Text
    -- ^ Original field name in graph record (for dispatch)
  } deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- REIFICATION TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Types that can have their MCP tools reified at runtime.
--
-- @
-- tools <- reifyMCPTools (Proxy \@MyGraph)
-- @
class ReifyMCPTools (graph :: Type -> Type) where
  reifyMCPTools :: Proxy graph -> [MCPToolInfo]

instance (Generic (graph AsGraph), GReifyMCPEntries (Rep (graph AsGraph)))
      => ReifyMCPTools graph where
  reifyMCPTools _ = gReifyMCPEntries (Proxy @(Rep (graph AsGraph)))

-- ════════════════════════════════════════════════════════════════════════════
-- GENERIC TRAVERSAL
-- ════════════════════════════════════════════════════════════════════════════

-- | Walk Generic Rep collecting MCPToolInfo.
class GReifyMCPEntries (rep :: Type -> Type) where
  gReifyMCPEntries :: Proxy rep -> [MCPToolInfo]

-- Unwrap metadata
instance GReifyMCPEntries inner => GReifyMCPEntries (M1 D meta inner) where
  gReifyMCPEntries _ = gReifyMCPEntries (Proxy @inner)

instance GReifyMCPEntries inner => GReifyMCPEntries (M1 C meta inner) where
  gReifyMCPEntries _ = gReifyMCPEntries (Proxy @inner)

-- Product: combine both sides
instance (GReifyMCPEntries left, GReifyMCPEntries right)
      => GReifyMCPEntries (left :*: right) where
  gReifyMCPEntries _ =
    gReifyMCPEntries (Proxy @left) ++ gReifyMCPEntries (Proxy @right)

-- Empty: no fields
instance GReifyMCPEntries U1 where
  gReifyMCPEntries _ = []

-- Field: check for MCPExport
instance ( KnownSymbol name
         , ReifyMCPField (HasMCPExport fieldType) name fieldType
         )
      => GReifyMCPEntries (M1 S ('MetaSel ('Just name) su ss ds) (K1 i fieldType)) where
  gReifyMCPEntries _ = reifyMCPField @(HasMCPExport fieldType) @name @fieldType Proxy

-- ════════════════════════════════════════════════════════════════════════════
-- FIELD REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Conditionally reify a field based on MCPExport presence.
class ReifyMCPField (hasMCP :: Bool) (name :: Symbol) (fieldType :: Type) where
  reifyMCPField :: Proxy fieldType -> [MCPToolInfo]

-- Not MCP-exported: skip
instance ReifyMCPField 'False name fieldType where
  reifyMCPField _ = []

-- MCP-exported: extract tool definition
instance ( KnownSymbol name
         , ExtractEntryInput fieldType
         , HasJSONSchema (EntryInputType fieldType)
         , ExtractToolMeta name fieldType
         )
      => ReifyMCPField 'True name fieldType where
  reifyMCPField _ = [MCPToolInfo
    { mtdName = getToolName @name @fieldType
    , mtdDescription = getToolDescription @name @fieldType
    , mtdInputSchema = schemaToValue (jsonSchema @(EntryInputType fieldType))
    , mtdEntryName = T.pack (symbolVal (Proxy @name))
    }]

-- ════════════════════════════════════════════════════════════════════════════
-- HELPER TYPE FAMILIES AND CLASSES
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract the input type from an EntryNode node definition.
class ExtractEntryInput (node :: Type) where
  type EntryInputType node :: Type

-- Base case: strip annotations to find EntryNode
instance {-# OVERLAPPABLE #-} ExtractEntryInput inner => ExtractEntryInput (inner :@ ann) where
  type EntryInputType (inner :@ ann) = EntryInputType inner

-- EntryNode with input type (for AsGraph mode, this is EntryNode t directly)
instance ExtractEntryInput (EntryNode t) where
  type EntryInputType (EntryNode t) = t

class ExtractToolMeta (name :: Symbol) (node :: Type) where
  getToolName :: Text
  getToolDescription :: Text

-- Default: use field name as tool name
instance {-# OVERLAPPABLE #-} KnownSymbol name => ExtractToolMeta name node where
  getToolName = T.pack (camelToSnake (symbolVal (Proxy @name)))
  getToolDescription = "No description provided"

-- With MCPToolDef: use provided name/description
instance (KnownSymbol toolName, KnownSymbol toolDesc)
      => ExtractToolMeta name (node :@ MCPToolDef '(toolName, toolDesc)) where
  getToolName = T.pack (symbolVal (Proxy @toolName))
  getToolDescription = T.pack (symbolVal (Proxy @toolDesc))

-- | Convert camelCase to snake_case.
camelToSnake :: String -> String
camelToSnake = concatMap go
  where
    go c
      | isUpper c = ['_', toLower c]
      | otherwise = [c]

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPHENTRIES-BASED REIFICATION (New simplified DSL)
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify MCP tools from a graph's 'GraphEntries' type family instance.
--
-- This is the new, simplified approach for graphs that don't need
-- Entry/Exit node ceremony. The graph declares its external entry points
-- via a type instance:
--
-- @
-- newtype FindCallers mode = FindCallers
--   { run :: mode :- LogicNode :@ Input Args :@ UsesEffects '[Return Result]
--   }
--
-- type instance GraphEntries FindCallers =
--   '[ "find_callers" ':~> '("run", FindCallersArgs, "Find call sites") ]
--
-- -- At runtime:
-- tools <- reifyGraphEntries (Proxy \@FindCallers)
-- -- [MCPToolInfo { mtdName = "find_callers", ... }]
-- @
class ReifyGraphEntries (graph :: Type -> Type) where
  reifyGraphEntries :: Proxy graph -> [MCPToolInfo]

-- | Default instance that delegates to 'ReifyEntryList'.
instance ReifyEntryList (GraphEntries graph) => ReifyGraphEntries graph where
  reifyGraphEntries _ = reifyEntryList (Proxy @(GraphEntries graph))

-- | Reify a type-level list of 'GraphEntry' values.
class ReifyEntryList (entries :: [GraphEntry]) where
  reifyEntryList :: Proxy entries -> [MCPToolInfo]

-- | Base case: empty list.
instance ReifyEntryList '[] where
  reifyEntryList _ = []

-- | Recursive case: reify head and append tail.
instance ( KnownSymbol toolName
         , KnownSymbol nodeField
         , KnownSymbol description
         , HasJSONSchema inputType
         , ReifyEntryList rest
         )
      => ReifyEntryList ((toolName ':~> '(nodeField, inputType, description)) ': rest) where
  reifyEntryList _ =
    let tool = MCPToolInfo
          { mtdName = T.pack (symbolVal (Proxy @toolName))
          , mtdDescription = T.pack (symbolVal (Proxy @description))
          , mtdInputSchema = schemaToValue (jsonSchema @inputType)
          , mtdEntryName = T.pack (symbolVal (Proxy @nodeField))
          }
    in tool : reifyEntryList (Proxy @rest)
