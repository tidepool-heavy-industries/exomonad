-- | JSON export for graph introspection data.
--
-- Converts GraphInfo/NodeInfo to Aeson-serializable types for:
-- - Tooling and LLM context
-- - D3 visualization
-- - API responses
module Tidepool.Graph.Export
  ( -- * Export Types
    GraphExport(..)
  , NodeExport(..)
  , EdgeExport(..)
  , SchemaExport(..)
  , TemplateExport(..)
  , MemoryExport(..)
  , ToolExport(..)

    -- * Conversion
  , graphToExport
  , graphToJSON
  ) where

import Data.Aeson (ToJSON(..), (.=), object, Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep)
import GHC.Generics (Generic)

import Tidepool.Graph.Reify
  ( GraphInfo(..)
  , NodeInfo(..)
  , EdgeInfo(..)
  , SchemaInfo(..)
  , TemplateInfo(..)
  , MemoryInfo(..)
  , ToolInfo(..)
  , RuntimeNodeKind(..)
  , RuntimeEdgeKind(..)
  )
import Tidepool.Schema (schemaToValue)

-- ════════════════════════════════════════════════════════════════════════════
-- EXPORT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Export format for a complete graph.
data GraphExport = GraphExport
  { geEntryType :: Maybe Text      -- ^ Entry type name
  , geExitType :: Maybe Text       -- ^ Exit type name
  , geNodes :: Map Text NodeExport -- ^ Nodes by name
  , geEdges :: [EdgeExport]        -- ^ All edges
  }
  deriving (Show, Eq, Generic)

instance ToJSON GraphExport where
  toJSON ge = object
    [ "entryType" .= ge.geEntryType
    , "exitType" .= ge.geExitType
    , "nodes" .= ge.geNodes
    , "edges" .= ge.geEdges
    ]

-- | Export format for a single node.
data NodeExport = NodeExport
  { neKind :: Text                     -- ^ "LLM" | "Logic"
  , neNeeds :: [Text]                  -- ^ Input type names
  , neSchema :: Maybe SchemaExport     -- ^ Structured output info
  , neTemplate :: Maybe TemplateExport -- ^ User template info
  , neSystem :: Maybe TemplateExport   -- ^ System template info
  , neTools :: [ToolExport]            -- ^ Available tools
  , neMemory :: Maybe MemoryExport     -- ^ Node-private state info
  , neTransitions :: [Text]            -- ^ Explicit Goto target names
  , neCanExit :: Bool                  -- ^ Has Goto Exit?
  , neHasVision :: Bool                -- ^ Vision enabled?
  }
  deriving (Show, Eq, Generic)

instance ToJSON NodeExport where
  toJSON ne = object
    [ "kind" .= ne.neKind
    , "needs" .= ne.neNeeds
    , "schema" .= ne.neSchema
    , "template" .= ne.neTemplate
    , "system" .= ne.neSystem
    , "tools" .= ne.neTools
    , "memory" .= ne.neMemory
    , "transitions" .= ne.neTransitions
    , "canExit" .= ne.neCanExit
    , "hasVision" .= ne.neHasVision
    ]

-- | Export format for an edge.
data EdgeExport = EdgeExport
  { eeFrom :: Text           -- ^ Source node name
  , eeTo :: Text             -- ^ Target node name
  , eePayload :: Maybe Text  -- ^ Payload type name
  , eeKind :: Text           -- ^ "implicit" | "explicit"
  }
  deriving (Show, Eq, Generic)

instance ToJSON EdgeExport where
  toJSON ee = object
    [ "from" .= ee.eeFrom
    , "to" .= ee.eeTo
    , "payload" .= ee.eePayload
    , "kind" .= ee.eeKind
    ]

-- | Export format for schema info.
data SchemaExport = SchemaExport
  { seTypeName :: Text                       -- ^ Type name (e.g., "Intent")
  , seFields :: [(Text, Text, Bool)]         -- ^ (name, type, required)
  , seJsonSchema :: Value                    -- ^ Full JSON schema
  }
  deriving (Show, Eq, Generic)

instance ToJSON SchemaExport where
  toJSON se = object
    [ "typeName" .= se.seTypeName
    , "fields" .= map fieldToJSON se.seFields
    , "jsonSchema" .= se.seJsonSchema
    ]
    where
      fieldToJSON (name, ty, req) = object
        [ "name" .= name
        , "type" .= ty
        , "required" .= req
        ]

-- | Export format for template info.
data TemplateExport = TemplateExport
  { teTypeName :: Text          -- ^ Template type name
  , tePath :: FilePath          -- ^ Template file path
  , teDeps :: [FilePath]        -- ^ Template dependencies
  , teAccessedFields :: [Text]  -- ^ Fields accessed in template
  , teContextType :: Text       -- ^ Context type name
  }
  deriving (Show, Eq, Generic)

instance ToJSON TemplateExport where
  toJSON te = object
    [ "typeName" .= te.teTypeName
    , "path" .= te.tePath
    , "deps" .= te.teDeps
    , "accessedFields" .= te.teAccessedFields
    , "contextType" .= te.teContextType
    ]

-- | Export format for memory info.
data MemoryExport = MemoryExport
  { meTypeName :: Text  -- ^ Memory type name
  }
  deriving (Show, Eq, Generic)

instance ToJSON MemoryExport where
  toJSON me = object
    [ "typeName" .= me.meTypeName
    ]

-- | Export format for tool info.
data ToolExport = ToolExport
  { txName :: Text            -- ^ Tool name
  , txDescription :: Text     -- ^ Tool description
  , txInputSchema :: Value    -- ^ Input JSON schema
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolExport where
  toJSON tx = object
    [ "name" .= tx.txName
    , "description" .= tx.txDescription
    , "inputSchema" .= tx.txInputSchema
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert GraphInfo to GraphExport.
graphToExport :: GraphInfo -> GraphExport
graphToExport gi = GraphExport
  { geEntryType = simplifyTypeName <$> gi.giEntryType
  , geExitType = simplifyTypeName <$> gi.giExitType
  , geNodes = Map.fromList
      [ (ni.niName, nodeToExport ni)
      | ni <- gi.giNodes
      ]
  , geEdges = map edgeToExport gi.giEdges
  }

-- | Convert GraphInfo to JSON Value directly.
graphToJSON :: GraphInfo -> Value
graphToJSON = toJSON . graphToExport

-- | Convert NodeInfo to NodeExport.
nodeToExport :: NodeInfo -> NodeExport
nodeToExport ni = NodeExport
  { neKind = kindToText ni.niKind
  , neNeeds = maybe [] (\t -> [simplifyTypeName t]) ni.niInput
  , neSchema = schemaToExport <$> ni.niSchema
  , neTemplate = templateToExport <$> ni.niTemplate
  , neSystem = templateToExport <$> ni.niSystem
  , neTools = map toolToExport ni.niToolInfos
  , neMemory = memoryToExport <$> ni.niMemory
  , neTransitions = map fst ni.niGotoTargets
  , neCanExit = ni.niHasGotoExit
  , neHasVision = ni.niHasVision
  }

-- | Convert EdgeInfo to EdgeExport.
edgeToExport :: EdgeInfo -> EdgeExport
edgeToExport ei = EdgeExport
  { eeFrom = ei.eiFrom
  , eeTo = ei.eiTo
  , eePayload = simplifyTypeName <$> ei.eiPayload
  , eeKind = edgeKindToText ei.eiKind
  }

-- | Convert SchemaInfo to SchemaExport.
schemaToExport :: SchemaInfo -> SchemaExport
schemaToExport si = SchemaExport
  { seTypeName = si.siTypeName
  , seFields = si.siFields
  , seJsonSchema = schemaToValue si.siJsonSchema
  }

-- | Convert TemplateInfo to TemplateExport.
templateToExport :: TemplateInfo -> TemplateExport
templateToExport ti = TemplateExport
  { teTypeName = ti.tiTypeName
  , tePath = ti.tiPath
  , teDeps = ti.tiDeps
  , teAccessedFields = map T.pack ti.tiAccessedFields
  , teContextType = ti.tiContextType
  }

-- | Convert MemoryInfo to MemoryExport.
memoryToExport :: MemoryInfo -> MemoryExport
memoryToExport mi = MemoryExport
  { meTypeName = mi.miTypeName
  }

-- | Convert ToolInfo to ToolExport.
toolToExport :: ToolInfo -> ToolExport
toolToExport ti = ToolExport
  { txName = ti.tiName
  , txDescription = ti.tiDescription
  , txInputSchema = schemaToValue ti.tiInputSchema
  }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert RuntimeNodeKind to text.
kindToText :: RuntimeNodeKind -> Text
kindToText RuntimeLLM = "LLM"
kindToText RuntimeLogic = "Logic"

-- | Convert RuntimeEdgeKind to text.
edgeKindToText :: RuntimeEdgeKind -> Text
edgeKindToText RuntimeImplicit = "implicit"
edgeKindToText RuntimeExplicit = "explicit"

-- | Simplify a TypeRep to just the type name.
simplifyTypeName :: TypeRep -> Text
simplifyTypeName = T.pack . show
