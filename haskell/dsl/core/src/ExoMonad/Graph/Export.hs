-- | JSON export for graph introspection data.
--
-- Converts GraphInfo/NodeInfo to Aeson-serializable types for:
-- - Tooling and LLM context
-- - D3 visualization
-- - API responses
module ExoMonad.Graph.Export
  ( -- * Export Types
    GraphExport (..),
    NodeExport (..),
    EdgeExport (..),
    SchemaExport (..),
    TemplateExport (..),
    MemoryExport (..),
    ClaudeCodeExport (..),
    GeminiExport (..),
    ToolExport (..),

    -- * Conversion
    graphToExport,
    graphToJSON,
  )
where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Graph.Reify
  ( ClaudeCodeInfo (..),
    EdgeInfo (..),
    GeminiInfo (..),
    GraphInfo (..),
    MemoryInfo (..),
    NodeInfo (..),
    RuntimeEdgeKind (..),
    RuntimeNodeKind (..),
    SchemaInfo (..),
    TemplateInfo (..),
    ToolInfo (..),
    simplifyTypeName,
  )
import ExoMonad.Schema (schemaToValue)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- EXPORT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Export format for a complete graph.
data GraphExport = GraphExport
  { -- | EntryNode type name
    geEntryType :: Maybe Text,
    -- | Exit type name
    geExitType :: Maybe Text,
    -- | Nodes by name
    geNodes :: Map Text NodeExport,
    -- | All edges
    geEdges :: [EdgeExport]
  }
  deriving (Show, Eq, Generic)

instance ToJSON GraphExport where
  toJSON ge =
    object
      [ "entryType" .= ge.geEntryType,
        "exitType" .= ge.geExitType,
        "nodes" .= ge.geNodes,
        "edges" .= ge.geEdges
      ]

-- | Export format for a single node.
data NodeExport = NodeExport
  { -- | "LLM" | "ClaudeCode" | "Logic"
    neKind :: Text,
    -- | Input type names
    neNeeds :: [Text],
    -- | Structured output info
    neSchema :: Maybe SchemaExport,
    -- | User template info
    neTemplate :: Maybe TemplateExport,
    -- | System template info
    neSystem :: Maybe TemplateExport,
    -- | Available tools
    neTools :: [ToolExport],
    -- | Node-private state info
    neMemory :: Maybe MemoryExport,
    -- | ClaudeCode annotation info
    neClaudeCode :: Maybe ClaudeCodeExport,
    -- | Gemini annotation info
    neGemini :: Maybe GeminiExport,
    -- | Explicit Goto target names
    neTransitions :: [Text],
    -- | Has Goto Exit?
    neCanExit :: Bool,
    -- | Vision enabled?
    neHasVision :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON NodeExport where
  toJSON ne =
    object
      [ "kind" .= ne.neKind,
        "needs" .= ne.neNeeds,
        "schema" .= ne.neSchema,
        "template" .= ne.neTemplate,
        "system" .= ne.neSystem,
        "tools" .= ne.neTools,
        "memory" .= ne.neMemory,
        "claudeCode" .= ne.neClaudeCode,
        "transitions" .= ne.neTransitions,
        "canExit" .= ne.neCanExit,
        "hasVision" .= ne.neHasVision
      ]

-- | Export format for an edge.
data EdgeExport = EdgeExport
  { -- | Source node name
    eeFrom :: Text,
    -- | Target node name
    eeTo :: Text,
    -- | Payload type name
    eePayload :: Maybe Text,
    -- | "implicit" | "explicit"
    eeKind :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON EdgeExport where
  toJSON ee =
    object
      [ "from" .= ee.eeFrom,
        "to" .= ee.eeTo,
        "payload" .= ee.eePayload,
        "kind" .= ee.eeKind
      ]

-- | Export format for schema info.
data SchemaExport = SchemaExport
  { -- | Type name (e.g., "Intent")
    seTypeName :: Text,
    -- | (name, type, required)
    seFields :: [(Text, Text, Bool)],
    -- | Full JSON schema
    seJsonSchema :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON SchemaExport where
  toJSON se =
    object
      [ "typeName" .= se.seTypeName,
        "fields" .= map fieldToJSON se.seFields,
        "jsonSchema" .= se.seJsonSchema
      ]
    where
      fieldToJSON (name, ty, req) =
        object
          [ "name" .= name,
            "type" .= ty,
            "required" .= req
          ]

-- | Export format for template info.
data TemplateExport = TemplateExport
  { -- | Template type name
    teTypeName :: Text,
    -- | Template file path
    tePath :: FilePath,
    -- | Template dependencies
    teDeps :: [FilePath],
    -- | Fields accessed in template
    teAccessedFields :: [Text],
    -- | Context type name
    teContextType :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TemplateExport where
  toJSON te =
    object
      [ "typeName" .= te.teTypeName,
        "path" .= te.tePath,
        "deps" .= te.teDeps,
        "accessedFields" .= te.teAccessedFields,
        "contextType" .= te.teContextType
      ]

-- | Export format for memory info.
data MemoryExport = MemoryExport
  { -- | Memory type name
    meTypeName :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON MemoryExport where
  toJSON me =
    object
      [ "typeName" .= me.meTypeName
      ]

-- | Export format for ClaudeCode info.
data ClaudeCodeExport = ClaudeCodeExport
  { -- | Model: "Haiku", "Sonnet", or "Opus"
    ceModel :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ClaudeCodeExport where
  toJSON ce =
    object
      [ "model" .= ce.ceModel
      ]

-- | Export format for Gemini info.
data GeminiExport = GeminiExport
  { -- | Model: "Flash", "Pro", or "Ultra"
    geModel :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON GeminiExport where
  toJSON ge =
    object
      [ "model" .= ge.geModel
      ]

-- | Export format for tool info.
data ToolExport = ToolExport
  { -- | Tool name
    txName :: Text,
    -- | Tool description
    txDescription :: Text,
    -- | Input JSON schema
    txInputSchema :: Value
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolExport where
  toJSON tx =
    object
      [ "name" .= tx.txName,
        "description" .= tx.txDescription,
        "inputSchema" .= tx.txInputSchema
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert GraphInfo to GraphExport.
graphToExport :: GraphInfo -> GraphExport
graphToExport gi =
  GraphExport
    { geEntryType = simplifyTypeName <$> gi.giEntryType,
      geExitType = simplifyTypeName <$> gi.giExitType,
      geNodes =
        Map.fromList
          [ (ni.niName, nodeToExport ni)
          | ni <- gi.giNodes
          ],
      geEdges = map edgeToExport gi.giEdges
    }

-- | Convert GraphInfo to JSON Value directly.
graphToJSON :: GraphInfo -> Value
graphToJSON = toJSON . graphToExport

-- | Convert NodeInfo to NodeExport.
nodeToExport :: NodeInfo -> NodeExport
nodeToExport ni =
  NodeExport
    { neKind = kindToText ni.niKind,
      neNeeds = maybe [] (\t -> [simplifyTypeName t]) ni.niInput,
      neSchema = schemaToExport <$> ni.niSchema,
      neTemplate = templateToExport <$> ni.niTemplate,
      neSystem = templateToExport <$> ni.niSystem,
      neTools = map toolToExport ni.niToolInfos,
      neMemory = memoryToExport <$> ni.niMemory,
      neClaudeCode = claudeCodeToExport <$> ni.niClaudeCode,
      neGemini = geminiToExport <$> ni.niGemini,
      neTransitions = map fst ni.niGotoTargets,
      neCanExit = ni.niHasGotoExit,
      neHasVision = ni.niHasVision
    }

-- | Convert EdgeInfo to EdgeExport.
edgeToExport :: EdgeInfo -> EdgeExport
edgeToExport ei =
  EdgeExport
    { eeFrom = ei.eiFrom,
      eeTo = ei.eiTo,
      eePayload = simplifyTypeName <$> ei.eiPayload,
      eeKind = edgeKindToText ei.eiKind
    }

-- | Convert SchemaInfo to SchemaExport.
schemaToExport :: SchemaInfo -> SchemaExport
schemaToExport si =
  SchemaExport
    { seTypeName = si.siTypeName,
      seFields = si.siFields,
      seJsonSchema = schemaToValue si.siJsonSchema
    }

-- | Convert TemplateInfo to TemplateExport.
templateToExport :: TemplateInfo -> TemplateExport
templateToExport ti =
  TemplateExport
    { teTypeName = ti.tiTypeName,
      tePath = ti.tiPath,
      teDeps = ti.tiDeps,
      teAccessedFields = map T.pack ti.tiAccessedFields,
      teContextType = ti.tiContextType
    }

-- | Convert MemoryInfo to MemoryExport.
memoryToExport :: MemoryInfo -> MemoryExport
memoryToExport mi =
  MemoryExport
    { meTypeName = mi.miTypeName
    }

-- | Convert ToolInfo to ToolExport.
toolToExport :: ToolInfo -> ToolExport
toolToExport ti =
  ToolExport
    { txName = ti.tiName,
      txDescription = ti.tiDescription,
      txInputSchema = schemaToValue ti.tiInputSchema
    }

-- | Convert ClaudeCodeInfo to ClaudeCodeExport.
claudeCodeToExport :: ClaudeCodeInfo -> ClaudeCodeExport
claudeCodeToExport cci =
  ClaudeCodeExport
    { ceModel = cci.cciModel
    }

-- | Convert GeminiInfo to GeminiExport.
geminiToExport :: GeminiInfo -> GeminiExport
geminiToExport gi =
  GeminiExport
    { geModel = gi.giModel
    }

-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert RuntimeNodeKind to text.
kindToText :: RuntimeNodeKind -> Text
kindToText RuntimeLLM = "LLM"
kindToText RuntimeClaudeCode = "ClaudeCode"
kindToText RuntimeGemini = "Gemini"
kindToText RuntimeLogic = "Logic"
kindToText RuntimeFork = "Fork"
kindToText RuntimeBarrier = "Barrier"

-- | Convert RuntimeEdgeKind to text.
edgeKindToText :: RuntimeEdgeKind -> Text
edgeKindToText RuntimeImplicit = "implicit"
edgeKindToText RuntimeExplicit = "explicit"
