
-- | Runtime reification of type-level graph information.
--
-- This module provides typeclasses that convert compile-time type information
-- into runtime data structures. This enables:
--
-- * Mermaid diagram generation
-- * Runtime introspection for debugging
-- * Dynamic graph traversal
--
-- = Reification Pattern
--
-- The reification uses a Servant-style pattern with explicit 'Proxy' passing
-- to avoid 'AllowAmbiguousTypes' issues:
--
-- @
-- class ReifyTypeList (ts :: [Type]) where
--   reifyTypeList :: Proxy ts -> [TypeRep]
--
-- instance ReifyTypeList '[] where
--   reifyTypeList _ = []
--
-- instance (Typeable t, ReifyTypeList ts) => ReifyTypeList (t ': ts) where
--   reifyTypeList _ = typeRep (Proxy \@t) : reifyTypeList (Proxy \@ts)
-- @
module Tidepool.Graph.Reify
  ( -- * Graph Info Types
    GraphInfo(..)
  , NodeInfo(..)
  , EdgeInfo(..)
  , RuntimeNodeKind(..)
  , RuntimeEdgeKind(..)
  , ToolInfo(..)

    -- * Rich Info Types
  , SchemaInfo(..)
  , TemplateInfo(..)
  , MemoryInfo(..)
  , ClaudeCodeInfo(..)

    -- * Reification Typeclasses
  , ReifyGraph(..)

    -- * Record-Based Graph Reification
  , ReifyRecordGraph(..)
  , GReifyFields(..)
  , ReifyNodeDef(..)
  , makeGraphInfo

    -- * Entry/Exit Type Extraction
  , GetEntryTypeFromGraph
  , GetExitTypeFromGraph

    -- * Helper Typeclasses
  , ReifyTypeList(..)
  , ReifyMaybeType(..)
  , ReifyGotoTargets(..)
  , ReifyNodeKind(..)
  , ReifyBool(..)
  , ReifySchemaInfo(..)
  , ReifyTemplateInfo(..)
  , ReifyMemoryInfo(..)
  , ReifyClaudeCodeInfo(..)

    -- * Goto Target Extraction
  , GotoTargetsFromDef
  , GotoTargetsFromEffects
  , HasGotoExitInDef
  , HasGotoExitFromEffects

    -- * Utilities
  , simplifyTypeName

    -- * Node Type Predicates
  , IsForkNode
  , IsBarrierNode
  ) where

import Data.Char (isAsciiUpper)
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics (Generic(..), K1(..), M1(..), (:*:)(..), Meta(..), S, D, C)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Data.Map.Strict as Map

import Tidepool.Graph.Types (NodeKind(..), type (:@), ModelChoice(..))
import Tidepool.Graph.Tool (ToolInfo(..))
import Tidepool.Graph.Generic.Core (Entry, Exit, LLMNode, LogicNode, ForkNode, BarrierNode, AsGraph)
import Tidepool.Graph.Edges
  ( GetInput, GetSchema, GetTemplate, GetSystem
  , GetVision, GetTools, GetMemory, GetUsesEffects
  , GetGotoTargets, HasGotoExit
  , GetClaudeCode
  )
import Tidepool.Schema (JSONSchema(..), HasJSONSchema(..), SchemaType(..))
import Tidepool.Graph.Template
  ( TemplateDef(..)
  , TemplateDependency(..)
  , TemplateContextInfo(..)
  , templateDependencyTree
  , flattenDeps
  , templateContextInfo
  , templateAccessedFields
  )

-- | Effect type alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type

-- | Helper type family to get Entry type from a graph.
--
-- This is a simplified version that works directly on graph records.
type GetEntryTypeFromGraph :: (Type -> Type) -> Maybe Type
type family GetEntryTypeFromGraph graph where
  GetEntryTypeFromGraph graph = GetEntryTypeRep (Rep (graph AsGraph))

-- | Extract Entry type from Generic representation.
type GetEntryTypeRep :: (Type -> Type) -> Maybe Type
type family GetEntryTypeRep f where
  GetEntryTypeRep (M1 D _ f) = GetEntryTypeRep f
  GetEntryTypeRep (M1 C _ f) = GetEntryTypeRep f
  GetEntryTypeRep (M1 S _ (K1 _ (Entry a))) = 'Just a
  GetEntryTypeRep (M1 S _ _) = 'Nothing
  GetEntryTypeRep (l :*: r) = OrMaybe (GetEntryTypeRep l) (GetEntryTypeRep r)
  GetEntryTypeRep _ = 'Nothing

-- | Extract Exit type from a graph.
type GetExitTypeFromGraph :: (Type -> Type) -> Maybe Type
type family GetExitTypeFromGraph graph where
  GetExitTypeFromGraph graph = GetExitTypeRep (Rep (graph AsGraph))

-- | Extract Exit type from Generic representation.
type GetExitTypeRep :: (Type -> Type) -> Maybe Type
type family GetExitTypeRep f where
  GetExitTypeRep (M1 D _ f) = GetExitTypeRep f
  GetExitTypeRep (M1 C _ f) = GetExitTypeRep f
  GetExitTypeRep (M1 S _ (K1 _ (Exit a))) = 'Just a
  GetExitTypeRep (M1 S _ _) = 'Nothing
  GetExitTypeRep (l :*: r) = OrMaybe (GetExitTypeRep l) (GetExitTypeRep r)
  GetExitTypeRep _ = 'Nothing

-- | Return first Just, or Nothing if both Nothing.
type OrMaybe :: Maybe k -> Maybe k -> Maybe k
type family OrMaybe a b where
  OrMaybe ('Just x) _ = 'Just x
  OrMaybe 'Nothing b = b

-- ════════════════════════════════════════════════════════════════════════════
-- RUNTIME INFO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Complete runtime representation of a graph.
data GraphInfo = GraphInfo
  { giEntryType :: Maybe TypeRep   -- ^ Type accepted at Entry
  , giExitType :: Maybe TypeRep    -- ^ Type produced at Exit
  , giNodes :: [NodeInfo]          -- ^ All node declarations
  , giEdges :: [EdgeInfo]          -- ^ Derived edges
  , giGroups :: [(Text, [Text])]   -- ^ Groups annotation (for Mermaid subgraphs)
  }
  deriving (Show, Eq)

-- | Runtime representation of a node.
--
-- Contains rich information about each node including template paths,
-- schema details, and tool information. This serves as the single source
-- of truth for Mermaid diagrams, JSON export, and D3 visualization.
data NodeInfo = NodeInfo
  { niName :: Text                      -- ^ Node name (from Symbol)
  , niKind :: RuntimeNodeKind           -- ^ LLM, ClaudeCode, or Logic
  , niInput :: Maybe TypeRep            -- ^ Input type this node needs
  , niSchema :: Maybe SchemaInfo        -- ^ Rich schema info (LLM nodes)
  , niGotoTargets :: [(Text, TypeRep)]  -- ^ Goto targets (Logic nodes)
  , niHasGotoExit :: Bool               -- ^ Can this node exit the graph?
  , niHasVision :: Bool                 -- ^ Has Vision annotation?
  , niTools :: [TypeRep]                -- ^ Tool types (for backwards compat)
  , niToolInfos :: [ToolInfo]           -- ^ Full tool info with schemas (V2)
  , niSystem :: Maybe TemplateInfo      -- ^ System prompt template (rich info)
  , niTemplate :: Maybe TemplateInfo    -- ^ User prompt template (rich info)
  , niMemory :: Maybe MemoryInfo        -- ^ Memory type (rich info)
  , niClaudeCode :: Maybe ClaudeCodeInfo -- ^ ClaudeCode annotation (rich info)
  }
  deriving (Show, Eq)

-- | Runtime representation of an edge.
data EdgeInfo = EdgeInfo
  { eiFrom :: Text               -- ^ Source node (or "Entry")
  , eiTo :: Text                 -- ^ Target node (or "Exit")
  , eiPayload :: Maybe TypeRep   -- ^ Type carried on edge
  , eiKind :: RuntimeEdgeKind    -- ^ Edge classification
  }
  deriving (Show, Eq)

-- | Runtime node kind.
data RuntimeNodeKind
  = RuntimeLLM         -- ^ Standard LLM API call
  | RuntimeClaudeCode  -- ^ Claude Code subprocess
  | RuntimeLogic       -- ^ Pure routing logic
  | RuntimeFork        -- ^ Fork node - spawns parallel workers
  | RuntimeBarrier     -- ^ Barrier node - collects parallel results
  deriving (Show, Eq)

-- | Runtime edge kind.
data RuntimeEdgeKind
  = RuntimeImplicit    -- ^ Schema → Needs (data flow)
  | RuntimeExplicit    -- ^ Goto (transition)
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- RICH INFO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Rich schema information including JSON schema and field details.
--
-- Captures everything an LLM or visualization tool needs to understand
-- the output type of an LLM node.
data SchemaInfo = SchemaInfo
  { siType :: TypeRep                   -- ^ The Haskell type
  , siTypeName :: Text                  -- ^ Simplified type name (e.g., "Intent")
  , siJsonSchema :: JSONSchema          -- ^ Full JSON schema with descriptions
  , siFields :: [(Text, Text, Bool)]    -- ^ (fieldName, fieldType, isRequired)
  }
  deriving (Show, Eq)

-- | Rich template information including file path and dependencies.
--
-- Captures everything needed to understand which template file is wired
-- to a node and what context it expects.
data TemplateInfo = TemplateInfo
  { tiType :: TypeRep                   -- ^ The template type (e.g., ClassifyTpl)
  , tiTypeName :: Text                  -- ^ Simplified type name
  , tiPath :: FilePath                  -- ^ Template file path (e.g., "templates/classify.jinja")
  , tiDeps :: [FilePath]                -- ^ Transitive template dependencies
  , tiAccessedFields :: [String]        -- ^ Fields accessed in template (e.g., ["topic", "history"])
  , tiContextType :: Text               -- ^ Context type name (e.g., "ClassifyContext")
  }
  deriving (Show, Eq)

-- | Rich memory information for node-private persistent state.
data MemoryInfo = MemoryInfo
  { miType :: TypeRep                   -- ^ The memory type
  , miTypeName :: Text                  -- ^ Simplified type name
  }
  deriving (Show, Eq)

-- | Rich ClaudeCode information for nodes executed via Claude Code subprocess.
data ClaudeCodeInfo = ClaudeCodeInfo
  { cciModel :: Text                    -- ^ Model choice: "Haiku", "Sonnet", or "Opus"
  , cciCwd :: Maybe Text                -- ^ Working directory if specified
  }
  deriving (Show, Eq)

-- | Extract fields from a JSONSchema.
--
-- Returns (fieldName, fieldType, isRequired) for each property.
extractSchemaFields :: JSONSchema -> [(Text, Text, Bool)]
extractSchemaFields schema =
  [ (name, schemaTypeToText fieldSchema.schemaType, name `elem` schema.schemaRequired)
  | (name, fieldSchema) <- Map.toList schema.schemaProperties
  ]
  where
    schemaTypeToText TString = "String"
    schemaTypeToText TNumber = "Number"
    schemaTypeToText TInteger = "Integer"
    schemaTypeToText TBoolean = "Boolean"
    schemaTypeToText TObject = "Object"
    schemaTypeToText TArray = "Array"
    schemaTypeToText TNull = "Null"

-- | Simplify a TypeRep to just the type name (strip module prefix).
simplifyTypeName :: TypeRep -> Text
simplifyTypeName tr = T.pack $ go $ show tr
  where
    go s = case break (== '.') s of
      (_, []) -> s
      (_, '.':rest) ->
        case rest of
          (c:_) | isAsciiUpper c -> go rest
          _ -> s
      _ -> s

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for reifying a complete graph to runtime GraphInfo.
--
-- Instances should be defined for record-based graphs using GHC.Generics.
class ReifyGraph (g :: Type) where
  reifyGraph :: GraphInfo

-- ════════════════════════════════════════════════════════════════════════════
-- HELPER TYPECLASSES
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify a type-level list of Types to runtime [TypeRep].
--
-- Uses explicit Proxy passing to avoid ambiguous type issues.
type ReifyTypeList :: [Type] -> Constraint
class ReifyTypeList (ts :: [Type]) where
  reifyTypeList :: Proxy ts -> [TypeRep]

instance ReifyTypeList '[] where
  reifyTypeList _ = []

instance (Typeable t, ReifyTypeList ts) => ReifyTypeList (t ': ts) where
  reifyTypeList _ = typeRep (Proxy @t) : reifyTypeList (Proxy @ts)

-- | Reify a type-level Maybe Type to runtime Maybe TypeRep.
type ReifyMaybeType :: Maybe Type -> Constraint
class ReifyMaybeType (mt :: Maybe Type) where
  reifyMaybeType :: Proxy mt -> Maybe TypeRep

instance ReifyMaybeType 'Nothing where
  reifyMaybeType _ = Nothing

instance Typeable t => ReifyMaybeType ('Just t) where
  reifyMaybeType _ = Just (typeRep (Proxy @t))

-- | Reify a type-level list of (Symbol, Type) pairs to runtime [(Text, TypeRep)].
--
-- Used for Goto targets extraction.
type ReifyGotoTargets :: [(Symbol, Type)] -> Constraint
class ReifyGotoTargets (ts :: [(Symbol, Type)]) where
  reifyGotoTargets :: Proxy ts -> [(Text, TypeRep)]

instance ReifyGotoTargets '[] where
  reifyGotoTargets _ = []

instance (KnownSymbol name, Typeable payload, ReifyGotoTargets rest)
      => ReifyGotoTargets ('(name, payload) ': rest) where
  reifyGotoTargets _ =
    (T.pack (symbolVal (Proxy @name)), typeRep (Proxy @payload))
    : reifyGotoTargets (Proxy @rest)

-- | Reify NodeKind to RuntimeNodeKind.
type ReifyNodeKind :: NodeKind -> Constraint
class ReifyNodeKind (k :: NodeKind) where
  reifyNodeKind :: Proxy k -> RuntimeNodeKind

instance ReifyNodeKind 'LLM where
  reifyNodeKind _ = RuntimeLLM

instance ReifyNodeKind 'Logic where
  reifyNodeKind _ = RuntimeLogic

-- | Reify type-level Bool to runtime Bool.
type ReifyBool :: Bool -> Constraint
class ReifyBool (b :: Bool) where
  reifyBool :: Proxy b -> Bool

instance ReifyBool 'True where
  reifyBool _ = True

instance ReifyBool 'False where
  reifyBool _ = False

-- ════════════════════════════════════════════════════════════════════════════
-- RICH INFO REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify a type-level Maybe schema type to runtime Maybe SchemaInfo.
--
-- Extracts JSON schema and field information from types with HasJSONSchema.
type ReifySchemaInfo :: Maybe Type -> Constraint
class ReifySchemaInfo (mt :: Maybe Type) where
  reifySchemaInfo :: Proxy mt -> Maybe SchemaInfo

instance ReifySchemaInfo 'Nothing where
  reifySchemaInfo _ = Nothing

instance (Typeable t, HasJSONSchema t) => ReifySchemaInfo ('Just t) where
  reifySchemaInfo _ = Just SchemaInfo
    { siType = typeRep (Proxy @t)
    , siTypeName = simplifyTypeName (typeRep (Proxy @t))
    , siJsonSchema = jsonSchema @t
    , siFields = extractSchemaFields (jsonSchema @t)
    }

-- | Reify a type-level Maybe template type to runtime Maybe TemplateInfo.
--
-- Extracts template path, dependencies, and accessed fields from TemplateDef.
type ReifyTemplateInfo :: Maybe Type -> Constraint
class ReifyTemplateInfo (mt :: Maybe Type) where
  reifyTemplateInfo :: Proxy mt -> Maybe TemplateInfo

instance ReifyTemplateInfo 'Nothing where
  reifyTemplateInfo _ = Nothing

instance (Typeable t, TemplateDef t) => ReifyTemplateInfo ('Just t) where
  reifyTemplateInfo _ = Just TemplateInfo
    { tiType = typeRep (Proxy @t)
    , tiTypeName = simplifyTypeName (typeRep (Proxy @t))
    , tiPath = depRelativePath depTree
    , tiDeps = map depRelativePath $ drop 1 $ flattenDeps depTree  -- Skip root
    , tiAccessedFields = templateAccessedFields compiled
    , tiContextType = T.pack $ tciTypeName $ templateContextInfo compiled
    }
    where
      compiled = templateCompiled @t
      depTree = templateDependencyTree compiled

-- | Reify a type-level Maybe memory type to runtime Maybe MemoryInfo.
type ReifyMemoryInfo :: Maybe Type -> Constraint
class ReifyMemoryInfo (mt :: Maybe Type) where
  reifyMemoryInfo :: Proxy mt -> Maybe MemoryInfo

instance ReifyMemoryInfo 'Nothing where
  reifyMemoryInfo _ = Nothing

instance Typeable t => ReifyMemoryInfo ('Just t) where
  reifyMemoryInfo _ = Just MemoryInfo
    { miType = typeRep (Proxy @t)
    , miTypeName = simplifyTypeName (typeRep (Proxy @t))
    }

-- | Reify a type-level Maybe ClaudeCode annotation to runtime Maybe ClaudeCodeInfo.
--
-- The type-level representation is @Maybe (ModelChoice, Maybe Symbol)@ where:
-- * @ModelChoice@ is 'Haiku, 'Sonnet, or 'Opus
-- * @Maybe Symbol@ is the optional working directory path
type ReifyClaudeCodeInfo :: Maybe (ModelChoice, Maybe Symbol) -> Constraint
class ReifyClaudeCodeInfo (mcc :: Maybe (ModelChoice, Maybe Symbol)) where
  reifyClaudeCodeInfo :: Proxy mcc -> Maybe ClaudeCodeInfo

instance ReifyClaudeCodeInfo 'Nothing where
  reifyClaudeCodeInfo _ = Nothing

instance KnownSymbol cwd => ReifyClaudeCodeInfo ('Just '( 'Haiku, 'Just cwd)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Haiku"
    , cciCwd = Just $ T.pack (symbolVal (Proxy @cwd))
    }

instance ReifyClaudeCodeInfo ('Just '( 'Haiku, 'Nothing)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Haiku"
    , cciCwd = Nothing
    }

instance KnownSymbol cwd => ReifyClaudeCodeInfo ('Just '( 'Sonnet, 'Just cwd)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Sonnet"
    , cciCwd = Just $ T.pack (symbolVal (Proxy @cwd))
    }

instance ReifyClaudeCodeInfo ('Just '( 'Sonnet, 'Nothing)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Sonnet"
    , cciCwd = Nothing
    }

instance KnownSymbol cwd => ReifyClaudeCodeInfo ('Just '( 'Opus, 'Just cwd)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Opus"
    , cciCwd = Just $ T.pack (symbolVal (Proxy @cwd))
    }

instance ReifyClaudeCodeInfo ('Just '( 'Opus, 'Nothing)) where
  reifyClaudeCodeInfo _ = Just ClaudeCodeInfo
    { cciModel = "Opus"
    , cciCwd = Nothing
    }

-- ════════════════════════════════════════════════════════════════════════════
-- GOTO TARGET REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Extract Goto targets from a node definition's UsesEffects annotation.
--
-- This uses the same pattern as validation code in Generic.hs:
-- explicitly apply @Effect kind to resolve polykind ambiguity.
--
-- @
-- GotoTargetsFromDef (LogicNode :@ UsesEffects '[Goto "a" A, Goto "b" B])
--   = '[ '("a", A), '("b", B) ]
-- @
type GotoTargetsFromDef :: Type -> [(Symbol, Type)]
type family GotoTargetsFromDef def where
  GotoTargetsFromDef def = GotoTargetsFromEffects (GetUsesEffects @Effect def)

-- | Extract Goto targets from Maybe effect list.
type GotoTargetsFromEffects :: Maybe [Effect] -> [(Symbol, Type)]
type family GotoTargetsFromEffects mEffs where
  GotoTargetsFromEffects 'Nothing = '[]
  GotoTargetsFromEffects ('Just effs) = GetGotoTargets @Effect effs

-- | Check if a node definition has Goto Exit in its effect stack.
type HasGotoExitInDef :: Type -> Bool
type family HasGotoExitInDef def where
  HasGotoExitInDef def = HasGotoExitFromEffects (GetUsesEffects @Effect def)

-- | Extract HasGotoExit from Maybe effect list.
type HasGotoExitFromEffects :: Maybe [Effect] -> Bool
type family HasGotoExitFromEffects mEffs where
  HasGotoExitFromEffects 'Nothing = 'False
  HasGotoExitFromEffects ('Just effs) = HasGotoExit @Effect effs

-- ════════════════════════════════════════════════════════════════════════════
-- RECORD-BASED GRAPH REIFICATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Reify a record-based graph to runtime GraphInfo.
--
-- This typeclass enables automatic reification for graphs defined using
-- the record-based DSL (see "Tidepool.Graph.Generic").
--
-- @
-- data SupportGraph mode = SupportGraph { ... } deriving Generic
--
-- graphInfo :: GraphInfo
-- graphInfo = reifyRecordGraph (Proxy @SupportGraph)
-- @
--
-- == Goto Target Extraction
--
-- Goto targets are extracted from Logic nodes by using 'GotoTargetsFromDef',
-- which applies @Effect@ kind explicitly to resolve polykind ambiguity.
-- This enables full Mermaid diagram generation including explicit Goto edges.
class ReifyRecordGraph (graph :: Type -> Type) where
  reifyRecordGraph :: Proxy graph -> GraphInfo

-- | Helper to construct GraphInfo from a record-based graph.
--
-- Use this when defining @ReifyRecordGraph@ instances:
--
-- @
-- instance ReifyRecordGraph SupportGraph where
--   reifyRecordGraph = makeGraphInfo
-- @
--
-- Requires:
--
-- * @Generic (graph AsGraph)@ - for field traversal
-- * @GReifyFields (Rep (graph AsGraph))@ - for node extraction
-- * @ReifyMaybeType (GetEntryTypeFromGraph graph)@ - for entry type
-- * @ReifyMaybeType (GetExitTypeFromGraph graph)@ - for exit type
makeGraphInfo
  :: forall graph.
     ( GReifyFields (Rep (graph AsGraph))
     , ReifyMaybeType (GetEntryTypeFromGraph graph)
     , ReifyMaybeType (GetExitTypeFromGraph graph)
     )
  => Proxy graph
  -> GraphInfo
makeGraphInfo _ = GraphInfo
  { giEntryType = entryType
  , giExitType = exitType
  , giNodes = nodes
  , giEdges = deriveImplicitEdges entryType nodes
          ++ deriveExitEdges exitType nodes
          ++ deriveGotoEdges nodes
          ++ deriveGotoExitEdges nodes
  , giGroups = []  -- Would need graph-level annotation extraction
  }
  where
    entryType = reifyMaybeType (Proxy @(GetEntryTypeFromGraph graph))
    exitType = reifyMaybeType (Proxy @(GetExitTypeFromGraph graph))
    nodes = gReifyFields (Proxy @(Rep (graph AsGraph)))

-- | Generic traversal for reifying record fields to NodeInfo list.
--
-- Handles GHC.Generics representation types:
--
-- * @M1 D@ - datatype metadata, pass through
-- * @M1 C@ - constructor metadata, pass through
-- * @M1 S ('MetaSel ('Just name) ...)@ - named field, extract name and def
-- * @l :*: r@ - product, combine left and right
-- * @K1@ - field value (handled via M1 S wrapper)
class GReifyFields (f :: Type -> Type) where
  gReifyFields :: Proxy f -> [NodeInfo]

-- Datatype metadata: pass through
instance GReifyFields f => GReifyFields (M1 D meta f) where
  gReifyFields _ = gReifyFields (Proxy @f)

-- Constructor metadata: pass through
instance GReifyFields f => GReifyFields (M1 C meta f) where
  gReifyFields _ = gReifyFields (Proxy @f)

-- Product: combine left and right
instance (GReifyFields l, GReifyFields r) => GReifyFields (l :*: r) where
  gReifyFields _ = gReifyFields (Proxy @l) ++ gReifyFields (Proxy @r)

-- Named field: extract name and delegate to ReifyNodeDef
instance (KnownSymbol name, ReifyNodeDef def)
      => GReifyFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i def)) where
  gReifyFields _ = reifyNodeDef (Proxy @name) (Proxy @def)

-- Unnamed field or bare K1: no nodes
instance GReifyFields (M1 S ('MetaSel 'Nothing su ss ds) f) where
  gReifyFields _ = []

instance GReifyFields (K1 i c) where
  gReifyFields _ = []

-- | Reify a single node definition to Maybe NodeInfo.
--
-- Returns @[]@ for Entry/Exit (not real nodes), @[NodeInfo]@ for LLMNode/LogicNode.
class ReifyNodeDef (def :: Type) where
  reifyNodeDef :: KnownSymbol name => Proxy name -> Proxy def -> [NodeInfo]

-- Entry and Exit are not nodes
instance ReifyNodeDef (Entry a) where
  reifyNodeDef _ _ = []

instance ReifyNodeDef (Exit a) where
  reifyNodeDef _ _ = []

-- | Get the base node type from a potentially annotated node definition.
--
-- @
-- GetBaseNode (LLMNode :@ A :@ B :@ C) = LLMNode
-- GetBaseNode LogicNode = LogicNode
-- @
type GetBaseNode :: Type -> Type
type family GetBaseNode def where
  GetBaseNode (node :@ _) = GetBaseNode node
  GetBaseNode node = node

-- | Check if the base node is LLMNode.
type IsLLMNode :: Type -> Bool
type family IsLLMNode def where
  IsLLMNode LLMNode = 'True
  IsLLMNode (node :@ _) = IsLLMNode node
  IsLLMNode _ = 'False

-- | Check if the base node is LogicNode.
type IsLogicNode :: Type -> Bool
type family IsLogicNode def where
  IsLogicNode LogicNode = 'True
  IsLogicNode (node :@ _) = IsLogicNode node
  IsLogicNode _ = 'False

-- | Check if the base node is ForkNode.
type IsForkNode :: Type -> Bool
type family IsForkNode def where
  IsForkNode ForkNode = 'True
  IsForkNode (node :@ _) = IsForkNode node
  IsForkNode _ = 'False

-- | Check if the base node is BarrierNode.
type IsBarrierNode :: Type -> Bool
type family IsBarrierNode def where
  IsBarrierNode BarrierNode = 'True
  IsBarrierNode (node :@ _) = IsBarrierNode node
  IsBarrierNode _ = 'False

-- ForkNode instance - spawns parallel workers
--
-- Basic support: extracts input, produces RuntimeFork kind.
-- TODO: Could extract Spawn targets, Barrier name for richer visualization.
instance {-# OVERLAPPING #-}
         ( ReifyMaybeType (GetInput (ForkNode :@ ann))
         ) => ReifyNodeDef (ForkNode :@ ann) where
  reifyNodeDef pName _ = [NodeInfo
    { niName = T.pack (symbolVal pName)
    , niKind = RuntimeFork
    , niInput = reifyMaybeType (Proxy @(GetInput (ForkNode :@ ann)))
    , niSchema = Nothing
    , niGotoTargets = []  -- ForkNode uses Spawn, not Goto
    , niHasGotoExit = False
    , niHasVision = False
    , niTools = []
    , niToolInfos = []
    , niSystem = Nothing
    , niTemplate = Nothing
    , niMemory = Nothing
    , niClaudeCode = Nothing
    }]

-- BarrierNode instance - collects parallel results
--
-- Basic support: extracts input, produces RuntimeBarrier kind.
-- TODO: Could extract Awaits types for richer visualization.
instance {-# OVERLAPPING #-}
         ( ReifyMaybeType (GetInput (BarrierNode :@ ann))
         , ReifyGotoTargets (GotoTargetsFromDef (BarrierNode :@ ann))
         , ReifyBool (HasGotoExitInDef (BarrierNode :@ ann))
         ) => ReifyNodeDef (BarrierNode :@ ann) where
  reifyNodeDef pName _ = [NodeInfo
    { niName = T.pack (symbolVal pName)
    , niKind = RuntimeBarrier
    , niInput = reifyMaybeType (Proxy @(GetInput (BarrierNode :@ ann)))
    , niSchema = Nothing
    , niGotoTargets = reifyGotoTargets (Proxy @(GotoTargetsFromDef (BarrierNode :@ ann)))
    , niHasGotoExit = reifyBool (Proxy @(HasGotoExitInDef (BarrierNode :@ ann)))
    , niHasVision = False
    , niTools = []
    , niToolInfos = []
    , niSystem = Nothing
    , niTemplate = Nothing
    , niMemory = Nothing
    , niClaudeCode = Nothing
    }]

-- General instance for any annotated node: dispatch based on base type
instance {-# OVERLAPPABLE #-}
         ( ReifyAnnotatedNode (def :@ ann) (IsLLMNode (def :@ ann)) (IsLogicNode (def :@ ann))
         ) => ReifyNodeDef (def :@ ann) where
  reifyNodeDef pName pDef = reifyAnnotatedNode pDef (Proxy @(IsLLMNode (def :@ ann))) (Proxy @(IsLogicNode (def :@ ann))) pName pDef

-- | Helper class to dispatch based on node kind.
class ReifyAnnotatedNode (def :: Type) (isLLM :: Bool) (isLogic :: Bool) where
  reifyAnnotatedNode :: Proxy def -> Proxy isLLM -> Proxy isLogic
                     -> (forall name. KnownSymbol name => Proxy name -> Proxy def -> [NodeInfo])

-- LLMNode case
--
-- Uses rich info typeclasses to extract template paths, schema fields, etc.
-- ClaudeCode annotation is extracted to determine if this is a ClaudeCode node.
instance ( ReifyMaybeType (GetInput def)
         , ReifySchemaInfo (GetSchema def)
         , ReifyTemplateInfo (GetTemplate def)
         , ReifyTemplateInfo (GetSystem def)
         , ReifyMemoryInfo (GetMemory def)
         , ReifyClaudeCodeInfo (GetClaudeCode def)
         , ReifyBool (GetVision def)
         , ReifyTypeList (GetTools def)
         ) => ReifyAnnotatedNode def 'True 'False where
  reifyAnnotatedNode _ _ _ pName _ =
    let claudeCodeInfo = reifyClaudeCodeInfo (Proxy @(GetClaudeCode def))
        nodeKind = case claudeCodeInfo of
          Just _  -> RuntimeClaudeCode
          Nothing -> RuntimeLLM
    in [NodeInfo
      { niName = T.pack (symbolVal pName)
      , niKind = nodeKind
      , niInput = reifyMaybeType (Proxy @(GetInput def))
      , niSchema = reifySchemaInfo (Proxy @(GetSchema def))
      , niGotoTargets = []  -- LLM nodes don't have Goto
      , niHasGotoExit = False
      , niHasVision = reifyBool (Proxy @(GetVision def))
      , niTools = reifyTypeList (Proxy @(GetTools def))
      , niToolInfos = []  -- Would require ToolDef instances
      , niSystem = reifyTemplateInfo (Proxy @(GetSystem def))
      , niTemplate = reifyTemplateInfo (Proxy @(GetTemplate def))
      , niMemory = reifyMemoryInfo (Proxy @(GetMemory def))
      , niClaudeCode = claudeCodeInfo
      }]

-- LogicNode case
--
-- Extracts Goto targets by using GotoTargetsFromDef, which applies
-- @Effect kind explicitly to resolve polykind ambiguity.
instance ( ReifyMaybeType (GetInput def)
         , ReifyMemoryInfo (GetMemory def)
         , ReifyGotoTargets (GotoTargetsFromDef def)
         , ReifyBool (HasGotoExitInDef def)
         ) => ReifyAnnotatedNode def 'False 'True where
  reifyAnnotatedNode _ _ _ pName _ = [NodeInfo
    { niName = T.pack (symbolVal pName)
    , niKind = RuntimeLogic
    , niInput = reifyMaybeType (Proxy @(GetInput def))
    , niSchema = Nothing  -- Logic nodes don't have Schema
    , niGotoTargets = reifyGotoTargets (Proxy @(GotoTargetsFromDef def))
    , niHasGotoExit = reifyBool (Proxy @(HasGotoExitInDef def))
    , niHasVision = False
    , niTools = []
    , niToolInfos = []
    , niSystem = Nothing
    , niTemplate = Nothing
    , niMemory = reifyMemoryInfo (Proxy @(GetMemory def))
    , niClaudeCode = Nothing  -- Logic nodes don't use ClaudeCode
    }]

-- Neither LLM nor Logic - unknown node type, return empty
instance ReifyAnnotatedNode def 'False 'False where
  reifyAnnotatedNode _ _ _ _ _ = []

-- ════════════════════════════════════════════════════════════════════════════
-- EDGE DERIVATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Derive implicit edges from nodes.
--
-- For each node, creates edges from:
-- 1. Entry → nodes that need the entry type
-- 2. Nodes with Schema → nodes that need that schema type
deriveImplicitEdges
  :: Maybe TypeRep    -- ^ Entry type
  -> [NodeInfo]       -- ^ All nodes
  -> [EdgeInfo]
deriveImplicitEdges mEntryType nodes = entryEdges ++ schemaEdges
  where
    -- Edges from Entry to nodes that need the entry type
    entryEdges = case mEntryType of
      Nothing -> []
      Just entryTy ->
        [ EdgeInfo "Entry" n.niName (Just entryTy) RuntimeImplicit
        | n <- nodes
        , n.niInput == Just entryTy
        ]

    -- Edges from nodes with Schema to nodes that need that type
    schemaEdges =
      [ EdgeInfo producer.niName consumer.niName (Just schemaTy) RuntimeImplicit
      | producer <- nodes
      , Just schemaInfo <- [producer.niSchema]
      , let schemaTy = schemaInfo.siType
      , consumer <- nodes
      , consumer.niInput == Just schemaTy
      ]

-- | Derive edges to Exit from Schema matching.
--
-- Any node with Schema matching exit type creates an edge to Exit.
deriveExitEdges
  :: Maybe TypeRep    -- ^ Exit type
  -> [NodeInfo]       -- ^ All nodes
  -> [EdgeInfo]
deriveExitEdges mExitType nodes = case mExitType of
  Nothing -> []
  Just exitTy ->
    [ EdgeInfo n.niName "Exit" (Just exitTy) RuntimeImplicit
    | n <- nodes
    , Just schemaInfo <- [n.niSchema]
    , schemaInfo.siType == exitTy
    ]

-- | Derive explicit Goto edges from Logic nodes.
--
-- For each Logic node, creates edges to its Goto targets.
deriveGotoEdges :: [NodeInfo] -> [EdgeInfo]
deriveGotoEdges nodes =
  [ EdgeInfo node.niName targetName (Just payload) RuntimeExplicit
  | node <- nodes
  , (targetName, payload) <- node.niGotoTargets
  ]

-- | Derive edges to Exit from Goto Exit.
--
-- Logic nodes with niHasGotoExit create an edge to Exit.
-- Note: We don't have the exact payload type, so we use Nothing.
deriveGotoExitEdges :: [NodeInfo] -> [EdgeInfo]
deriveGotoExitEdges nodes =
  [ EdgeInfo node.niName "Exit" Nothing RuntimeExplicit
  | node <- nodes
  , node.niHasGotoExit
  ]
