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
module ExoMonad.Graph.Reify
  ( -- * Graph Info Types
    GraphInfo (..),
    NodeInfo (..),
    EdgeInfo (..),
    RuntimeNodeKind (..),
    RuntimeEdgeKind (..),
    ToolInfo (..),

    -- * Rich Info Types
    SchemaInfo (..),
    TemplateInfo (..),
    MemoryInfo (..),

    -- * Reification Typeclasses
    ReifyGraph (..),

    -- * Record-Based Graph Reification
    ReifyRecordGraph (..),
    GReifyFields (..),
    ReifyNodeDef (..),
    makeGraphInfo,

    -- * EntryNode/Exit Type Extraction
    GetEntryTypeFromGraph,
    GetExitTypeFromGraph,

    -- * Helper Typeclasses
    ReifyTypeList (..),
    ReifyMaybeType (..),
    ReifyGotoTargets (..),
    ReifyNodeKind (..),
    ReifyBool (..),
    ReifySchemaInfo (..),
    ReifyTemplateInfo (..),
    ReifyMemoryInfo (..),

    -- * Goto Target Extraction
    GotoTargetsFromDef,
    GotoTargetsFromEffects,
    HasGotoExitInDef,
    HasGotoExitFromEffects,

    -- * Utilities
    simplifyTypeName,

    -- * Node Type Predicates
    IsForkNode,
    IsBarrierNode,
  )
where

import Data.Char (isAsciiUpper)
import Data.Kind (Constraint, Type)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (TypeRep, Typeable, typeRep)
import ExoMonad.Graph.Edges
  ( GetGotoTargets,
    GetInput,
    GetMemory,
    GetSchema,
    GetSystem,
    GetTemplate,
    GetTools,
    GetUsesEffects,
    GetVision,
    HasGotoExit,
  )
import ExoMonad.Graph.Generic.Core (AsGraph, BarrierNode, EntryNode, ExitNode, ForkNode, LLMNode, LogicNode)
import ExoMonad.Graph.Template
  ( TemplateContextInfo (..),
    TemplateDef (..),
    TemplateDependency (..),
    flattenDeps,
    templateAccessedFields,
    templateContextInfo,
    templateDependencyTree,
  )
import ExoMonad.Graph.Tool (ToolInfo (..))
import ExoMonad.Graph.Types (NodeKind (..), Tool, type (:@))
import ExoMonad.Schema (HasJSONSchema (..), JSONSchema (..), SchemaType (..))
import GHC.Generics (C, D, Generic (..), K1 (..), M1 (..), Meta (..), S, (:*:) (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | Effect type alias (freer-simple effects have kind Type -> Type).
type Effect = Type -> Type

-- | Helper type family to get EntryNode type from a graph.
--
-- This is a simplified version that works directly on graph records.
type GetEntryTypeFromGraph :: (Type -> Type) -> Maybe Type
type family GetEntryTypeFromGraph graph where
  GetEntryTypeFromGraph graph = GetEntryTypeRep (Rep (graph AsGraph))

-- | Extract EntryNode type from Generic representation.
type GetEntryTypeRep :: (Type -> Type) -> Maybe Type
type family GetEntryTypeRep f where
  GetEntryTypeRep (M1 D _ f) = GetEntryTypeRep f
  GetEntryTypeRep (M1 C _ f) = GetEntryTypeRep f
  GetEntryTypeRep (M1 S _ (K1 _ (EntryNode a))) = 'Just a
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
  GetExitTypeRep (M1 S _ (K1 _ (ExitNode a))) = 'Just a
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
  { -- | Type accepted at EntryNode
    giEntryType :: Maybe TypeRep,
    -- | Type produced at Exit
    giExitType :: Maybe TypeRep,
    -- | All node declarations
    giNodes :: [NodeInfo],
    -- | Derived edges
    giEdges :: [EdgeInfo],
    -- | Groups annotation (for Mermaid subgraphs)
    giGroups :: [(Text, [Text])]
  }
  deriving (Show, Eq)

-- | Runtime representation of a node.
--
-- Contains rich information about each node including template paths,
-- schema details, and tool information. This serves as the single source
-- of truth for Mermaid diagrams, JSON export, and D3 visualization.
data NodeInfo = NodeInfo
  { -- | Node name (from Symbol)
    niName :: Text,
    -- | LLM or Logic
    niKind :: RuntimeNodeKind,
    -- | Input type this node needs
    niInput :: Maybe TypeRep,
    -- | Rich schema info (LLM nodes)
    niSchema :: Maybe SchemaInfo,
    -- | Goto targets (Logic nodes)
    niGotoTargets :: [(Text, TypeRep)],
    -- | Can this node exit the graph?
    niHasGotoExit :: Bool,
    -- | Has Vision annotation?
    niHasVision :: Bool,
    -- | Tool types (for backwards compat)
    niTools :: [TypeRep],
    -- | Full tool info with schemas (V2)
    niToolInfos :: [ToolInfo],
    -- | System prompt template (rich info)
    niSystem :: Maybe TemplateInfo,
    -- | User prompt template (rich info)
    niTemplate :: Maybe TemplateInfo,
    -- | Memory type (rich info)
    niMemory :: Maybe MemoryInfo
  }
  deriving (Show, Eq)

-- | Runtime representation of an edge.
data EdgeInfo = EdgeInfo
  { -- | Source node (or "EntryNode")
    eiFrom :: Text,
    -- | Target node (or "Exit")
    eiTo :: Text,
    -- | Type carried on edge
    eiPayload :: Maybe TypeRep,
    -- | Edge classification
    eiKind :: RuntimeEdgeKind
  }
  deriving (Show, Eq)

-- | Runtime node kind.
data RuntimeNodeKind
  = -- | Standard LLM API call
    RuntimeLLM
  | -- | Pure routing logic
    RuntimeLogic
  | -- | Fork node - spawns parallel workers
    RuntimeFork
  | -- | Barrier node - collects parallel results
    RuntimeBarrier
  deriving (Show, Eq)

-- | Runtime edge kind.
data RuntimeEdgeKind
  = -- | Schema → Needs (data flow)
    RuntimeImplicit
  | -- | Goto (transition)
    RuntimeExplicit
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- RICH INFO TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Rich schema information including JSON schema and field details.
--
-- Captures everything an LLM or visualization tool needs to understand
-- the output type of an LLM node.
data SchemaInfo = SchemaInfo
  { -- | The Haskell type
    siType :: TypeRep,
    -- | Simplified type name (e.g., "Intent")
    siTypeName :: Text,
    -- | Full JSON schema with descriptions
    siJsonSchema :: JSONSchema,
    -- | (fieldName, fieldType, isRequired)
    siFields :: [(Text, Text, Bool)]
  }
  deriving (Show, Eq)

-- | Rich template information including file path and dependencies.
--
-- Captures everything needed to understand which template file is wired
-- to a node and what context it expects.
data TemplateInfo = TemplateInfo
  { -- | The template type (e.g., ClassifyTpl)
    tiType :: TypeRep,
    -- | Simplified type name
    tiTypeName :: Text,
    -- | Template file path (e.g., "templates/classify.jinja")
    tiPath :: FilePath,
    -- | Transitive template dependencies
    tiDeps :: [FilePath],
    -- | Fields accessed in template (e.g., ["topic", "history"])
    tiAccessedFields :: [String],
    -- | Context type name (e.g., "ClassifyContext")
    tiContextType :: Text
  }
  deriving (Show, Eq)

-- | Rich memory information for node-private persistent state.
data MemoryInfo = MemoryInfo
  { -- | The memory type
    miType :: TypeRep,
    -- | Simplified type name
    miTypeName :: Text
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
      (_, '.' : rest) ->
        case rest of
          (c : _) | isAsciiUpper c -> go rest
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

-- | Reify tool record fields to runtime [TypeRep] of tool input types.
--
-- This typeclass traverses Generic representation of tool records
-- (e.g., WorkTools mode) and extracts input types from Tool annotations.
--
-- Example:
-- @
-- data WorkTools mode = WorkTools
--   { search :: mode :- Tool SearchQuery SearchResult
--   , calc   :: mode :- Tool CalculatorInput CalculatorResult
--   }
--   deriving Generic
--
-- reifyToolRecord (Proxy @WorkTools) == [TypeRep @SearchQuery, TypeRep @CalculatorInput]
-- @
type ReifyToolRecord :: (Type -> Type) -> Constraint
class ReifyToolRecord (record :: Type -> Type) where
  reifyToolInputs :: Proxy record -> [TypeRep]

-- Default instance: traverse Generic representation
instance
  (GReifyToolFields (Rep (record AsGraph))) =>
  ReifyToolRecord record
  where
  reifyToolInputs _ = gReifyToolFields (Proxy @(Rep (record AsGraph)))

-- | Generic traversal for tool record fields.
--
-- Mirrors GReifyFields pattern but extracts tool input types instead of nodes.
class GReifyToolFields (f :: Type -> Type) where
  gReifyToolFields :: Proxy f -> [TypeRep]

-- Datatype metadata: pass through
instance (GReifyToolFields f) => GReifyToolFields (M1 D meta f) where
  gReifyToolFields _ = gReifyToolFields (Proxy @f)

-- Constructor metadata: pass through
instance (GReifyToolFields f) => GReifyToolFields (M1 C meta f) where
  gReifyToolFields _ = gReifyToolFields (Proxy @f)

-- Product: combine left and right
instance (GReifyToolFields l, GReifyToolFields r) => GReifyToolFields (l :*: r) where
  gReifyToolFields _ = gReifyToolFields (Proxy @l) ++ gReifyToolFields (Proxy @r)

-- Named field with Tool annotation: extract input type
instance
  (Typeable input) =>
  GReifyToolFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i (Tool input result)))
  where
  gReifyToolFields _ = [typeRep (Proxy @input)]

-- Unnamed field or non-Tool field: skip
instance GReifyToolFields (M1 S ('MetaSel 'Nothing su ss ds) f) where
  gReifyToolFields _ = []

instance GReifyToolFields (K1 i c) where
  gReifyToolFields _ = []

-- | Reify Maybe tool record to runtime [TypeRep].
--
-- Handles GetTools result which is Maybe (Type -> Type).
-- If Just, traverses the record. If Nothing, returns empty list.
type ReifyMaybeToolRecord :: Maybe (Type -> Type) -> Constraint
class ReifyMaybeToolRecord (mrecord :: Maybe (Type -> Type)) where
  reifyMaybeToolInputs :: Proxy mrecord -> [TypeRep]

instance ReifyMaybeToolRecord 'Nothing where
  reifyMaybeToolInputs _ = []

instance (ReifyToolRecord record) => ReifyMaybeToolRecord ('Just record) where
  reifyMaybeToolInputs _ = reifyToolInputs (Proxy @record)

-- | Reify a type-level Maybe Type to runtime Maybe TypeRep.
type ReifyMaybeType :: Maybe Type -> Constraint
class ReifyMaybeType (mt :: Maybe Type) where
  reifyMaybeType :: Proxy mt -> Maybe TypeRep

instance ReifyMaybeType 'Nothing where
  reifyMaybeType _ = Nothing

instance (Typeable t) => ReifyMaybeType ('Just t) where
  reifyMaybeType _ = Just (typeRep (Proxy @t))

-- | Reify a type-level list of (Symbol, Type) pairs to runtime [(Text, TypeRep)].
--
-- Used for Goto targets extraction.
type ReifyGotoTargets :: [(Symbol, Type)] -> Constraint
class ReifyGotoTargets (ts :: [(Symbol, Type)]) where
  reifyGotoTargets :: Proxy ts -> [(Text, TypeRep)]

instance ReifyGotoTargets '[] where
  reifyGotoTargets _ = []

instance
  (KnownSymbol name, Typeable payload, ReifyGotoTargets rest) =>
  ReifyGotoTargets ('(name, payload) ': rest)
  where
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
  reifySchemaInfo _ =
    Just
      SchemaInfo
        { siType = typeRep (Proxy @t),
          siTypeName = simplifyTypeName (typeRep (Proxy @t)),
          siJsonSchema = jsonSchema @t,
          siFields = extractSchemaFields (jsonSchema @t)
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
  reifyTemplateInfo _ =
    Just
      TemplateInfo
        { tiType = typeRep (Proxy @t),
          tiTypeName = simplifyTypeName (typeRep (Proxy @t)),
          tiPath = depRelativePath depTree,
          tiDeps = map depRelativePath $ drop 1 $ flattenDeps depTree, -- Skip root
          tiAccessedFields = templateAccessedFields compiled,
          tiContextType = T.pack $ tciTypeName $ templateContextInfo compiled
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

instance (Typeable t) => ReifyMemoryInfo ('Just t) where
  reifyMemoryInfo _ =
    Just
      MemoryInfo
        { miType = typeRep (Proxy @t),
          miTypeName = simplifyTypeName (typeRep (Proxy @t))
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
-- the record-based DSL (see "ExoMonad.Graph.Generic").
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
makeGraphInfo ::
  forall graph.
  ( GReifyFields (Rep (graph AsGraph)),
    ReifyMaybeType (GetEntryTypeFromGraph graph),
    ReifyMaybeType (GetExitTypeFromGraph graph)
  ) =>
  Proxy graph ->
  GraphInfo
makeGraphInfo _ =
  GraphInfo
    { giEntryType = entryType,
      giExitType = exitType,
      giNodes = nodes,
      giEdges =
        deriveImplicitEdges entryType nodes
          ++ deriveExitEdges exitType nodes
          ++ deriveGotoEdges nodes
          ++ deriveGotoExitEdges nodes,
      giGroups = [] -- Would need graph-level annotation extraction
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
instance (GReifyFields f) => GReifyFields (M1 D meta f) where
  gReifyFields _ = gReifyFields (Proxy @f)

-- Constructor metadata: pass through
instance (GReifyFields f) => GReifyFields (M1 C meta f) where
  gReifyFields _ = gReifyFields (Proxy @f)

-- Product: combine left and right
instance (GReifyFields l, GReifyFields r) => GReifyFields (l :*: r) where
  gReifyFields _ = gReifyFields (Proxy @l) ++ gReifyFields (Proxy @r)

-- Named field: extract name and delegate to ReifyNodeDef
instance
  (KnownSymbol name, ReifyNodeDef def) =>
  GReifyFields (M1 S ('MetaSel ('Just name) su ss ds) (K1 i def))
  where
  gReifyFields _ = reifyNodeDef (Proxy @name) (Proxy @def)

-- Unnamed field or bare K1: no nodes
instance GReifyFields (M1 S ('MetaSel 'Nothing su ss ds) f) where
  gReifyFields _ = []

instance GReifyFields (K1 i c) where
  gReifyFields _ = []

-- | Reify a single node definition to Maybe NodeInfo.
--
-- Returns @[]@ for EntryNode/Exit (not real nodes), @[NodeInfo]@ for LLMNode/LogicNode.
class ReifyNodeDef (def :: Type) where
  reifyNodeDef :: (KnownSymbol name) => Proxy name -> Proxy def -> [NodeInfo]

-- EntryNode and Exit are not nodes
instance ReifyNodeDef (EntryNode a) where
  reifyNodeDef _ _ = []

instance ReifyNodeDef (ExitNode a) where
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
  IsLLMNode (LLMNode _subtype) = 'True -- LLMNode now has subtype parameter
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
instance
  {-# OVERLAPPING #-}
  (ReifyMaybeType (GetInput (ForkNode :@ ann))) =>
  ReifyNodeDef (ForkNode :@ ann)
  where
  reifyNodeDef pName _ =
    [ NodeInfo
        { niName = T.pack (symbolVal pName),
          niKind = RuntimeFork,
          niInput = reifyMaybeType (Proxy @(GetInput (ForkNode :@ ann))),
          niSchema = Nothing,
          niGotoTargets = [], -- ForkNode uses Spawn, not Goto
          niHasGotoExit = False,
          niHasVision = False,
          niTools = [],
          niToolInfos = [],
          niSystem = Nothing,
          niTemplate = Nothing,
          niMemory = Nothing
        }
    ]

-- BarrierNode instance - collects parallel results
--
-- Basic support: extracts input, produces RuntimeBarrier kind.
-- TODO: Could extract Awaits types for richer visualization.
instance
  {-# OVERLAPPING #-}
  ( ReifyMaybeType (GetInput (BarrierNode :@ ann)),
    ReifyGotoTargets (GotoTargetsFromDef (BarrierNode :@ ann)),
    ReifyBool (HasGotoExitInDef (BarrierNode :@ ann))
  ) =>
  ReifyNodeDef (BarrierNode :@ ann)
  where
  reifyNodeDef pName _ =
    [ NodeInfo
        { niName = T.pack (symbolVal pName),
          niKind = RuntimeBarrier,
          niInput = reifyMaybeType (Proxy @(GetInput (BarrierNode :@ ann))),
          niSchema = Nothing,
          niGotoTargets = reifyGotoTargets (Proxy @(GotoTargetsFromDef (BarrierNode :@ ann))),
          niHasGotoExit = reifyBool (Proxy @(HasGotoExitInDef (BarrierNode :@ ann))),
          niHasVision = False,
          niTools = [],
          niToolInfos = [],
          niSystem = Nothing,
          niTemplate = Nothing,
          niMemory = Nothing
        }
    ]

-- General instance for any annotated node: dispatch based on base type
instance
  {-# OVERLAPPABLE #-}
  (ReifyAnnotatedNode (def :@ ann) (IsLLMNode (def :@ ann)) (IsLogicNode (def :@ ann))) =>
  ReifyNodeDef (def :@ ann)
  where
  reifyNodeDef pName pDef = reifyAnnotatedNode pDef (Proxy @(IsLLMNode (def :@ ann))) (Proxy @(IsLogicNode (def :@ ann))) pName pDef

-- | Helper class to dispatch based on node kind.
class ReifyAnnotatedNode (def :: Type) (isLLM :: Bool) (isLogic :: Bool) where
  reifyAnnotatedNode ::
    Proxy def ->
    Proxy isLLM ->
    Proxy isLogic ->
    (forall name. (KnownSymbol name) => Proxy name -> Proxy def -> [NodeInfo])

-- LLMNode case
--
-- Uses rich info typeclasses to extract template paths, schema fields, etc.
instance
  ( ReifyMaybeType (GetInput def),
    ReifySchemaInfo (GetSchema def),
    ReifyTemplateInfo (GetTemplate def),
    ReifyTemplateInfo (GetSystem def),
    ReifyMemoryInfo (GetMemory def),
    ReifyBool (GetVision def),
    ReifyMaybeToolRecord (GetTools def)
  ) =>
  ReifyAnnotatedNode def 'True 'False
  where
  reifyAnnotatedNode _ _ _ pName _ =
    [ NodeInfo
            { niName = T.pack (symbolVal pName),
              niKind = RuntimeLLM,
              niInput = reifyMaybeType (Proxy @(GetInput def)),
              niSchema = reifySchemaInfo (Proxy @(GetSchema def)),
              niGotoTargets = [], -- LLM nodes don't have Goto
              niHasGotoExit = False,
              niHasVision = reifyBool (Proxy @(GetVision def)),
              niTools = reifyMaybeToolInputs (Proxy @(GetTools def)),
              niToolInfos = [], -- Would require ToolDef instances
              niSystem = reifyTemplateInfo (Proxy @(GetSystem def)),
              niTemplate = reifyTemplateInfo (Proxy @(GetTemplate def)),
              niMemory = reifyMemoryInfo (Proxy @(GetMemory def))
            }
        ]

-- LogicNode case
--
-- Extracts Goto targets by using GotoTargetsFromDef, which applies
-- @Effect kind explicitly to resolve polykind ambiguity.
instance
  ( ReifyMaybeType (GetInput def),
    ReifyMemoryInfo (GetMemory def),
    ReifyGotoTargets (GotoTargetsFromDef def),
    ReifyBool (HasGotoExitInDef def)
  ) =>
  ReifyAnnotatedNode def 'False 'True
  where
  reifyAnnotatedNode _ _ _ pName _ =
    [ NodeInfo
        { niName = T.pack (symbolVal pName),
          niKind = RuntimeLogic,
          niInput = reifyMaybeType (Proxy @(GetInput def)),
          niSchema = Nothing, -- Logic nodes don't have Schema
          niGotoTargets = reifyGotoTargets (Proxy @(GotoTargetsFromDef def)),
          niHasGotoExit = reifyBool (Proxy @(HasGotoExitInDef def)),
          niHasVision = False,
          niTools = [],
          niToolInfos = [],
          niSystem = Nothing,
          niTemplate = Nothing,
          niMemory = reifyMemoryInfo (Proxy @(GetMemory def))
        }
    ]

-- Neither LLM nor Logic - unknown node type, return empty
instance ReifyAnnotatedNode def 'False 'False where
  reifyAnnotatedNode _ _ _ _ _ = []

-- ════════════════════════════════════════════════════════════════════════════
-- EDGE DERIVATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Derive implicit edges from nodes.
--
-- For each node, creates edges from:
-- 1. EntryNode → nodes that need the entry type
-- 2. Nodes with Schema → nodes that need that schema type
deriveImplicitEdges ::
  -- | EntryNode type
  Maybe TypeRep ->
  -- | All nodes
  [NodeInfo] ->
  [EdgeInfo]
deriveImplicitEdges mEntryType nodes = entryEdges ++ schemaEdges
  where
    -- Edges from EntryNode to nodes that need the entry type
    entryEdges = case mEntryType of
      Nothing -> []
      Just entryTy ->
        [ EdgeInfo "EntryNode" n.niName (Just entryTy) RuntimeImplicit
        | n <- nodes,
          n.niInput == Just entryTy
        ]

    -- Edges from nodes with Schema to nodes that need that type
    schemaEdges =
      [ EdgeInfo producer.niName consumer.niName (Just schemaTy) RuntimeImplicit
      | producer <- nodes,
        Just schemaInfo <- [producer.niSchema],
        let schemaTy = schemaInfo.siType,
        consumer <- nodes,
        consumer.niInput == Just schemaTy
      ]

-- | Derive edges to Exit from Schema matching.
--
-- Any node with Schema matching exit type creates an edge to Exit.
deriveExitEdges ::
  -- | Exit type
  Maybe TypeRep ->
  -- | All nodes
  [NodeInfo] ->
  [EdgeInfo]
deriveExitEdges mExitType nodes = case mExitType of
  Nothing -> []
  Just exitTy ->
    [ EdgeInfo n.niName "Exit" (Just exitTy) RuntimeImplicit
    | n <- nodes,
      Just schemaInfo <- [n.niSchema],
      schemaInfo.siType == exitTy
    ]

-- | Derive explicit Goto edges from Logic nodes.
--
-- For each Logic node, creates edges to its Goto targets.
deriveGotoEdges :: [NodeInfo] -> [EdgeInfo]
deriveGotoEdges nodes =
  [ EdgeInfo node.niName targetName (Just payload) RuntimeExplicit
  | node <- nodes,
    (targetName, payload) <- node.niGotoTargets
  ]

-- | Derive edges to Exit from Goto Exit.
--
-- Logic nodes with niHasGotoExit create an edge to Exit.
-- Note: We don't have the exact payload type, so we use Nothing.
deriveGotoExitEdges :: [NodeInfo] -> [EdgeInfo]
deriveGotoExitEdges nodes =
  [ EdgeInfo node.niName "Exit" Nothing RuntimeExplicit
  | node <- nodes,
    node.niHasGotoExit
  ]
