{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- * Reification Typeclasses
  , ReifyGraph(..)

    -- * Helper Typeclasses
  , ReifyTypeList(..)
  , ReifyMaybeType(..)
  , ReifyGotoTargets(..)
  , ReifyNodeKind(..)
  , ReifyBool(..)
  ) where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import Tidepool.Graph.Types (NodeKind(..))
import Tidepool.Graph.Tool (ToolInfo(..))

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
data NodeInfo = NodeInfo
  { niName :: Text               -- ^ Node name (from Symbol)
  , niKind :: RuntimeNodeKind    -- ^ LLM or Logic
  , niNeeds :: [TypeRep]         -- ^ Types this node needs
  , niSchema :: Maybe TypeRep    -- ^ Schema output type (LLM nodes)
  , niGotoTargets :: [(Text, TypeRep)]  -- ^ Goto targets (Logic nodes)
  , niHasGotoExit :: Bool        -- ^ Can this node exit the graph?
  , niHasVision :: Bool          -- ^ Has Vision annotation?
  , niTools :: [TypeRep]         -- ^ Tool types (for backwards compat)
  , niToolInfos :: [ToolInfo]    -- ^ Full tool info with schemas (V2)
  , niSystem :: Maybe TypeRep    -- ^ System prompt template type
  , niTemplate :: Maybe TypeRep  -- ^ User prompt template type
  , niMemory :: Maybe TypeRep    -- ^ Memory type (node-private persistent state)
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
data RuntimeNodeKind = RuntimeLLM | RuntimeLogic
  deriving (Show, Eq)

-- | Runtime edge kind.
data RuntimeEdgeKind
  = RuntimeImplicit    -- ^ Schema → Needs (data flow)
  | RuntimeExplicit    -- ^ Goto (transition)
  deriving (Show, Eq)

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

-- Note: Reification instances for record-based graphs should be defined
-- using GHC.Generics traversal.
