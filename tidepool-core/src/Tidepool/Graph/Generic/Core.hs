{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core types for the record-based (Servant-style) Graph DSL.
--
-- This module exists to break the circular dependency between Generic.hs
-- and RecordStructure.hs. Both modules import these shared types.
module Tidepool.Graph.Generic.Core
  ( -- * Mode Class
    GraphMode(..)
  , (:-) -- Re-export the type family for convenience

    -- * Modes
  , AsGraph

    -- * Node Markers
  , LLMNode
  , LogicNode
  , Entry
  , Exit
  ) where

import Data.Kind (Type)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH MODE CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mode determines how graph record fields are interpreted.
--
-- Modes are the key to the Servant-style pattern:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- Entry Message
--   , classify :: mode :- LLMNode :@ Needs '[Message] :@ Schema Intent
--   , exit     :: mode :- Exit Response
--   }
-- @
--
-- With 'AsGraph' mode, fields are the node definitions themselves.
-- With 'AsHandler es' mode (in Generic.hs), fields become handler function types.
class GraphMode mode where
  type mode :- nodeDef :: Type

infixl 0 :-

-- ════════════════════════════════════════════════════════════════════════════
-- MODES
-- ════════════════════════════════════════════════════════════════════════════

-- | Identity mode - fields contain node definitions as-is.
--
-- This mode is used for:
-- * Type-level graph analysis and validation
-- * Extracting graph structure via Generic
data AsGraph

instance GraphMode AsGraph where
  type AsGraph :- nodeDef = nodeDef

-- ════════════════════════════════════════════════════════════════════════════
-- NODE MARKERS
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM node marker.
--
-- In record syntax, use this instead of the 'LLM' kind from Types.hs:
--
-- @
-- data MyGraph mode = MyGraph
--   { classify :: mode :- LLMNode :@ Needs '[Message] :@ Schema Intent
--   }
-- @
--
-- LLMNode has kind 'Type' (unlike 'LLM' which has kind 'NodeKind').
data LLMNode

-- | Logic node marker.
--
-- For pure/effectful routing logic:
--
-- @
-- data MyGraph mode = MyGraph
--   { router :: mode :- LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "next" ...]
--   }
-- @
data LogicNode

-- | Entry point marker (parameterized by input type).
--
-- @
-- data MyGraph mode = MyGraph
--   { entry :: mode :- Entry Message
--   }
-- @
type Entry :: Type -> Type
data Entry inputType

-- | Exit point marker (parameterized by output type).
--
-- @
-- data MyGraph mode = MyGraph
--   { exit :: mode :- Exit Response
--   }
-- @
type Exit :: Type -> Type
data Exit outputType
