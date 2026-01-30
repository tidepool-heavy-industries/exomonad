{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Mixed LLM and Logic nodes in a heterogeneous graph.
--
-- Validates that LLM and Logic nodes can coexist with proper data flow.
module MixedLLMLogicRecord where

import ExoMonad.Graph.Generic (GraphMode (..))
import ExoMonad.Graph.Generic qualified as G
import ExoMonad.Graph.Goto (Goto)
import ExoMonad.Graph.Types (Exit, Input, LLMKind (..), Schema, UsesEffects, type (:@))
import GHC.Generics (Generic)

data Query

data Intent

data Response

-- | Mixed graph: LLM classification followed by Logic routing to LLM handlers
-- Uses Goto Exit to demonstrate exit transitions from Logic nodes
data MixedGraph mode = MixedGraph
  { mgEntry :: mode :- G.EntryNode Query,
    mgClassify :: mode :- G.LLMNode 'API :@ Input Query :@ Schema Intent,
    mgRouter :: mode :- G.LogicNode :@ Input Intent :@ UsesEffects '[Goto "mgHandler" Query, Goto Exit Response],
    mgHandler :: mode :- G.LLMNode 'API :@ Input Query :@ Schema Response,
    mgExit :: mode :- G.ExitNode Response
  }
  deriving (Generic)

-- This should compile without errors
validGraph :: (G.ValidGraphRecord MixedGraph) => ()
validGraph = ()
