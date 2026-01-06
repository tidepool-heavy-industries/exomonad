{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Mixed LLM and Logic nodes in a heterogeneous graph.
--
-- Validates that LLM and Logic nodes can coexist with proper data flow.
module MixedLLMLogicRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, Schema, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

data Query
data Intent
data Response

-- | Mixed graph: LLM classification followed by Logic routing to LLM handlers
-- Uses Goto Exit to demonstrate exit transitions from Logic nodes
data MixedGraph mode = MixedGraph
  { mgEntry    :: mode :- G.Entry Query
  , mgClassify :: mode :- G.LLMNode :@ Input Query :@ Schema Intent
  , mgRouter   :: mode :- G.LogicNode :@ Input Intent :@ UsesEffects '[Goto "mgHandler" Query, Goto Exit Response]
  , mgHandler  :: mode :- G.LLMNode :@ Input Query :@ Schema Response
  , mgExit     :: mode :- G.Exit Response
  }
  deriving Generic

-- This should compile without errors
validGraph :: G.ValidGraphRecord MixedGraph => ()
validGraph = ()
