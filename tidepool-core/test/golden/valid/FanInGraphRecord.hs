{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Fan-in graph with multiple producers converging.
--
-- Validates that multiple nodes can satisfy a multi-element Needs list.
module FanInGraphRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, Schema)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord)

data Input
data Analysis
data Enrichment
data Combined

-- | Fan-in graph: Entry -> (analyze, enrich) -> combine -> Exit
-- The combine node needs both Analysis and Enrichment
data FanInGraph mode = FanInGraph
  { fiEntry   :: mode :- Entry Input
  , fiAnalyze :: mode :- LLMNode :@ Needs '[Input] :@ Schema Analysis
  , fiEnrich  :: mode :- LLMNode :@ Needs '[Input] :@ Schema Enrichment
  , fiCombine :: mode :- LLMNode :@ Needs '[Analysis, Enrichment] :@ Schema Combined
  , fiExit    :: mode :- Exit Combined
  }
  deriving Generic

-- This should compile without errors
validGraph :: ValidGraphRecord FanInGraph => ()
validGraph = ()
