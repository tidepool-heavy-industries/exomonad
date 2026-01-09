{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Fan-in graph with multiple producers converging.
--
-- Validates that tuple Input types are satisfied by multiple Schema outputs.
module FanInGraphRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, Schema)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord)

data InputData
data Analysis
data Enrichment
data Combined

-- | Fan-in graph: Entry -> (analyze, enrich) -> combine -> Exit
-- The combine node needs both Analysis and Enrichment as a tuple
data FanInGraph mode = FanInGraph
  { fiEntry   :: mode :- Entry InputData
  , fiAnalyze :: mode :- LLMNode :@ Input InputData :@ Schema Analysis
  , fiEnrich  :: mode :- LLMNode :@ Input InputData :@ Schema Enrichment
  , fiCombine :: mode :- LLMNode :@ Input (Analysis, Enrichment) :@ Schema Combined
  , fiExit    :: mode :- Exit Combined
  }
  deriving Generic

-- This should compile without errors
validGraph :: ValidGraphRecord FanInGraph => ()
validGraph = ()
