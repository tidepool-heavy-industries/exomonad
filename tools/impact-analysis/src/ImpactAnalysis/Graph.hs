-- | V2 Graph DSL definition for impact analysis.
--
-- This module defines the graph structure:
--
-- @
-- Entry(ImpactInput) → LogicNode(analyze) → Exit(ImpactOutput)
-- @
--
module ImpactAnalysis.Graph
  ( ImpactGraph(..)
  ) where

import GHC.Generics (Generic)
import Tidepool.Graph.Generic (GraphMode(..), Entry, LogicNode)
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Types (Needs, UsesEffects, Exit, type (:@))
import Tidepool.Graph.Goto (Goto)

import ImpactAnalysis.Types


-- | Impact analysis graph: Entry → Analyze → Exit
--
-- Simple linear graph with one LogicNode that performs LSP queries
-- to gather symbol information, definitions, and references.
data ImpactGraph mode = ImpactGraph
  { igEntry   :: mode :- Entry ImpactInput
  , igAnalyze :: mode :- LogicNode
                    :@ Needs '[ImpactInput]
                    :@ UsesEffects '[Goto Exit ImpactOutput]
  , igExit    :: mode :- G.Exit ImpactOutput
  }
  deriving Generic
