{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Branching graph with Logic node should compile.
--
-- Validates Goto target validation and Exit path checking.
module BranchingLogicRecord where

import ExoMonad.Graph.Generic (Entry, Exit, GraphMode (..), LLMNode, LogicNode, ValidGraphRecord)
import ExoMonad.Graph.Goto (Goto)
import ExoMonad.Graph.Types (Input, LLMKind (..), Schema, UsesEffects, type (:@))
import GHC.Generics (Generic)

data Message

data Intent

data Response

-- | Branching graph: Entry -> classify -> router -> (refund | faq) -> Exit
data BranchingGraph mode = BranchingGraph
  { bgEntry :: mode :- Entry Message,
    bgClassify :: mode :- LLMNode 'API :@ Input Message :@ Schema Intent,
    bgRouter :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "bgRefund" Message, Goto "bgFaq" Message],
    bgRefund :: mode :- LLMNode 'API :@ Input Message :@ Schema Response,
    bgFaq :: mode :- LLMNode 'API :@ Input Message :@ Schema Response,
    bgExit :: mode :- Exit Response
  }
  deriving (Generic)

-- This should compile without errors
validGraph :: (ValidGraphRecord BranchingGraph) => ()
validGraph = ()
