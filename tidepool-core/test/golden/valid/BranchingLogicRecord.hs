{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Branching graph with Logic node should compile.
--
-- Validates Goto target validation and Exit path checking.
module BranchingLogicRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, Schema, UsesEffects)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, LogicNode, ValidGraphRecord)
import Tidepool.Graph.Goto (Goto)

data Message
data Intent
data Response

-- | Branching graph: Entry -> classify -> router -> (refund | faq) -> Exit
data BranchingGraph mode = BranchingGraph
  { bgEntry    :: mode :- Entry Message
  , bgClassify :: mode :- LLMNode :@ Needs '[Message] :@ Schema Intent
  , bgRouter   :: mode :- LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "bgRefund" Message, Goto "bgFaq" Message]
  , bgRefund   :: mode :- LLMNode :@ Needs '[Message] :@ Schema Response
  , bgFaq      :: mode :- LLMNode :@ Needs '[Message] :@ Schema Response
  , bgExit     :: mode :- Exit Response
  }
  deriving Generic

-- This should compile without errors
validGraph :: ValidGraphRecord BranchingGraph => ()
validGraph = ()
