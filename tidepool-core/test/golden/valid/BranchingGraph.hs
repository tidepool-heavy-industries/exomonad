{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Positive test: Branching graph with Goto compiles successfully.
module BranchingGraph where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraphStructure)
import Tidepool.Graph.Goto (Goto)

data Message
data Intent
data ResponseA
data ResponseB

-- | Branching graph with router node using Goto
type BranchingGraph = Graph
  '[ Entry :~> Message
   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
   , "router" := Logic
       :@ Needs '[Intent]
       :@ UsesEffects '[Goto "pathA" Message, Goto "pathB" Message, Goto Exit ResponseA]
   , "pathA" := LLM :@ Needs '[Message] :@ Schema ResponseA
   , "pathB" := LLM :@ Needs '[Message] :@ Schema ResponseB
   , Exit :<~ ResponseA
   ]

check :: ()
check = validGraph @BranchingGraph

validGraph :: forall g. ValidGraphStructure g => ()
validGraph = ()
