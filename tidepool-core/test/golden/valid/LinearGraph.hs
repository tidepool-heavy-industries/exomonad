{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Positive test: Simple linear graph compiles successfully.
module LinearGraph where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraphStructure)

data Input
data Middle
data Output

-- | Linear graph: Entry -> step1 -> step2 -> Exit
type LinearGraph = Graph
  '[ Entry :~> Input
   , "step1" := LLM :@ Needs '[Input] :@ Schema Middle
   , "step2" := LLM :@ Needs '[Middle] :@ Schema Output
   , Exit :<~ Output
   ]

check :: ()
check = validGraph @LinearGraph

validGraph :: forall g. ValidGraphStructure g => ()
validGraph = ()
