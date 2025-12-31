{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Positive test: Graph with Goto Exit compiles.
module GotoExitGraph where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraphStructure)
import Tidepool.Graph.Goto (Goto)

data Query
data Result
data Response

-- | Graph where logic node can exit directly
type GotoExitGraph = Graph
  '[ Entry :~> Query
   , "process" := LLM :@ Needs '[Query] :@ Schema Result
   , "decide" := Logic :@ Needs '[Result] :@ UsesEffects '[Goto Exit Response]
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @GotoExitGraph

validGraph :: forall g. ValidGraphStructure g => ()
validGraph = ()
