{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Positive test: Fan-in graph (multiple inputs to one node) compiles.
module FanInGraph where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraphStructure)

data Input
data Analysis1
data Analysis2
data Output

-- | Fan-in: Entry fans out to parallel analyzers, then combines
type FanInGraph = Graph
  '[ Entry :~> Input
   , "analyze1" := LLM :@ Needs '[Input] :@ Schema Analysis1
   , "analyze2" := LLM :@ Needs '[Input] :@ Schema Analysis2
   , "combine" := LLM :@ Needs '[Analysis1, Analysis2] :@ Schema Output
   , Exit :<~ Output
   ]

check :: ()
check = validGraph @FanInGraph

validGraph :: forall g. ValidGraphStructure g => ()
validGraph = ()
