{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module UnsatisfiedNeed where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Intent
data Response

type BadGraph = Graph
  '[ Entry :~> Message
   , "process" := LLM :@ Needs '[Message, Intent] :@ Schema Response
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
