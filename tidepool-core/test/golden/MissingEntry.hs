{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module MissingEntry where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Response

type BadGraph = Graph
  '[ "process" := LLM :@ Needs '[Message] :@ Schema Response
   , Exit :<~ Response
   ]

-- Force the constraint to be solved at compile time
check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
