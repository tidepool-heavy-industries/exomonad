{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module MissingExit where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Response

type BadGraph = Graph
  '[ Entry :~> Message
   , "process" := LLM :@ Needs '[Message] :@ Schema Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
