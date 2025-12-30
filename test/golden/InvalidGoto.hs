{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module InvalidGoto where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)
import Tidepool.Graph.Goto (Goto)

data Message
data Response

type BadGraph = Graph
  '[ Entry :~> Message
   , "router" := Logic :@ Needs '[Message] :@ Eff '[Goto "nonexistent" Message]
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
