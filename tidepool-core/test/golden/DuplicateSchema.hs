{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module DuplicateSchema where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Response

-- This graph has two nodes with the same Schema type (Response).
-- The improved error message will now show which nodes conflict.
type BadGraph = Graph
  '[ Entry :~> Message
   , "nodeA" := LLM :@ Needs '[Message] :@ Schema Response
   , "nodeB" := LLM :@ Needs '[Message] :@ Schema Response  -- Duplicate!
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
