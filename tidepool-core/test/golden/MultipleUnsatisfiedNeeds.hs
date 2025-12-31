{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Negative test: Multiple unsatisfied Needs should fail.
module MultipleUnsatisfiedNeeds where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Intent
data Context
data Response

-- | Invalid: Node needs both Intent and Context, but neither is provided
--
-- Expected error: "Graph validation failed: unsatisfied dependency"
type BadGraph = Graph
  '[ Entry :~> Message
   , "process" := LLM :@ Needs '[Message, Intent, Context] :@ Schema Response
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
