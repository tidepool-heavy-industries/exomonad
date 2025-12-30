{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module InvalidTool where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)

data Message
data Response
data UndefinedTool  -- No ToolDef instance

type BadGraph = Graph
  '[ Entry :~> Message
   , "process" := LLM :@ Needs '[Message] :@ Schema Response :@ Tools '[UndefinedTool]
   , Exit :<~ Response
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
