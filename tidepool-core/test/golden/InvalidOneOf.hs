{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module InvalidOneOf where

import Tidepool.Graph.Types
import Tidepool.Graph.Validate (ValidGraph)
import Tidepool.Schema (deriveUsesOneOf)

data Message
data MyChoice = OptionA | OptionB

-- Mark MyChoice as using oneOf schema (sum type)
$(deriveUsesOneOf ''MyChoice)

type BadGraph = Graph
  '[ Entry :~> Message
   , "process" := LLM :@ Needs '[Message] :@ Schema MyChoice
   , Exit :<~ MyChoice
   ]

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraph g => ()
validGraph = ()
