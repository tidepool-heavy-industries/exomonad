{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Simple linear graph should compile successfully.
--
-- Entry -> node -> Exit, basic data flow validation.
module SimpleLinearRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, Schema)
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord)

data A
data B

-- | Simple linear graph: Entry -> node -> Exit
data SimpleGraph mode = SimpleGraph
  { sgEntry :: mode :- Entry A
  , sgNode  :: mode :- LLMNode :@ Input A :@ Schema B
  , sgExit  :: mode :- Exit B
  }
  deriving Generic

-- This should compile without errors
validGraph :: ValidGraphRecord SimpleGraph => ()
validGraph = ()
