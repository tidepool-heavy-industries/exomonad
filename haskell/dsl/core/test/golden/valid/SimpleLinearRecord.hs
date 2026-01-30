{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Simple linear graph should compile successfully.
--
-- Entry -> node -> Exit, basic data flow validation.
module SimpleLinearRecord where

import ExoMonad.Graph.Generic (Entry, Exit, GraphMode (..), LLMNode, ValidGraphRecord)
import ExoMonad.Graph.Types (Input, LLMKind (..), Schema, type (:@))
import GHC.Generics (Generic)

data A

data B

-- | Simple linear graph: Entry -> node -> Exit
data SimpleGraph mode = SimpleGraph
  { sgEntry :: mode :- Entry A,
    sgNode :: mode :- LLMNode 'API :@ Input A :@ Schema B,
    sgExit :: mode :- Exit B
  }
  deriving (Generic)

-- This should compile without errors
validGraph :: (ValidGraphRecord SimpleGraph) => ()
validGraph = ()
