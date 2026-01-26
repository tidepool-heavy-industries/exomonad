{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Simple linear graph should compile successfully.
--
-- Entry -> node -> Exit, basic data flow validation.
module SimpleLinearRecord where

import GHC.Generics (Generic)

import ExoMonad.Graph.Types (type (:@), Input, Schema, LLMKind(..))
import ExoMonad.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord)

data A
data B

-- | Simple linear graph: Entry -> node -> Exit
data SimpleGraph mode = SimpleGraph
  { sgEntry :: mode :- Entry A
  , sgNode  :: mode :- LLMNode 'API :@ Input A :@ Schema B
  , sgExit  :: mode :- Exit B
  }
  deriving Generic

-- This should compile without errors
validGraph :: ValidGraphRecord SimpleGraph => ()
validGraph = ()
