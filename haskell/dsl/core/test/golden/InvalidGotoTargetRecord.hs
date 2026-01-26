{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for invalid Goto target in record-based graph.
--
-- This should produce:
--   Graph validation failed: invalid Goto target
--   Goto "nonexistent" references a node that doesn't exist.
module InvalidGotoTargetRecord where

import GHC.Generics (Generic)

import ExoMonad.Graph.Types (type (:@), Input, Schema, UsesEffects, LLMKind(..))
import ExoMonad.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, LogicNode, ValidGraphRecord)
import ExoMonad.Graph.Goto (Goto)

data A
data B
data C

-- | Graph with Logic node pointing to nonexistent field - invalid!
data BadGraph mode = BadGraph
  { entry  :: mode :- Entry A
  , router :: mode :- LogicNode :@ Input A :@ UsesEffects '[Goto "nonexistent" B, Goto "handler" B]
  , handler :: mode :- LLMNode 'API :@ Input B :@ Schema C
  , exit   :: mode :- Exit C
  }
  deriving Generic

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
