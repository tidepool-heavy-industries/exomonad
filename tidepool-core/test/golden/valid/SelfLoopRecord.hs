{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test: Graph with Goto Self should compile.
--
-- Validates that Goto Self is supported in UsesEffects annotations.
-- Note: Execution of Self-loops requires using DispatchGotoWithSelf
-- instead of the regular DispatchGoto.
module SelfLoopRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit, Self)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

data Input
data Result

-- | Graph with a self-loop: the 'loop' node can transition back to itself.
data SelfLoopGraph mode = SelfLoopGraph
  { entry :: mode :- G.Entry Input
  , loop  :: mode :- G.LogicNode :@ Needs '[Input]
            :@ UsesEffects '[Goto Self Input, Goto Exit Result]
  , exit  :: mode :- G.Exit Result
  }
  deriving Generic

-- This should compile without errors
validGraph :: G.ValidGraphRecord SelfLoopGraph => ()
validGraph = ()
