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

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, Self)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

data InputData
data Result

-- | Graph with a self-loop: the 'loop' node can transition back to itself.
data SelfLoopGraph mode = SelfLoopGraph
  { entry :: mode :- G.EntryNode InputData
  , loop  :: mode :- G.LogicNode :@ Input InputData
            :@ UsesEffects '[Goto Self InputData, Goto Exit Result]
  , exit  :: mode :- G.ExitNode Result
  }
  deriving Generic

-- This should compile without errors
validGraph :: G.ValidGraphRecord SelfLoopGraph => ()
validGraph = ()
