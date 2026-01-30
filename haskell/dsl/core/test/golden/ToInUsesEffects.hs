{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for using 'To' markers in UsesEffects instead of 'Goto' effects.
--
-- This should produce:
--   Wrong type in UsesEffects
--   UsesEffects contains 'To' markers, but needs 'Goto' effects.
--
-- This is a common mistake when maintaining parallel type aliases for
-- graph definitions (Goto) and handler signatures (To).
module ToInUsesEffects where

import ExoMonad.Graph.Generic (GraphMode (..), LogicNode, ValidGraphRecord, type (:-))
import ExoMonad.Graph.Generic qualified as G (EntryNode, ExitNode)
import ExoMonad.Graph.Goto (To) -- Imported to demonstrate incorrect usage in UsesEffects
import ExoMonad.Graph.Types (Exit, Input, UsesEffects, type (:@))
import GHC.Generics (Generic)

data InputData

data Output

data Result

-- | Wrong: Using 'To' markers in UsesEffects
--
-- The correct approach is to use 'Goto' effects:
--   UsesEffects '[Goto "handler" Output, Goto Exit Result]
type WrongEffects = '[To "handler" Output, To Exit Result]

-- | Graph with wrong effect type - should produce a helpful error
data BadGraph mode = BadGraph
  { entry :: mode :- G.EntryNode InputData,
    router :: mode :- LogicNode :@ Input InputData :@ UsesEffects WrongEffects,
    handler :: mode :- LogicNode :@ Input Output :@ UsesEffects '[To Exit Result],
    exit :: mode :- G.ExitNode Result
  }
  deriving (Generic)

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. (ValidGraphRecord g) => ()
validGraph = ()
