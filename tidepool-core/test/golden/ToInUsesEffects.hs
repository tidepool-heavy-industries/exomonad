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

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-), LogicNode, ValidGraphRecord)
import qualified Tidepool.Graph.Generic as G (Entry, Exit)
import Tidepool.Graph.Goto (To)  -- Imported to demonstrate incorrect usage in UsesEffects

data Input
data Output
data Result

-- | Wrong: Using 'To' markers in UsesEffects
--
-- The correct approach is to use 'Goto' effects:
--   UsesEffects '[Goto "handler" Output, Goto Exit Result]
type WrongEffects = '[To "handler" Output, To Exit Result]

-- | Graph with wrong effect type - should produce a helpful error
data BadGraph mode = BadGraph
  { entry   :: mode :- G.Entry Input
  , router  :: mode :- LogicNode :@ Needs '[Input] :@ UsesEffects WrongEffects
  , handler :: mode :- LogicNode :@ Needs '[Output] :@ UsesEffects '[To Exit Result]
  , exit    :: mode :- G.Exit Result
  }
  deriving Generic

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
