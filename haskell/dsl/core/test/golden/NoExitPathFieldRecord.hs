{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for Logic field without exit path in record-based graph.
--
-- This should produce:
--   Graph validation failed: Logic node cannot reach Exit
--   Field 'loop' has no path to Exit.
module NoExitPathFieldRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, LLMKind(..))
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LogicNode, ValidGraphRecord)
import Tidepool.Graph.Goto (Goto)

data A
data B

-- | Graph with Logic node that can't reach Exit.
-- 'loop' only goes back to 'loop', creating an infinite loop.
data BadGraph mode = BadGraph
  { entry :: mode :- Entry A
  , loop  :: mode :- LogicNode :@ Input A :@ UsesEffects '[Goto "loop" A]  -- Loops forever!
  , exit  :: mode :- Exit B
  }
  deriving Generic

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
