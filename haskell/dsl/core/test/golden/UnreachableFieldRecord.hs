{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for unreachable field in record-based graph.
--
-- This should produce:
--   Graph validation failed: unreachable node
--   Field 'orphan' cannot be reached from Entry.
module UnreachableFieldRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, Schema, LLMKind(..))
import Tidepool.Graph.Generic (GraphMode(..), Entry, Exit, LLMNode, ValidGraphRecord)

data A
data B
data C
data X  -- Type that nobody provides

-- | Graph with unreachable field.
-- 'orphan' needs X, but no Entry or Schema provides it.
data BadGraph mode = BadGraph
  { entry     :: mode :- Entry A
  , reachable :: mode :- LLMNode 'API :@ Input A :@ Schema B
  , orphan    :: mode :- LLMNode 'API :@ Input X :@ Schema C  -- X is never provided!
  , exit      :: mode :- Exit B
  }
  deriving Generic

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
