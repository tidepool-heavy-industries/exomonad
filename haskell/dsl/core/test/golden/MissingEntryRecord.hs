{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for missing Entry field in record-based graph.
--
-- This should produce:
--   Graph record validation failed: missing Entry field
--   Add a field like: entry :: mode :- Entry YourInputType
module MissingEntryRecord where

import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, Schema, LLMKind(..))
import Tidepool.Graph.Generic (GraphMode(..), Exit, LLMNode, ValidGraphRecord)

data A
data B

-- | Graph missing Entry field - invalid!
data BadGraph mode = BadGraph
  { node :: mode :- LLMNode 'API :@ Input A :@ Schema B
  , exit :: mode :- Exit B
  }
  deriving Generic

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. ValidGraphRecord g => ()
validGraph = ()
