{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Golden test for missing Exit field in record-based graph.
--
-- This should produce:
--   Graph record validation failed: missing Exit field
--   Add a field like: exit :: mode :- Exit YourOutputType
module MissingExitRecord where

import ExoMonad.Graph.Generic (Entry, GraphMode (..), LLMNode, ValidGraphRecord)
import ExoMonad.Graph.Types (Input, LLMKind (..), Schema, type (:@))
import GHC.Generics (Generic)

data A

data B

-- | Graph missing Exit field - invalid!
data BadGraph mode = BadGraph
  { entry :: mode :- Entry A,
    node :: mode :- LLMNode 'API :@ Input A :@ Schema B
  }
  deriving (Generic)

check :: ()
check = validGraph @BadGraph

validGraph :: forall g. (ValidGraphRecord g) => ()
validGraph = ()
