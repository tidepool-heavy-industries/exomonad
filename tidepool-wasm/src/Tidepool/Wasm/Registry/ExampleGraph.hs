{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | ExampleGraph registry entry.
--
-- Uses makeGraphEntry for minimal boilerplate:
-- - Graph ID is single source of truth
-- - GraphInfo derived from ExampleGraph type via Generics
-- - Session management handled by Registry
module Tidepool.Wasm.Registry.ExampleGraph
  ( exampleGraphEntry
  ) where

import Tidepool.Wasm.Registry (makeGraphEntry, GraphEntry)
import Tidepool.Wasm.ExampleGraph (ExampleGraph, runExampleGraph)


-- | ExampleGraph entry for the registry.
--
-- That's it! makeGraphEntry:
-- - Derives nodes/edges from ExampleGraph type
-- - Uses "example" as graph ID everywhere (registry key, ActiveSession, graphInfo)
-- - Handles all session state boilerplate
exampleGraphEntry :: GraphEntry
exampleGraphEntry = makeGraphEntry @ExampleGraph "example" runExampleGraph
