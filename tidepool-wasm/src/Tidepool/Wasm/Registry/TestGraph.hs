{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | TestGraph registry entry.
--
-- Uses makeGraphEntry for minimal boilerplate:
-- - Graph ID is single source of truth
-- - GraphInfo derived from TestGraph type via Generics
-- - Session management handled by Registry
module Tidepool.Wasm.Registry.TestGraph
  ( testGraphEntry
  ) where

import Tidepool.Wasm.Registry (makeGraphEntry, GraphEntry)
import Tidepool.Wasm.TestGraph (TestGraph, computeHandlerWasm)


-- | TestGraph entry for the registry.
--
-- That's it! makeGraphEntry:
-- - Derives nodes/edges from TestGraph type
-- - Uses "test" as graph ID everywhere (registry key, ActiveSession, graphInfo)
-- - Handles all session state boilerplate
testGraphEntry :: GraphEntry
testGraphEntry = makeGraphEntry @TestGraph "test" computeHandlerWasm
