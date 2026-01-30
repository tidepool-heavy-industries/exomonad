{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | ExampleGraph registry entry.
--
-- Uses makeGraphEntry for minimal boilerplate:
-- - Graph ID is single source of truth
-- - GraphInfo derived from ExampleGraph type via Generics
-- - Session management handled by Registry
module ExoMonad.Wasm.Registry.ExampleGraph
  ( exampleGraphEntry,
  )
where

import ExoMonad.Wasm.ExampleGraph (ExampleGraph, runExampleGraph)
import ExoMonad.Wasm.Registry (GraphEntry, makeGraphEntry)

-- | ExampleGraph entry for the registry.
--
-- That's it! makeGraphEntry:
-- - Derives nodes/edges from ExampleGraph type
-- - Uses "example" as graph ID everywhere (registry key, ActiveSession, graphInfo)
-- - Handles all session state boilerplate
exampleGraphEntry :: GraphEntry
exampleGraphEntry = makeGraphEntry @ExampleGraph "example" runExampleGraph
