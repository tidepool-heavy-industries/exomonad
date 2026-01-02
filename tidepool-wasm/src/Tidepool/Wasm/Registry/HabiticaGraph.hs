{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | HabiticaRoutingGraph registry entry.
--
-- Uses makeGraphEntry for minimal boilerplate:
-- - Graph ID is single source of truth
-- - GraphInfo derived from HabiticaRoutingGraph type via Generics
-- - Session management handled by Registry
module Tidepool.Wasm.Registry.HabiticaGraph
  ( habiticaGraphEntry
  ) where

import Tidepool.Wasm.Registry (makeGraphEntry, GraphEntry)
import Tidepool.Wasm.HabiticaRoutingGraph (HabiticaRoutingGraph, runHabiticaRoutingGraph)


-- | HabiticaRoutingGraph entry for the registry.
--
-- That's it! makeGraphEntry:
-- - Derives nodes/edges from HabiticaRoutingGraph type
-- - Uses "habitica" as graph ID everywhere (registry key, ActiveSession, graphInfo)
-- - Handles all session state boilerplate
habiticaGraphEntry :: GraphEntry
habiticaGraphEntry = makeGraphEntry @HabiticaRoutingGraph "habitica" runHabiticaRoutingGraph
