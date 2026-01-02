{-# LANGUAGE OverloadedStrings #-}

-- | Default graph entries for tidepool-wasm.
--
-- This module provides the standard graphs that ship with tidepool-wasm.
-- Applications can use 'setupDefaultRegistry' to register all default graphs,
-- or use individual entries to build a custom registry.
module Tidepool.Wasm.Registry.Default
  ( -- * Setup
    setupDefaultRegistry
  , defaultGraphEntries

    -- * Individual entries (re-exported)
  , testGraphEntry
  , exampleGraphEntry
  , habiticaGraphEntry
  ) where

import Data.Text (Text)
import Tidepool.Wasm.Registry (setRegistry)
import Tidepool.Wasm.Registry.Types (GraphEntry)
import Tidepool.Wasm.Registry.TestGraph (testGraphEntry)
import Tidepool.Wasm.Registry.ExampleGraph (exampleGraphEntry)
import Tidepool.Wasm.Registry.HabiticaGraph (habiticaGraphEntry)


-- | All default graph entries.
defaultGraphEntries :: [(Text, GraphEntry)]
defaultGraphEntries =
  [ ("test", testGraphEntry)
  , ("example", exampleGraphEntry)
  , ("habitica", habiticaGraphEntry)
  ]


-- | Set up the default registry with all standard graphs.
--
-- Call this at init for applications that want the standard tidepool-wasm graphs.
setupDefaultRegistry :: IO ()
setupDefaultRegistry = setRegistry defaultGraphEntries
