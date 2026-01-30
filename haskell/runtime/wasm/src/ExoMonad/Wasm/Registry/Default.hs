{-# LANGUAGE OverloadedStrings #-}

-- | Default graph entries for exomonad-wasm.
--
-- This module provides the standard graphs that ship with exomonad-wasm.
-- Applications can use 'setupDefaultRegistry' to register all default graphs,
-- or use individual entries to build a custom registry.
module ExoMonad.Wasm.Registry.Default
  ( -- * Setup
    setupDefaultRegistry,
    defaultGraphEntries,

    -- * Individual entries (re-exported)
    testGraphEntry,
    exampleGraphEntry,
  )
where

import Data.Text (Text)
import ExoMonad.Wasm.Registry (setRegistry)
import ExoMonad.Wasm.Registry.ExampleGraph (exampleGraphEntry)
import ExoMonad.Wasm.Registry.TestGraph (testGraphEntry)
import ExoMonad.Wasm.Registry.Types (GraphEntry)

-- | All default graph entries.
defaultGraphEntries :: [(Text, GraphEntry)]
defaultGraphEntries =
  [ ("test", testGraphEntry),
    ("example", exampleGraphEntry)
  ]

-- | Set up the default registry with all standard graphs.
--
-- Call this at init for applications that want the standard exomonad-wasm graphs.
setupDefaultRegistry :: IO ()
setupDefaultRegistry = setRegistry defaultGraphEntries
