-- | Dev role WASM entry point.
--
-- This module generates the WASM exports for the Dev role using TH.
-- The resulting WASM blob only includes tools from 'DevTools'.
module Main where

import Dev.Tools (devToolsHandler)
import ExoMonad.Guest.Tool.TH (mkWasmExportsRecord)

-- Generate WASM exports for DevTools
$(mkWasmExportsRecord 'devToolsHandler)

-- Required for WASM but not called (reactor model)
main :: IO ()
main = pure ()
