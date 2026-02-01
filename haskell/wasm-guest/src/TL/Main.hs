-- | TL role WASM entry point.
--
-- This module generates the WASM exports for the TL role using TH.
-- The resulting WASM blob includes all tools from 'TLTools'.
module Main where

import ExoMonad.Guest.Tool.TH (mkWasmExportsRecord)
import TL.Tools (tlToolsHandler)

-- Generate WASM exports for TLTools
$(mkWasmExportsRecord 'tlToolsHandler)

-- Required for WASM but not called (reactor model)
main :: IO ()
main = pure ()
