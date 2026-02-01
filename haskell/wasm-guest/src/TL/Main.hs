-- | TL role WASM entry point.
--
-- This module generates the WASM exports for the TL role using TH.
-- The resulting WASM blob includes all tools from 'TLTools'.
module Main where

import ExoMonad.Guest.Tool.TH (mkWasmExports)
import TL.Tools (TLTools)

-- Generate WASM exports for TLTools
$(mkWasmExports ''TLTools)

-- Required for WASM but not called (reactor model)
main :: IO ()
main = pure ()
