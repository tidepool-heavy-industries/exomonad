-- | WASM reactor entry point.
--
-- This module exists solely to produce a statically-linked WASM executable
-- that includes all the FFI exports from Tidepool.Wasm.Ffi.
--
-- When building a library for WASM, GHC produces a dynamically-linked .so
-- that requires the GHC runtime to be loaded separately. By creating an
-- executable that imports the Ffi module, we get a self-contained WASM
-- binary that can run standalone in Cloudflare Workers.
--
-- The -no-hs-main flag means main is never called - the foreign exports
-- (initialize, step, etc.) are the entry points.
module Main where

-- Import Ffi and Roundtrip to ensure the foreign exports are linked
import Tidepool.Wasm.Ffi ()
import Tidepool.Wasm.Roundtrip ()

-- Dummy main - never called when using -no-hs-main, but GHC requires it
main :: IO ()
main = pure ()
