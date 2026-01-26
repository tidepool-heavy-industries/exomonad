{-# LANGUAGE CPP #-}

-- | WASM reactor entry point.
--
-- This module exists solely to produce a statically-linked WASM executable
-- that includes all the FFI exports from the unified Registry.
--
-- When building a library for WASM, GHC produces a dynamically-linked .so
-- that requires the GHC runtime to be loaded separately. By creating an
-- executable that imports the Ffi.Unified module, we get a self-contained WASM
-- binary that can run standalone in Cloudflare Workers.
--
-- The -no-hs-main flag means main is never called - the foreign exports
-- (initialize, step, getGraphInfo, getGraphState) are the entry points.
module Main where

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString(..))
#endif

-- Import unified FFI and Roundtrip to ensure the foreign exports are linked
import ExoMonad.Wasm.Ffi.Unified ()
import ExoMonad.Wasm.Roundtrip ()
import ExoMonad.Wasm.Registry.Default (setupDefaultRegistry)


-- | Initialize the registry with default graphs.
-- TypeScript must call this once after loading WASM, before any initialize() calls.
initRegistry :: IO ()
initRegistry = setupDefaultRegistry

#if defined(wasm32_HOST_ARCH)
foreign export javascript "initRegistry" initRegistry :: IO ()
#endif

-- Dummy main - never called when using -no-hs-main, but GHC requires it
main :: IO ()
main = pure ()
