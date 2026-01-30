{-# LANGUAGE StandaloneKindSignatures #-}

-- | Platform constraints for native builds
--
-- In native builds, platform-specific constraints are trivially satisfied.
-- See 'src-platform-wasm/ExoMonad/Platform.hs' for the WASM version.
module ExoMonad.Platform
  ( NativeOnly,
  )
where

import Data.Kind (Constraint)

-- | Constraint for effects that require native execution.
--
-- In native builds, this constraint is trivially satisfied (empty constraint).
-- In WASM builds, this constraint produces a helpful compile-time error.
--
-- Effects that are native-only (like LSP) add this constraint to their
-- smart constructors, preventing accidental use in WASM contexts.
type NativeOnly :: Constraint
type NativeOnly = ()
