{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- | Platform constraints for WASM builds
--
-- In WASM builds, platform-specific constraints produce helpful type errors.
-- See 'src-platform-native/ExoMonad/Platform.hs' for the native version.
module ExoMonad.Platform
  ( NativeOnly
  ) where

import Data.Kind (Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

-- | Constraint for effects that require native execution.
--
-- In WASM builds, this constraint produces a compile-time error explaining
-- why the effect is not available.
--
-- Effects that are native-only (like LSP) add this constraint to their
-- smart constructors, preventing accidental use in WASM contexts.
type NativeOnly :: Constraint
type NativeOnly = TypeError
  ( 'Text "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  ':$$: 'Text ""
  ':$$: 'Text "  This effect is not available in WASM builds"
  ':$$: 'Text ""
  ':$$: 'Text "  Some effects require native execution capabilities:"
  ':$$: 'Text "    • LSP: requires language server subprocess (HLS, etc.)"
  ':$$: 'Text "    • Subprocess spawning via stdin/stdout"
  ':$$: 'Text "    • Local filesystem access"
  ':$$: 'Text ""
  ':$$: 'Text "  These are not available in browser/worker environments."
  ':$$: 'Text ""
  ':$$: 'Text "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  )
