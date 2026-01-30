-- | Cabal interpreter - interprets the Cabal effect to IO.
--
-- Runs cabal commands in subprocesses.
--
-- = Usage
--
-- @
-- import ExoMonad.Cabal.Interpreter
--
-- -- Execution is handled by ExoMonad.Control.Effects.Cabal via docker-ctl.
-- @
module ExoMonad.Cabal.Interpreter
  ( -- * Re-exports
    Cabal (..),
    CabalResult (..),
    cabalBuild,
    cabalTest,
    cabalClean,
  )
where

import ExoMonad.Effects.Cabal
