-- | DEPRECATED: DevLog interpreter - use ExoMonad.Log.Interpreter instead.
--
-- The DevLog effect has been consolidated into the Log effect.
-- This module provides backward compatibility shims.
--
-- == Migration Guide
--
-- Before:
-- @
-- import ExoMonad.DevLog.Interpreter (runDevLog)
-- import ExoMonad.DevLog.Config
-- runDevLog config $ do ...
-- @
--
-- After:
-- @
-- import ExoMonad.Log.Interpreter (withLogInterpreter, defaultLogConfig)
-- withLogInterpreter config $ \runLog -> runLog $ do ...
-- @
module ExoMonad.DevLog.Interpreter
  {-# DEPRECATED "Use ExoMonad.Log.Interpreter instead" #-}
  ( -- * Re-exports from Log interpreter
    module ExoMonad.DevLog.Config
  ) where

import ExoMonad.DevLog.Config
