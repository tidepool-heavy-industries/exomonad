-- | DEPRECATED: DevLog interpreter - use Tidepool.Log.Interpreter instead.
--
-- The DevLog effect has been consolidated into the Log effect.
-- This module provides backward compatibility shims.
--
-- == Migration Guide
--
-- Before:
-- @
-- import Tidepool.DevLog.Interpreter (runDevLog)
-- import Tidepool.DevLog.Config
-- runDevLog config $ do ...
-- @
--
-- After:
-- @
-- import Tidepool.Log.Interpreter (withLogInterpreter, defaultLogConfig)
-- withLogInterpreter config $ \runLog -> runLog $ do ...
-- @
module Tidepool.DevLog.Interpreter
  {-# DEPRECATED "Use Tidepool.Log.Interpreter instead" #-}
  ( -- * Re-exports from Log interpreter
    module Tidepool.DevLog.Config
  ) where

import Tidepool.DevLog.Config
