-- | DEPRECATED: DevLog configuration - use ExoMonad.Log.Types instead.
--
-- == Migration Guide
--
-- @
-- -- Before
-- import ExoMonad.DevLog.Config (DevLogConfig(..), defaultDevLogConfig, OutputMode(..))
--
-- -- After
-- import ExoMonad.Log.Types (LogConfig(..), defaultLogConfig, LogOutput(..))
-- @
--
-- Mapping:
-- - @OutputStderr@ -> @LogStderr@
-- - @OutputFile@ -> @LogFile@
-- - @OutputBoth@ -> @LogBoth@
-- - @dcVerbosity@ -> @lcMinLevel@ (VQuiet->Error, VNormal->Info, VVerbose->Debug, VTrace->Trace)
module ExoMonad.DevLog.Config
  {-# DEPRECATED "Use ExoMonad.Log.Types instead" #-}
  ( -- * Re-exports
    Verbosity(..)
  ) where

import ExoMonad.Effect.DevLog (Verbosity(..))
