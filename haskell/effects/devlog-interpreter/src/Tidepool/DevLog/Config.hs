-- | DEPRECATED: DevLog configuration - use Tidepool.Log.Types instead.
--
-- == Migration Guide
--
-- @
-- -- Before
-- import Tidepool.DevLog.Config (DevLogConfig(..), defaultDevLogConfig, OutputMode(..))
--
-- -- After
-- import Tidepool.Log.Types (LogConfig(..), defaultLogConfig, LogOutput(..))
-- @
--
-- Mapping:
-- - @OutputStderr@ -> @LogStderr@
-- - @OutputFile@ -> @LogFile@
-- - @OutputBoth@ -> @LogBoth@
-- - @dcVerbosity@ -> @lcMinLevel@ (VQuiet->Error, VNormal->Info, VVerbose->Debug, VTrace->Trace)
module Tidepool.DevLog.Config
  {-# DEPRECATED "Use Tidepool.Log.Types instead" #-}
  ( -- * Re-exports
    Verbosity(..)
  ) where

import Tidepool.Effect.DevLog (Verbosity(..))
