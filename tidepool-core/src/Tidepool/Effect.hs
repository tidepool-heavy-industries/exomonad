-- | Core effect types for Tidepool game loops
--
-- This module re-exports all effect types and operations from 'Tidepool.Effect.Types'.
-- For IO-based runners that need HTTP, SQLite, or GUI, see 'Tidepool.Effect.Runners'
-- in tidepool-platform.
module Tidepool.Effect
  ( -- * Effect Stacks
    module Tidepool.Effect.Types
    -- * DevLog
  , module Tidepool.Effect.DevLog
  ) where

import Tidepool.Effect.Types
import Tidepool.Effect.DevLog
