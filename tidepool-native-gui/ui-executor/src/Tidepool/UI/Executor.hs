-- | UI effect executor - bridges UI effects to wire types.
--
-- Interprets UI effect by:
-- 1. Accumulating UI state
-- 2. Emitting UIState via callback on Request* effects
-- 3. Resuming when UserAction arrives
module Tidepool.UI.Executor
  ( -- * Executor
    runUI
  , UICallback
  ) where

import Data.Text (Text)

import Tidepool.Effects.UI (UI(..))

-- | Callback for emitting UI state and receiving user actions.
type UICallback = Text -> IO Text  -- placeholder

-- | Run UI effects (stub - actual implementation by Agent 4).
runUI :: UICallback -> a -> a
runUI _ = id  -- stub
