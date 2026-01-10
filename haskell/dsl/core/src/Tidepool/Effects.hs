-- | Unified re-export module for all Tidepool effects.
--
-- This module provides a single import point for both core effects
-- (Tidepool.Effect.Types) and integration effects (Tidepool.Effects.*).
--
-- Note: Some integration modules (LLMProvider, UI) are excluded to avoid
-- export conflicts with Effect.Types. Import those modules directly if needed.
--
-- Usage:
--
-- @
-- import Tidepool.Effects
--
-- myHandler :: (Member State effs, Member BD effs) => ...
-- @
module Tidepool.Effects
  ( -- * Core Effects
    module Tidepool.Effect.Types

    -- * Integration Effects
  , module Tidepool.Effects.BD
  , module Tidepool.Effects.Cabal
  , module Tidepool.Effects.Git
  , module Tidepool.Effects.GitHub
  , module Tidepool.Effects.Habitica
  , module Tidepool.Effects.Observability
  , module Tidepool.Effects.Worktree
  , module Tidepool.Effect.Session
  ) where

import Tidepool.Effect.Types
import Tidepool.Effects.BD
import Tidepool.Effects.Cabal
import Tidepool.Effects.Git
import Tidepool.Effects.GitHub
import Tidepool.Effects.Habitica
import Tidepool.Effects.Observability
import Tidepool.Effects.Worktree
import Tidepool.Effect.Session
