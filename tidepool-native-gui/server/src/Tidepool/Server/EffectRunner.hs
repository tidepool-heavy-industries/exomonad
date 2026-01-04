-- | Effect runner - composes all executors.
--
-- Wires up UI, Habitica, Observability, and LLM executors.
module Tidepool.Server.EffectRunner
  ( -- * Runner
    runEffects
  ) where

-- | Run all effects (stub - actual implementation by Agent 1).
runEffects :: a -> a
runEffects = id  -- stub
