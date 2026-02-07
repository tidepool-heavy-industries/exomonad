{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Copilot effects for waiting on Copilot code reviews.
--
-- All effects are dispatched via the @copilot@ namespace.
-- Request and response types are proto-generated from @proto/effects/copilot.proto@.
module ExoMonad.Effects.Copilot
  ( -- * Effect Types
    CopilotWaitForReview,

    -- * Smart Constructors
    waitForCopilotReview,

    -- * Re-exported proto types
    module Effects.Copilot,
  )
where

import Effects.Copilot
import Effects.EffectError (EffectError)
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data CopilotWaitForReview

instance Effect CopilotWaitForReview where
  type Input CopilotWaitForReview = WaitForCopilotReviewRequest
  type Output CopilotWaitForReview = WaitForCopilotReviewResponse
  effectId = "copilot.wait_for_copilot_review"

-- ============================================================================
-- Smart constructors
-- ============================================================================

waitForCopilotReview :: WaitForCopilotReviewRequest -> IO (Either EffectError WaitForCopilotReviewResponse)
waitForCopilotReview = runEffect @CopilotWaitForReview
