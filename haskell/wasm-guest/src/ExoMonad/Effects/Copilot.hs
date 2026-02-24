{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Copilot effects for waiting on Copilot code reviews.
--
-- All effects are dispatched via the @copilot@ namespace.
-- Request and response types are proto-generated from @proto/effects/copilot.proto@.
module ExoMonad.Effects.Copilot
  ( -- * Effect Types
    CopilotWaitForReview,

    -- * Re-exported proto types
    module Effects.Copilot,
  )
where

import Effects.Copilot
import ExoMonad.Effect.Class (Effect (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data CopilotWaitForReview

instance Effect CopilotWaitForReview where
  type Input CopilotWaitForReview = WaitForCopilotReviewRequest
  type Output CopilotWaitForReview = WaitForCopilotReviewResponse
  effectId = "copilot.wait_for_copilot_review"
