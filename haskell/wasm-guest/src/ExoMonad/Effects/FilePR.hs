{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | File PR effects for creating pull requests from file changes.
--
-- All effects are dispatched via the @file_pr@ namespace.
-- Request and response types are proto-generated from @proto/effects/file_pr.proto@.
module ExoMonad.Effects.FilePR
  ( -- * Effect Types
    FilePRFilePr,

    -- * Smart Constructors
    filePR,

    -- * Re-exported proto types
    module Effects.FilePr,
  )
where

import Effects.EffectError (EffectError)
import Effects.FilePr
import ExoMonad.Effect.Class (Effect (..), runEffect)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data FilePRFilePr

instance Effect FilePRFilePr where
  type Input FilePRFilePr = FilePrRequest
  type Output FilePRFilePr = FilePrResponse
  effectId = "file_pr.file_pr"

-- ============================================================================
-- Smart constructors
-- ============================================================================

filePR :: FilePrRequest -> IO (Either EffectError FilePrResponse)
filePR = runEffect @FilePRFilePr
