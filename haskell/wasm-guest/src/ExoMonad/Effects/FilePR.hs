{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | File PR effects for creating pull requests from file changes.
--
-- All effects are dispatched via the @file_pr@ namespace.
-- Request and response types are proto-generated from @proto/effects/file_pr.proto@.
module ExoMonad.Effects.FilePR
  ( -- * Effect Types
    FilePRFilePr,

    -- * Re-exported proto types
    module Effects.FilePr,
  )
where

import Effects.FilePr
import ExoMonad.Effect.Class (Effect (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data FilePRFilePr

instance Effect FilePRFilePr where
  type Input FilePRFilePr = FilePrRequest
  type Output FilePRFilePr = FilePrResponse
  effectId = "file_pr.file_pr"
