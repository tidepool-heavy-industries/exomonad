{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.MergePR
  ( MergePRMergePr,
    module Effects.MergePr,
  )
where

import Effects.MergePr
import ExoMonad.Effect.Class (Effect (..))

data MergePRMergePr

instance Effect MergePRMergePr where
  type Input MergePRMergePr = MergePrRequest
  type Output MergePRMergePr = MergePrResponse
  effectId = "merge_pr.merge_pr"
