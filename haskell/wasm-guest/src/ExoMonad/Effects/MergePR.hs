{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.MergePR
  ( MergePRMergePr,
    mergePR,
    module Effects.MergePr,
  )
where

import Effects.EffectError (EffectError)
import Effects.MergePr
import ExoMonad.Effect.Class (Effect (..), runEffect)

data MergePRMergePr

instance Effect MergePRMergePr where
  type Input MergePRMergePr = MergePrRequest
  type Output MergePRMergePr = MergePrResponse
  effectId = "merge_pr.merge_pr"

mergePR :: MergePrRequest -> IO (Either EffectError MergePrResponse)
mergePR = runEffect @MergePRMergePr
