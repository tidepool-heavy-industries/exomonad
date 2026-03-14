{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Process execution effect for running host commands.
--
-- Dispatched via the @process@ namespace.
-- Request and response types are proto-generated from @proto/effects/process.proto@.
module ExoMonad.Effects.Process
  ( -- * Effect Types
    ProcessRun,

    -- * Re-exported proto types
    module Effects.Process,
  )
where

import Effects.Process
import ExoMonad.Effect.Class (Effect (..))

data ProcessRun

instance Effect ProcessRun where
  type Input ProcessRun = RunRequest
  type Output ProcessRun = RunResponse
  effectId = "process.run"
