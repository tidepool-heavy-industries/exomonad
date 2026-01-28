{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | PMStatus tool definition for the Role DSL.
module ExoMonad.Control.Role.Tool.PMStatus
  ( PMStatus(..)
  , PmStatusArgs(..)
  , PmStatusResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.PMStatus.Types (PmStatusArgs(..), PmStatusResult(..))

-- | Marker type for the PMStatus tool.
data PMStatus = PMStatus

instance ToolSpec PMStatus where
  type Args PMStatus = PmStatusArgs
  type Result PMStatus = PmStatusResult
  toolName = "pm_status"
  toolDescription = "Get sprint health dashboard: velocity, cycle time, PR lag, and current state distribution."
