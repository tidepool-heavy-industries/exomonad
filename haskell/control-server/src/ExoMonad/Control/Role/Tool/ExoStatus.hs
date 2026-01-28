{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | ExoStatus tool definition for the Role DSL.
--
-- This module provides the 'ToolSpec' instance for the ExoStatus tool,
-- which retrieves development context (issue, worktree, PR info).
module ExoMonad.Control.Role.Tool.ExoStatus
  ( -- * Tool Marker
    ExoStatus(..)

    -- * Re-exports
  , ExoStatusArgs(..)
  , ExoStatusResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.ExoTools.Status.Types (ExoStatusArgs(..))
import ExoMonad.Control.ExoTools.Internal (ExoStatusResult(..))

-- | Marker type for the ExoStatus tool.
data ExoStatus = ExoStatus

-- | ToolSpec instance for ExoStatus.
instance ToolSpec ExoStatus where
  type Args ExoStatus = ExoStatusArgs
  type Result ExoStatus = ExoStatusResult
  toolName = "exo_status"
  toolDescription = "Get current development context including issue details, worktree info, dirty files, and associated PR."
