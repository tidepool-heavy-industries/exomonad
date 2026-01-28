{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | FilePR tool definition for the Role DSL.
module ExoMonad.Control.Role.Tool.FilePR
  ( FilePR(..)
  , FilePRArgs(..)
  , FilePRResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.ExoTools.FilePR.Types (FilePRArgs(..), FilePRResult(..))

-- | Marker type for the FilePR tool.
data FilePR = FilePR

instance ToolSpec FilePR where
  type Args FilePR = FilePRArgs
  type Result FilePR = FilePRResult
  toolName = "file_pr"
  toolDescription = "File a GitHub pull request for the current branch. Issue number and title are inferred from branch name."
