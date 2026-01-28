{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Control.Role.Tool.KaizenReport
  ( -- * Tool Marker
    KaizenReport(..)
    -- * Re-exports
  , KaizenReportArgs(..)
  , KaizenReportResult(..)
  ) where

import ExoMonad.Control.Role.Types (ToolSpec(..))
import ExoMonad.Control.KaizenTools.Types
  ( KaizenReportArgs(..)
  , KaizenReportResult(..)
  )

-- | Marker type for the KaizenReport tool.
data KaizenReport = KaizenReport

instance ToolSpec KaizenReport where
  type Args KaizenReport = KaizenReportArgs
  type Result KaizenReport = KaizenReportResult
  toolName = "kaizen_report"
  toolDescription = "File a UX friction report, bug, or improvement idea for the framework."
