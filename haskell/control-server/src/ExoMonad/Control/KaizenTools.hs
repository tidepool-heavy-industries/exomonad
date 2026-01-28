{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Kaizen (continuous improvement) tools logic.
module ExoMonad.Control.KaizenTools
  ( kaizenReportLogic
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)
import qualified Data.Text as T

import ExoMonad.Control.KaizenTools.Types
import ExoMonad.Control.TLTools.Types (Component(..))
import ExoMonad.Effects.GitHub
  ( GitHub, CreateIssueInput(..), createIssue, defaultCreateIssueInput, Repo(..)
  )
import ExoMonad.Effect.Types (Log, logInfo)

-- ══════════════════════════════════════════════════════════════════════════════
-- KAIZEN REPORT TOOL
-- ══════════════════════════════════════════════════════════════════════════════

-- | Target repo for kaizen issues.
kaizenRepo :: Repo
kaizenRepo = Repo "tidepool-heavy-industries/exomonad"

-- | Core logic for kaizen_report.
-- Files a real GitHub issue with structured labels for tracking.
kaizenReportLogic
  :: (Member GitHub es, Member Log es)
  => KaizenReportArgs
  -> Eff es KaizenReportResult
kaizenReportLogic args = do
  logInfo $ "Filing Kaizen report: " <> args.kraSummary

  let body = formatKaizenBody args
      labels = constructKaizenLabels args
      input = (defaultCreateIssueInput kaizenRepo args.kraSummary)
        { ciiBody   = body
        , ciiLabels = labels
        }

  res <- createIssue input
  case res of
    Left err -> pure $ KaizenReportResult
      { krrNumber  = 0
      , krrUrl     = ""
      , krrSuccess = False
      , krrError   = Just (T.pack $ show err)
      }
    Right num -> pure $ KaizenReportResult
      { krrNumber  = num
      , krrUrl     = "https://github.com/" <> kaizenRepo.unRepo <> "/issues/" <> T.pack (show num)
      , krrSuccess = True
      , krrError   = Nothing
      }

-- | Format the issue body from nested structured fields.
formatKaizenBody :: KaizenReportArgs -> Text
formatKaizenBody args = T.unlines
  [ "**Component:** " <> T.pack (show args.kraComponent)
      <> maybe "" (\s -> " / " <> s) args.kraSubcomponent
  , "**Impact:** " <> T.pack (show args.kraImpact)
  , "**Friction Type:** " <> T.pack (show args.kraFrictionType)
  , "**Estimated Effort:** " <> T.pack (show args.kraActionability.acEstimatedEffort)
  , ""
  , "## Description"
  , args.kraDescription
  , ""
  , maybe "" (\w -> "## Current Workaround\n" <> w <> "\n") args.kraActionability.acWorkaround
  , maybe "" (\s -> "## Suggested Fix\n" <> s <> "\n") args.kraActionability.acSuggestedFix
  ]

-- | Construct labels from structured fields.
constructKaizenLabels :: KaizenReportArgs -> [Text]
constructKaizenLabels args =
  [ "kaizen"
  , impactLabel args.kraImpact
  , frictionLabel args.kraFrictionType
  , componentLabel args.kraComponent
  ]

-- | Convert Impact to label.
impactLabel :: Impact -> Text
impactLabel ImpactLow    = "impact:low"
impactLabel ImpactMedium = "impact:medium"
impactLabel ImpactHigh   = "impact:high"
impactLabel Blocker      = "impact:blocker"

-- | Convert FrictionType to label.
frictionLabel :: FrictionType -> Text
frictionLabel UX            = "friction:ux"
frictionLabel Performance   = "friction:performance"
frictionLabel Documentation = "friction:docs"
frictionLabel Tooling       = "friction:tooling"
frictionLabel DX            = "friction:dx"
frictionLabel Process       = "friction:process"

-- | Convert Component to label.
componentLabel :: Component -> Text
componentLabel ControlServer = "component:control-server"
componentLabel DSL           = "component:dsl"
componentLabel TUI           = "component:tui"
componentLabel Docker        = "component:docker"
componentLabel Hooks         = "component:hooks"
componentLabel MCP           = "component:mcp"
componentLabel Zellij        = "component:zellij"
componentLabel Other         = "component:other"
