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
import ExoMonad.Control.Combinators (withEffect)

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
  logInfo $ "Filing Kaizen report: " <> args.summary

  let body = formatKaizenBody args
      labels = constructKaizenLabels args
      input = (defaultCreateIssueInput kaizenRepo args.summary)
        { ciiBody   = body
        , ciiLabels = labels
        }

  withEffect (createIssue input)
    (\num -> pure $ KaizenReportResult
      { number  = num
      , url     = "https://github.com/" <> kaizenRepo.unRepo <> "/issues/" <> T.pack (show num)
      , success = True
      , error   = Nothing
      })
    (\err -> pure $ KaizenReportResult
      { number  = 0
      , url     = ""
      , success = False
      , error   = Just (T.pack $ show err)
      })

-- | Format the issue body from nested structured fields.
formatKaizenBody :: KaizenReportArgs -> Text
formatKaizenBody args = T.unlines
  [ "**Component:** " <> T.pack (show args.component)
      <> maybe "" (\s -> " / " <> s) args.subcomponent
  , "**Impact:** " <> T.pack (show args.impact)
  , "**Friction Type:** " <> T.pack (show args.frictionType)
  , "**Estimated Effort:** " <> T.pack (show args.actionability.estimatedEffort)
  , ""
  , "## Description"
  , args.description
  , ""
  , maybe "" (\w -> "## Current Workaround\n" <> w <> "\n") args.actionability.workaround
  , maybe "" (\s -> "## Suggested Fix\n" <> s <> "\n") args.actionability.suggestedFix
  ]

-- | Construct labels from structured fields.
constructKaizenLabels :: KaizenReportArgs -> [Text]
constructKaizenLabels args =
  [ "kaizen"
  , impactLabel args.impact
  , frictionLabel args.frictionType
  , componentLabel args.component
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
