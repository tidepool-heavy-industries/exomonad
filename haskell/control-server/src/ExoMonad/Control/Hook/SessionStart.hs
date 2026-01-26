{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | SessionStart hook implementation.
--
-- Injects bead context at session start when on a bd-* branch.
-- Uses typed Jinja template for consistent formatting.
module ExoMonad.Control.Hook.SessionStart
  ( -- * Logic
    sessionStartLogic

    -- * Template
  , SessionStartTpl

    -- * Context (re-export)
  , SessionStartContext(..)
  , IssueContext(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import ExoMonad.Effects.GitHub (GitHub, Issue(..), IssueState(..), Repo(..), Author(..), getIssue, listIssues, defaultIssueFilter, IssueFilter(..))
import ExoMonad.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import ExoMonad.Effect.Types (Log, logDebug, logInfo)
import ExoMonad.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile, runTypedTemplate)
import ExoMonad.Control.ExoTools (parseIssueNumber)
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Control.Hook.SessionStart.Context


-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Template marker type for session start context.
data SessionStartTpl

-- | Compiled templates (validated at compile time via TH).
sessionStartDevCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartDevCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start-dev.jinja")

sessionStartDevNoBeadCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartDevNoBeadCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start-dev-no-bead.jinja")

sessionStartTLCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartTLCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start-tl.jinja")

instance TemplateDef SessionStartTpl where
  type TemplateContext SessionStartTpl = SessionStartContext
  type TemplateConstraint SessionStartTpl es = ()

  templateName = "session-start"
  templateDescription = "Inject role-specific context at session start"
  templateCompiled = sessionStartDevCompiled -- Default, but logic uses specific ones
  buildContext = error "SessionStartTpl: Use sessionStartLogic to build context"


-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | SessionStart hook logic.
--
-- 1. Detects role
-- 2. Gets worktree info to determine current branch
-- 3. Parses issue number from branch name (gh-{id}/* pattern)
-- 4. Fetches issue info from GitHub
-- 5. Builds dashboard if role is TL
-- 6. Renders role-specific template with context
-- 7. Returns rendered content as additionalContext
sessionStartLogic
  :: (Member GitHub es, Member Git es, Member Log es)
  => Role
  -> Text  -- ^ Current working directory
  -> Eff es (Maybe Text)  -- ^ Additional context to inject
sessionStartLogic role cwdPath = do
  logDebug $ "Building SessionStart context for role: " <> T.pack (show role)

  -- TODO: Configurable repo
  let repo = Repo "exomonad/exomonad"

  case role of
    PM -> do
      logDebug "PM role: No SessionStart context injected."
      pure Nothing

    Dev -> do
      -- Get worktree info
      mWt <- getWorktreeInfo
      let branchName = maybe "" (.wiBranch) mWt
          maybeIssueNum = parseIssueNumber branchName

      -- Fetch issue info if we have a number
      mIssue <- case maybeIssueNum of
        Just num -> getIssue repo num False
        Nothing -> pure Nothing

      -- Build template context
      let ctx = SessionStartContext
            { role = Dev
            , issue_number = maybeIssueNum
            , branch = (.wiBranch) <$> mWt
            , cwd = cwdPath
            , issue = toIssueContext <$> mIssue
            , dashboard = Nothing
            }

      -- Select template based on issue availability
      let template = if isJust mIssue
                     then sessionStartDevCompiled
                     else sessionStartDevNoBeadCompiled

      let rendered = runTypedTemplate ctx template
      pure $ Just rendered

    TL -> do
      -- Get worktree info (though TL might be on any branch)
      mWt <- getWorktreeInfo

      -- Build dashboard
      db <- buildDashboard repo

      -- Build template context
      let ctx = SessionStartContext
            { role = TL
            , issue_number = Nothing
            , branch = (.wiBranch) <$> mWt
            , cwd = cwdPath
            , issue = Nothing
            , dashboard = Just db
            }

      let rendered = runTypedTemplate ctx sessionStartTLCompiled
      pure $ Just rendered

-- | Build dashboard for TL role.
buildDashboard :: (Member GitHub es, Member Log es) => Repo -> Eff es IssuesDashboardContext
buildDashboard repo = do
  logDebug "Building TL dashboard..."

  -- Fetch open issues
  openIssues <- listIssues repo (defaultIssueFilter { ifState = Just IssueOpen, ifLimit = Just 20 })

  pure IssuesDashboardContext
    { open = map toIssueContext openIssues
    }


-- | Convert Issue to template-friendly IssueContext.
toIssueContext :: Issue -> IssueContext
toIssueContext i =
  let Author login _ = i.issueAuthor
  in IssueContext
    { number = i.issueNumber
    , title = i.issueTitle
    , status = T.pack (show i.issueState)
    , author = login
    , labels = i.issueLabels
    , description = i.issueBody
    , url = i.issueUrl
    }
