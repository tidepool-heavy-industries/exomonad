{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | SessionStart hook implementation.
--
-- Injects issue context at session start when on a gh-* branch.
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

import Control.Monad.Freer (Eff, Member, LastMember)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import ExoMonad.Effects.GitHub (GitHub, Issue(..), IssueState(..), Repo(..), Author(..), getIssue, listIssues, defaultIssueFilter, IssueFilter(..), defaultRepo)
import ExoMonad.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import ExoMonad.Effect.Types (Log, logDebug, logWarn)
import ExoMonad.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile, runTypedTemplate)
import ExoMonad.Control.ExoTools (parseIssueNumber)
import ExoMonad.Control.RoleConfig (Role(..))
import ExoMonad.Control.Hook.GitHubRetry (withRetry, defaultRetryConfig, RetryConfig(..))
import ExoMonad.Control.Hook.SessionStart.Context
import OpenTelemetry.Trace (Tracer)


-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Template marker type for session start context.
data SessionStartTpl

-- | Compiled templates (validated at compile time via TH).
sessionStartDevCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartDevCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start-dev.jinja")

sessionStartDevNoIssueCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartDevNoIssueCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start-dev-no-issue.jinja")

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
  :: (Member GitHub es, Member Git es, Member Log es, LastMember IO es)
  => Tracer
  -> Role
  -> Text  -- ^ Current working directory
  -> Eff es (Maybe Text)  -- ^ Additional context to inject
sessionStartLogic tracer role cwdPath = do
  logDebug $ "Building SessionStart context for role: " <> T.pack (show role)

  let repo = defaultRepo
      retryCfg = defaultRetryConfig { tracer = Just tracer }

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
        Just num -> withRetry retryCfg $ do
          result <- getIssue repo num False
          case result of
            Left err -> do
              logWarn $ "[GitHub] Failed to fetch issue #" <> T.pack (show num) <> ": " <> T.pack (show err)
              pure Nothing
            Right mi -> pure mi
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
                     else sessionStartDevNoIssueCompiled

      let rendered = runTypedTemplate ctx template
      pure $ Just rendered

    TL -> do
      -- Get worktree info (though TL might be on any branch)
      mWt <- getWorktreeInfo

      -- Build dashboard
      db <- buildDashboard tracer repo

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
buildDashboard :: (Member GitHub es, Member Log es, LastMember IO es) => Tracer -> Repo -> Eff es IssuesDashboardContext
buildDashboard tracer repo = do
  logDebug "Building TL dashboard..."

  let retryCfg = defaultRetryConfig { tracer = Just tracer }

  -- Fetch open issues
  openIssuesResult <- withRetry retryCfg $ 
    listIssues repo (defaultIssueFilter { ifState = Just IssueOpen, ifLimit = Just 20 })
    
  -- Let's improve error logging for buildDashboard
  openIssuesFinal <- case openIssuesResult of
    Left err -> do
      logWarn $ "[GitHub] Dashboard failed to fetch issues for " <> repo.unRepo <> ": " <> T.pack (show err)
      pure []
    Right is -> pure is

  pure IssuesDashboardContext
    { open = map toIssueContext openIssuesFinal
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
