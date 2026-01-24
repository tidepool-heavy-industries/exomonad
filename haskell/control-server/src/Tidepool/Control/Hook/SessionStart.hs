{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | SessionStart hook implementation.
--
-- Injects bead context at session start when on a bd-* branch.
-- Uses typed Jinja template for consistent formatting.
module Tidepool.Control.Hook.SessionStart
  ( -- * Logic
    sessionStartLogic

    -- * Template
  , SessionStartTpl

    -- * Context (re-export)
  , SessionStartContext(..)
  , BeadContext(..)
  , DepContext(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), BeadType(..), DependencyInfo(..), getBead, listBeads, defaultListBeadsInput, ListBeadsInput(..))
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effect.Types (Log, logDebug, logInfo)
import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile, runTypedTemplate)
import Tidepool.Control.ExoTools (parseBeadId)
import Tidepool.Control.RoleConfig (Role(..))
import Tidepool.Control.Hook.SessionStart.Context


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
-- 3. Parses bead ID from branch name (bd-{id}/* pattern)
-- 4. Fetches bead info from BD
-- 5. Builds dashboard if role is TL
-- 6. Renders role-specific template with context
-- 7. Returns rendered content as additionalContext
sessionStartLogic
  :: (Member BD es, Member Git es, Member Log es)
  => Role
  -> Text  -- ^ Current working directory
  -> Eff es (Maybe Text)  -- ^ Additional context to inject
sessionStartLogic role cwdPath = do
  logDebug $ "Building SessionStart context for role: " <> T.pack (show role)

  case role of
    PM -> do
      logDebug "PM role: No SessionStart context injected."
      pure Nothing

    Dev -> do
      -- Get worktree info
      mWt <- getWorktreeInfo
      let branchName = maybe "" (.wiBranch) mWt
          maybeBeadId = parseBeadId branchName

      -- Fetch bead info if we have an ID
      mBead <- case maybeBeadId of
        Just bid -> getBead bid
        Nothing -> pure Nothing

      -- Build template context
      let ctx = SessionStartContext
            { role = Dev
            , bead_id = maybeBeadId
            , branch = (.wiBranch) <$> mWt
            , cwd = cwdPath
            , bead = toBeadContext <$> mBead
            , dashboard = Nothing
            }

      -- Select template based on bead availability
      let template = if isJust mBead
                     then sessionStartDevCompiled
                     else sessionStartDevNoBeadCompiled

      let rendered = runTypedTemplate ctx template
      pure $ Just rendered

    TL -> do
      -- Get worktree info (though TL might be on any branch)
      mWt <- getWorktreeInfo

      -- Build dashboard
      db <- buildDashboard

      -- Build template context
      let ctx = SessionStartContext
            { role = TL
            , bead_id = Nothing
            , branch = (.wiBranch) <$> mWt
            , cwd = cwdPath
            , bead = Nothing
            , dashboard = Just db
            }

      let rendered = runTypedTemplate ctx sessionStartTLCompiled
      pure $ Just rendered

-- | Build dashboard for TL role.
buildDashboard :: (Member BD es, Member Log es) => Eff es BeadsDashboardContext
buildDashboard = do
  logDebug "Building TL dashboard..."

  -- Fetch beads by status
  openBeads <- listBeads defaultListBeadsInput { lbiStatus = Just StatusOpen }
  inProgressBeads <- listBeads defaultListBeadsInput { lbiStatus = Just StatusInProgress }
  blockedBeads <- listBeads defaultListBeadsInput { lbiStatus = Just StatusBlocked }

  -- Ready beads = Open + No blockers
  let readyBeads = filter (all (== StatusClosed) . map (.diStatus) . (.biDependencies)) openBeads

  pure BeadsDashboardContext
    { ready = map toBeadContext readyBeads
    , in_progress = map toBeadContext inProgressBeads
    , blocked = map toBeadContext blockedBeads
    }


-- | Convert BeadInfo to template-friendly BeadContext.
toBeadContext :: BeadInfo -> BeadContext
toBeadContext bi = BeadContext
  { id = bi.biId
  , title = bi.biTitle
  , priority = T.pack $ show bi.biPriority
  , status = statusToText bi.biStatus
  , owner = bi.biAssignee
  , type_ = typeToText bi.biType
  , created = maybe "" (T.pack . show) bi.biCreatedAt
  , updated = maybe "" (T.pack . show) bi.biUpdatedAt
  , description = fromMaybe "(no description)" bi.biDescription
  , acceptance_criteria = Nothing -- TODO: Fetch if added to BeadInfo
  , depends_on = map toDepContext bi.biDependencies
  , blocks = map toDepContext bi.biDependents
  }

-- | Convert DependencyInfo to template-friendly DepContext.
toDepContext :: DependencyInfo -> DepContext
toDepContext di = DepContext
  { dep_id = di.diId
  , dep_title = di.diTitle
  , dep_priority = T.pack $ show di.diPriority
  }

-- | Convert BeadStatus to display text.
statusToText :: BeadStatus -> Text
statusToText = \case
  StatusOpen -> "open"
  StatusInProgress -> "in_progress"
  StatusClosed -> "closed"
  StatusHooked -> "hooked"
  StatusBlocked -> "blocked"

-- | Convert BeadType to display text.
typeToText :: BeadType -> Text
typeToText = \case
  TypeTask -> "task"
  TypeBug -> "bug"
  TypeFeature -> "feature"
  TypeEpic -> "epic"
  TypeMergeRequest -> "merge-request"
  TypeMessage -> "message"
  TypeMolecule -> "molecule"
  TypeAgent -> "agent"
  TypeOther t -> t
