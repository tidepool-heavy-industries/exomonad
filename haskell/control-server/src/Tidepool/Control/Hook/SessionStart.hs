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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), BeadType(..), DependencyInfo(..), getBead)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile, runTypedTemplate)
import Tidepool.Control.ExoTools (parseBeadId)
import Tidepool.Control.Hook.SessionStart.Context


-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Template marker type for session start context.
data SessionStartTpl

-- | Compiled template (validated at compile time via TH).
sessionStartCompiled :: TypedTemplate SessionStartContext SourcePos
sessionStartCompiled = $(typedTemplateFile ''SessionStartContext "templates/hook/session-start.jinja")

instance TemplateDef SessionStartTpl where
  type TemplateContext SessionStartTpl = SessionStartContext
  type TemplateConstraint SessionStartTpl es = ()

  templateName = "session-start"
  templateDescription = "Inject bead context at session start"
  templateCompiled = sessionStartCompiled
  buildContext = error "SessionStartTpl: Use sessionStartLogic to build context"


-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | SessionStart hook logic.
--
-- 1. Gets worktree info to determine current branch
-- 2. Parses bead ID from branch name (bd-{id}/* pattern)
-- 3. Fetches bead info from BD
-- 4. Renders template with context
-- 5. Returns rendered content as additionalContext
sessionStartLogic
  :: (Member BD es, Member Git es)
  => Text  -- ^ Current working directory
  -> Eff es (Maybe Text)  -- ^ Additional context to inject
sessionStartLogic cwdPath = do
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
        { bead_id = maybeBeadId
        , branch = (.wiBranch) <$> mWt
        , cwd = cwdPath
        , bead = toBeadContext <$> mBead
        }

  -- Render template
  let rendered = runTypedTemplate ctx sessionStartCompiled
  pure $ Just rendered


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
