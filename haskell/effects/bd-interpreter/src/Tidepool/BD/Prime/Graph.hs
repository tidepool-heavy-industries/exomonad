-- | Urchin Prime Graph - Graph-based context generator.
--
-- Demonstrates the Tidepool graph DSL for native CLI tools.
-- Uses Git and BD effects for context gathering, produces markdown output.
--
-- = Architecture
--
-- @
-- Entry(()) ─► Detect ─► Gather ─► Render ─► Exit(Text)
--                │                   │
--                └───────────────────┴──► Exit(Text) [on error]
-- @
--
-- = Effects Used
--
-- * 'Git' - Worktree detection, dirty files, recent commits
-- * 'BD' - Bead listing by status/type
module Tidepool.BD.Prime.Graph
  ( -- * Graph Definition
    UrchinPrimeGraph(..)

    -- * Handler Types
  , PrimeEffects
  , primeHandlers

    -- * Context Types
  , PrimeContext(..)

    -- * Rendering
  , renderPrime
  , renderPrimeJSON
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G (EntryNode, ExitNode, LogicNode, ValidGraphRecord)
import Tidepool.Graph.Goto (Goto, gotoChoice, gotoExit)
import Tidepool.Graph.Reify (ReifyRecordGraph(..), makeGraphInfo)

import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles, getRecentCommits)
import Tidepool.Effects.BD
  ( BD
  , BeadInfo(..)
  , BeadStatus(..)
  , BeadType(..)
  , DependencyInfo(..)
  , listByStatus
  , listByType
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONTEXT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Aggregated context for urchin prime output.
--
-- Contains all data needed to render the prime context as markdown or JSON.
data PrimeContext = PrimeContext
  { pcWorktree      :: WorktreeInfo
    -- ^ Current worktree/repo info
  , pcDirtyFiles    :: [FilePath]
    -- ^ Uncommitted changes
  , pcRecentCommits :: [Text]
    -- ^ Last 5 commit subjects
  , pcInProgress    :: [BeadInfo]
    -- ^ Tasks with status=in_progress
  , pcReady         :: [BeadInfo]
    -- ^ Open, unblocked tasks
  , pcHooked        :: [BeadInfo]
    -- ^ Hooked tasks (propulsion priority)
  , pcEpicContext   :: Maybe BeadInfo
    -- ^ Epic matching worktree name (if any)
  }
  deriving (Show, Generic)

instance ToJSON PrimeContext where
  toJSON ctx = object
    [ "worktree"    .= ctx.pcWorktree
    , "git"         .= object
        [ "dirty_count"    .= length ctx.pcDirtyFiles
        , "dirty_files"    .= ctx.pcDirtyFiles
        , "recent_commits" .= ctx.pcRecentCommits
        ]
    , "in_progress" .= ctx.pcInProgress
    , "ready"       .= ctx.pcReady
    , "hooked"      .= ctx.pcHooked
    , "epic"        .= ctx.pcEpicContext
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Goto targets for Detect node.
type DetectGotos = '[Goto "upGather" WorktreeInfo, Goto Exit Text]

-- | Goto targets for Gather node.
type GatherGotos = '[Goto "upRender" PrimeContext, Goto Exit Text]

-- | Goto targets for Render node.
type RenderGotos = '[Goto Exit Text]

-- | Urchin Prime graph definition.
--
-- A simple linear graph that:
-- 1. Detects worktree info (can exit early if not in git repo)
-- 2. Gathers Git + BD context
-- 3. Renders output as markdown
data UrchinPrimeGraph mode = UrchinPrimeGraph
  { upEntry  :: mode :- G.EntryNode ()
    -- ^ Entry point, no input needed

  , upDetect :: mode :- G.LogicNode :@ Input ()
               :@ UsesEffects DetectGotos
    -- ^ Detect worktree info. Exits early with error if not in git repo.

  , upGather :: mode :- G.LogicNode :@ Input WorktreeInfo
               :@ UsesEffects GatherGotos
    -- ^ Gather full context using Git + BD effects.

  , upRender :: mode :- G.LogicNode :@ Input PrimeContext
               :@ UsesEffects RenderGotos
    -- ^ Render context to markdown.

  , upExit   :: mode :- G.ExitNode Text
    -- ^ Exit with rendered markdown (or error message).
  }
  deriving Generic


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for urchin prime handlers.
type PrimeEffects = '[Git, BD]

-- | Handlers for UrchinPrimeGraph.
--
-- Uses Git and BD effects for context gathering.
primeHandlers :: (Member Git effs, Member BD effs) => UrchinPrimeGraph (AsHandler effs)
primeHandlers = UrchinPrimeGraph
  { upEntry  = Proxy @()

  , upDetect = \() -> do
      mWorktree <- getWorktreeInfo
      pure $ case mWorktree of
        Nothing -> gotoExit @Text "Error: Not in a git repository"
        Just wt -> gotoChoice @"upGather" wt

  , upGather = \worktree -> do
      -- Gather git info
      dirtyFiles <- getDirtyFiles
      recentCommits <- getRecentCommits 5

      -- Gather BD info
      inProgress <- listByStatus StatusInProgress
      hooked <- listByStatus StatusHooked
      allOpen <- listByStatus StatusOpen

      -- Filter open to only unblocked
      let ready = filter isUnblocked allOpen

      -- Find epic matching worktree name
      epics <- listByType TypeEpic
      let epic = findMatchingEpic worktree.wiName epics

      let ctx = PrimeContext
            { pcWorktree = worktree
            , pcDirtyFiles = dirtyFiles
            , pcRecentCommits = recentCommits
            , pcInProgress = sortByPriority inProgress
            , pcReady = sortByPriority ready
            , pcHooked = sortByPriority hooked
            , pcEpicContext = epic
            }

      pure $ gotoChoice @"upRender" ctx

  , upRender = \ctx -> do
      pure $ gotoExit (renderPrime ctx)

  , upExit   = Proxy @Text
  }


-- ════════════════════════════════════════════════════════════════════════════
-- VALIDATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile-time validation that the graph is well-formed.
_validGraph :: G.ValidGraphRecord UrchinPrimeGraph => ()
_validGraph = ()

-- | Enable Mermaid diagram generation.
instance ReifyRecordGraph UrchinPrimeGraph where
  reifyRecordGraph = makeGraphInfo


-- ════════════════════════════════════════════════════════════════════════════
-- RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render context as markdown.
renderPrime :: PrimeContext -> Text
renderPrime ctx = T.unlines $ concat
  [ header
  , [""]
  , gitSection
  , [""]
  , propulsionSection
  , [""]
  , inProgressSection
  , [""]
  , readySection
  , epicSection
  ]
  where
    wt = ctx.pcWorktree

    header =
      [ "# Urchin Prime: " <> wt.wiName <> " / " <> wt.wiBranch
      ]

    gitSection =
      [ "## Git Status"
      , "- **Branch**: " <> wt.wiBranch
      , "- **Dirty files**: " <> T.pack (show $ length ctx.pcDirtyFiles)
      ] ++
      case ctx.pcRecentCommits of
        [] -> []
        (recent:_) -> ["- **Recent**: " <> recent]

    propulsionSection =
      if null ctx.pcHooked
        then
          [ "## Propulsion"
          , ""
          , "No hooked work. Review **In Progress** or **Ready** below."
          ]
        else
          [ "## Propulsion"
          , ""
          , "**HOOKED WORK** - proceed immediately:"
          ] ++ map renderBead ctx.pcHooked

    inProgressSection =
      [ "## In Progress (" <> T.pack (show $ length ctx.pcInProgress) <> ")"
      ] ++
      if null ctx.pcInProgress
        then ["_None_"]
        else map renderBead ctx.pcInProgress

    readySection =
      [ "## Ready (" <> T.pack (show $ length ctx.pcReady) <> ")"
      ] ++
      if null ctx.pcReady
        then ["_None_"]
        else map renderBead ctx.pcReady

    epicSection =
      case ctx.pcEpicContext of
        Nothing -> []
        Just epic ->
          [ ""
          , "## Epic Context"
          , ""
          , "**" <> epic.biId <> "**: " <> epic.biTitle
          ]

    renderBead :: BeadInfo -> Text
    renderBead b =
      "- [" <> b.biId <> "] " <> b.biTitle <> " (P" <> T.pack (show b.biPriority) <> ")"


-- | Render context as JSON.
renderPrimeJSON :: PrimeContext -> Text
renderPrimeJSON = TE.decodeUtf8 . LBS.toStrict . encode


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if a bead is unblocked (no open dependencies).
isUnblocked :: BeadInfo -> Bool
isUnblocked bead =
  all depIsClosed bead.biDependencies
  where
    depIsClosed (DependencyInfo { diStatus = s }) = s == StatusClosed


-- | Find epic matching worktree name.
--
-- Matches by:
-- 1. Title contains worktree name
-- 2. ID contains worktree name
findMatchingEpic :: Text -> [BeadInfo] -> Maybe BeadInfo
findMatchingEpic wtName epics =
  listToMaybe $ filter matches epics
  where
    matches epic =
      T.toLower wtName `T.isInfixOf` T.toLower epic.biTitle ||
      T.toLower wtName `T.isInfixOf` T.toLower epic.biId


-- | Sort beads by priority (lower = higher priority).
sortByPriority :: [BeadInfo] -> [BeadInfo]
sortByPriority = sortOn (.biPriority)
