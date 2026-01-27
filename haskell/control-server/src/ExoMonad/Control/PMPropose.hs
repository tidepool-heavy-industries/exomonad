{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | PM Propose tool (Tier 1) as Graph DSL node.
--
-- Enables PMs to propose new beads with intent-level details.
module ExoMonad.Control.PMPropose
  ( -- * PM Propose Graph
    PMProposeGraph(..)
  , pmProposeHandlers
  , pmProposeLogic
  , PMProposeArgs(..)
  , PMProposeResult(..)
  )
  where

import Control.Monad.Freer (Eff, Member)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Effects.GitHub
  ( GitHub
  , CreateIssueInput(..)
  , defaultCreateIssueInput
  , Issue(..)
  , Repo(..)
  , createIssue
  )
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Role (Role(..))
import ExoMonad.Control.PMTools (labelNeedsTLReview)
import ExoMonad.Control.GHTools (getRepo)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)

import ExoMonad.Control.PMPropose.Types

-- ════════════════════════════════════════════════════════════════════════════
-- PM PROPOSE GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for pm_propose tool.
data PMProposeGraph mode = PMProposeGraph
  { ppEntry :: mode :- EntryNode PMProposeArgs
      :@ MCPExport
      :@ MCPToolDef '("pm_propose", "Propose a new issue with intent-level details for TL review.")
      :@ MCPRoleHint 'PM

  , ppRun :: mode :- LogicNode
      :@ Input PMProposeArgs
      :@ UsesEffects '[GitHub, Env, Goto Exit PMProposeResult]

  , ppExit :: mode :- ExitNode PMProposeResult
  }
  deriving Generic

-- | Handlers for pm_propose graph.
pmProposeHandlers
  :: (Member GitHub es, Member Env es)
  => PMProposeGraph (AsHandler es)
pmProposeHandlers = PMProposeGraph
  { ppEntry = ()
  , ppRun = pmProposeLogic
  , ppExit = ()
  }

-- | Core logic for pm_propose.
pmProposeLogic
  :: (Member GitHub es, Member Env es)
  => PMProposeArgs
  -> Eff es (GotoChoice '[To Exit PMProposeResult])
pmProposeLogic args = do
  repo <- getRepo args.ppaRepo

  -- Construct description from intent, context, and scope hint
  let descriptionParts =
        [
          Just args.ppaIntent
        , fmap ("\n\n## Context\n" <>) args.ppaContext
        , fmap ("\n\n## Scope Hint\n" <>) args.ppaScopeHint
        ]
      fullDescription = T.concat $ catMaybes descriptionParts
      priorityLabel = "P" <> T.pack (show (fromMaybe 2 args.ppaSuggestedPriority))
      
  -- Create the issue
  let input = (defaultCreateIssueInput repo args.ppaTitle)
        { ciiBody = fullDescription
        , ciiLabels = (fromMaybe [] args.ppaSuggestedLabels) ++ [labelNeedsTLReview, priorityLabel]
        }
  
  issueNum <- createIssue input

  pure $ gotoExit $ PMProposeResult
    { pprIssueNum = issueNum
    , pprStatus = "created"
    }