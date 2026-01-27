{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.ExoTools.Status
  ( ExoStatusGraph(..)
  , exoStatusHandlers
  , exoStatusLogic
  , ExoStatusArgs(..)
  , ExoStatusResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import GHC.Generics (Generic)

import ExoMonad.Effects.Git (Git)
import ExoMonad.Effects.GitHub (GitHub)
import ExoMonad.Graph.Generic (AsHandler, type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import ExoMonad.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)

import ExoMonad.Control.ExoTools.Internal (ExoStatusResult(..), getDevelopmentContext)
import ExoMonad.Control.ExoTools.Status.Types

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for exo_status tool.
data ExoStatusGraph mode = ExoStatusGraph
  { esEntry :: mode :- EntryNode ExoStatusArgs
      :@ MCPExport
      :@ MCPToolDef '("exo_status", "Get current development context: bead details, git status, and PR info.")

  , esRun :: mode :- LogicNode
      :@ Input ExoStatusArgs
      :@ UsesEffects '[Git, GitHub, Goto Exit ExoStatusResult]

  , esExit :: mode :- ExitNode ExoStatusResult
  }
  deriving Generic

-- | Handlers for exo_status graph.
exoStatusHandlers
  :: (Member Git es, Member GitHub es)
  => ExoStatusGraph (AsHandler es)
exoStatusHandlers = ExoStatusGraph
  { esEntry = ()
  , esRun = exoStatusLogic
  , esExit = ()
  }

-- | Core logic for exo_status.
exoStatusLogic
  :: (Member Git es, Member GitHub es)
  => ExoStatusArgs
  -> Eff es (GotoChoice '[To Exit ExoStatusResult])
exoStatusLogic args = do
  result <- getDevelopmentContext args.esaBeadId
  pure $ gotoExit result
