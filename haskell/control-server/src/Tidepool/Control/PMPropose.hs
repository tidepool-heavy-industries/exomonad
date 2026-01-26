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
module Tidepool.Control.PMPropose
  ( -- * PM Propose Graph
    PMProposeGraph(..)
  , pmProposeHandlers
  , pmProposeLogic
  , PMProposeArgs(..)
  , PMProposeResult(..)
  )
  where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Effects.GitHub

  ( GitHub

  , CreateIssueInput(..)

  , defaultCreateIssueInput

  , Issue(..)

  , Repo(..)

  , createIssue

  )

import Tidepool.Effects.Env (Env, getEnv)

import Tidepool.Role (Role(..))

import Tidepool.Control.PMTools (labelNeedsTLReview)

import Tidepool.Control.GHTools (getRepo)

import Tidepool.Graph.Generic (AsHandler, type (:-))

import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)

import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)



-- ════════════════════════════════════════════════════════════════════════════

-- PM PROPOSE GRAPH

-- ════════════════════════════════════════════════════════════════════════════



-- | Arguments for pm_propose tool.

data PMProposeArgs = PMProposeArgs

  { ppaTitle :: Text

  , ppaIntent :: Text

  , ppaSuggestedPriority :: Maybe Int

  , ppaSuggestedLabels :: Maybe [Text]

  , ppaContext :: Maybe Text

  , ppaScopeHint :: Maybe Text

  , ppaRepo :: Maybe Text

  }

  deriving stock (Show, Eq, Generic)



instance HasJSONSchema PMProposeArgs where

  jsonSchema = objectSchema

    [ ("title", describeField "title" "Title of the proposed issue" (emptySchema TString))

    , ("intent", describeField "intent" "The core intent or goal of this work" (emptySchema TString))

    , ("suggested_priority", describeField "suggested_priority" "Suggested priority (0-4, default 2)" (emptySchema TNumber))

    , ("suggested_labels", describeField "suggested_labels" "List of suggested labels" (arraySchema (emptySchema TString)))

    , ("context", describeField "context" "Additional context or background information" (emptySchema TString))

    , ("scope_hint", describeField "scope_hint" "Hint about the scope/size of the work" (emptySchema TString))

    , ("repo", describeField "repo" "GitHub repo" (emptySchema TString))

    ]

    ["title", "intent"]



instance FromJSON PMProposeArgs where

  parseJSON = withObject "PMProposeArgs" $ \v ->

    PMProposeArgs

      <$> v .: "title"

      <*> v .: "intent"

      <*> v .:? "suggested_priority"

      <*> v .:? "suggested_labels"

      <*> v .:? "context"

      <*> v .:? "scope_hint"

      <*> v .:? "repo"



instance ToJSON PMProposeArgs where

  toJSON args = object

    ["title" .= ppaTitle args

    , "intent" .= ppaIntent args

    , "suggested_priority" .= ppaSuggestedPriority args

    , "suggested_labels" .= ppaSuggestedLabels args

    , "context" .= ppaContext args

    , "scope_hint" .= ppaScopeHint args

    , "repo" .= ppaRepo args

    ]



-- | Result of pm_propose tool.

data PMProposeResult = PMProposeResult

  { pprIssueNum :: Int

  , pprStatus :: Text

  }

  deriving stock (Show, Eq, Generic)



instance ToJSON PMProposeResult where

  toJSON res = object

    ["issue_num" .= pprIssueNum res

    , "status" .= pprStatus res

    ]



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

        [ Just args.ppaIntent

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
