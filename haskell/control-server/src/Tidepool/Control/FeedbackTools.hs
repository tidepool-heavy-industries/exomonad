{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MCP tools for capturing subagent experience and feedback.
module Tidepool.Control.FeedbackTools
  ( -- * Register Feedback
    RegisterFeedbackGraph(..)
  , registerFeedbackLogic
  , RegisterFeedbackArgs(..)
  , RegisterFeedbackResult(..)
  , TokenCategoryEstimate(..)
  ) where

import Control.Monad.Freer (Eff, Member, sendM, LastMember)
import Data.Aeson (FromJSON(..), ToJSON(..), encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Tidepool.Graph.Generic (type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Estimate of token spend for a specific category.
data TokenCategoryEstimate = TokenCategoryEstimate
  { tceCategory :: Text -- ^ Category name (reading, searching, editing, testing, planning, waiting)
  , tceEstimate :: Text -- ^ Estimate: high, medium, low
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON TokenCategoryEstimate
instance ToJSON TokenCategoryEstimate

instance HasJSONSchema TokenCategoryEstimate where
  jsonSchema = objectSchema
    [ ("category", describeField "category" "Category name (reading, searching, editing, testing, planning, waiting)" (emptySchema TString))
    , ("estimate", describeField "estimate" "Estimate of token spend: high, medium, low" (emptySchema TString))
    ]
    ["category", "estimate"]

-- | Arguments for register_feedback tool.
data RegisterFeedbackArgs = RegisterFeedbackArgs
  { rfaBeadId :: Text            -- ^ The ID of the bead being worked on (e.g. tidepool-xyz)
  , rfaSuggestions :: [Text]     -- ^ Suggestions for making the task easier
  , rfaIdeas :: [Text]           -- ^ Ideas for new tools that should exist
  , rfaNits :: [Text]            -- ^ Small annoyances or points of confusion
  , rfaTokenCategories :: [TokenCategoryEstimate] -- ^ Token spend estimates by category
  , rfaOverallExperience :: Text -- ^ Overall experience: smooth, bumpy, blocked
  , rfaNotes :: Maybe Text       -- ^ Free-form notes and reflections
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RegisterFeedbackArgs
instance ToJSON RegisterFeedbackArgs

instance HasJSONSchema RegisterFeedbackArgs where
  jsonSchema = objectSchema
    [ ("bead_id", describeField "bead_id" "The ID of the bead being worked on (e.g. tidepool-xyz)" (emptySchema TString))
    , ("suggestions", describeField "suggestions" "Suggestions for making the task easier" (arraySchema $ emptySchema TString))
    , ("ideas", describeField "ideas" "Ideas for new tools that should exist" (arraySchema $ emptySchema TString))
    , ("nits", describeField "nits" "Small annoyances or points of confusion" (arraySchema $ emptySchema TString))
    , ("token_categories", describeField "token_categories" "Token spend estimates by category (reading, searching, editing, testing, planning, waiting)" (arraySchema (jsonSchema @TokenCategoryEstimate)))
    , ("overall_experience", describeField "overall_experience" "Overall experience: smooth, bumpy, blocked" (emptySchema TString))
    , ("notes", describeField "notes" "Free-form notes and reflections" (emptySchema TString))
    ]
    ["bead_id", "suggestions", "ideas", "nits", "token_categories", "overall_experience"]

-- | Result of register_feedback tool.
data RegisterFeedbackResult = RegisterFeedbackResult
  { rfrSuccess :: Bool
  , rfrPath :: Text
  , rfrError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON RegisterFeedbackResult

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Graph definition for register_feedback tool.
data RegisterFeedbackGraph mode = RegisterFeedbackGraph
  { rfEntry :: mode :- EntryNode RegisterFeedbackArgs
      :@ MCPExport
      :@ MCPToolDef '("register_feedback", "Capture subagent experience and suggestions for system improvement. Before calling this, review your session: What tools did you use most? What was missing? What friction did you encounter?")

  , rfRun :: mode :- LogicNode
      :@ Input RegisterFeedbackArgs
      :@ UsesEffects '[Goto Exit RegisterFeedbackResult]

  , rfExit :: mode :- ExitNode RegisterFeedbackResult
  }
  deriving Generic

-- ════════════════════════════════════════════════════════════════════════════
-- LOGIC
-- ════════════════════════════════════════════════════════════════════════════

-- | Core logic for register_feedback.
--
-- Saves the feedback to .tidepool/feedback/<bead_id>.json.
registerFeedbackLogic
  :: LastMember IO es
  => RegisterFeedbackArgs
  -> Eff es (GotoChoice '[To Exit RegisterFeedbackResult])
registerFeedbackLogic args = do
  let relativePath = ".tidepool" </> "feedback" </> T.unpack args.rfaBeadId <> ".json"
  
  -- Perform IO to write the file
  result <- sendM $ do
    createDirectoryIfMissing True (takeDirectory relativePath)
    LBS.writeFile relativePath (encode args)
    pure relativePath

  pure $ gotoExit RegisterFeedbackResult 
    { rfrSuccess = True 
    , rfrPath = T.pack result
    , rfrError = Nothing
    }