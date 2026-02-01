{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Codebase exploration tool.
module ExoMonad.Guest.Tools.Explorer
  ( -- * Tool types
    ExploreCodebase,
    ContinueExploration,

    -- * Argument types
    ExploreCodebaseArgs (..),
    ContinueExplorationArgs (..),
  )
where

import Control.Monad.Freer (runM)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import ExoMonad.Guest.Effects.Explore qualified as Exp
import ExoMonad.Guest.Graph qualified as Graph
import ExoMonad.Guest.Tool.Class
import GHC.Generics (Generic)

-- ============================================================================
-- ExploreCodebase
-- ============================================================================

data ExploreCodebase

data ExploreCodebaseArgs = ExploreCodebaseArgs
  { ecQuestion :: Text,
    ecSeed :: Maybe Text,
    ecMaxSteps :: Maybe Int,
    ecLanguages :: Maybe [Text],
    ecDepth :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExploreCodebaseArgs where
  parseJSON = Aeson.withObject "ExploreCodebaseArgs" $ \v ->
    ExploreCodebaseArgs
      <$> v .: "question"
      <*> v .:? "seed"
      <*> v .:? "max_steps"
      <*> v .:? "languages"
      <*> v .:? "depth"

instance MCPTool ExploreCodebase where
  type ToolArgs ExploreCodebase = ExploreCodebaseArgs
  toolName = "explore_codebase"
  toolDescription = "Iteratively explore the codebase to answer a question"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["question"] :: [Text]),
        "properties"
          .= object
            [ "question"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Question to answer" :: Text)
                  ],
              "seed"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Optional starting point (file path or symbol)" :: Text)
                  ],
              "max_steps"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Maximum exploration steps (default: 10)" :: Text)
                  ],
              "languages"
                .= object
                  [ "type" .= ("array" :: Text),
                    "items" .= object ["type" .= ("string" :: Text)],
                    "description" .= ("Languages to include (default: ['haskell'])" :: Text)
                  ],
              "depth"
                .= object
                  [ "type" .= ("string" :: Text),
                    "enum" .= (["quick", "medium", "thorough"] :: [Text]),
                    "description" .= ("Exploration depth" :: Text)
                  ]
            ]
      ]
  toolHandler args = do
    -- Simple implementation for now: do one search based on seed or question
    result <- runM $ Exp.runExplore $ do
      let startPath = maybe "." id (ecSeed args)

      -- If seed provided, try to find references
      -- This is just a basic verification that the effect works
      refs <- Exp.lspReferences startPath (Exp.Position 0 0)

      pure $ object ["summary" .= ("Exploration result stub" :: Text), "refs" .= refs]

    pure $ successResult result

-- ============================================================================
-- ContinueExploration
-- ============================================================================

data ContinueExploration

data ContinueExplorationArgs = ContinueExplorationArgs
  { ceExplorationId :: Text,
    ceFollowup :: Text,
    ceAdditionalSteps :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContinueExplorationArgs where
  parseJSON = Aeson.withObject "ContinueExplorationArgs" $ \v ->
    ContinueExplorationArgs
      <$> v .: "exploration_id"
      <*> v .: "followup"
      <*> v .:? "additional_steps"

instance MCPTool ContinueExploration where
  type ToolArgs ContinueExploration = ContinueExplorationArgs
  toolName = "continue_exploration"
  toolDescription = "Resume or extend a previous exploration"
  toolSchema =
    object
      [ "type" .= ("object" :: Text),
        "required" .= (["exploration_id", "followup"] :: [Text]),
        "properties"
          .= object
            [ "exploration_id"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("ID of previous exploration" :: Text)
                  ],
              "followup"
                .= object
                  [ "type" .= ("string" :: Text),
                    "description" .= ("Follow-up question or instruction" :: Text)
                  ],
              "additional_steps"
                .= object
                  [ "type" .= ("integer" :: Text),
                    "description" .= ("Additional steps to execute" :: Text)
                  ]
            ]
      ]
  toolHandler _args = do
    pure $ successResult $ object ["status" .= ("Not implemented yet" :: Text)]
