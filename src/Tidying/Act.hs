{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying Act - Stub module
--
-- This module is being deprecated as part of the Mode-based refactor.
-- The response generation will be handled by the turn loop directly,
-- using mode-specific templates.
--
-- Kept temporarily for compilation; will be removed or significantly
-- rewritten in a later phase.
module Tidying.Act
  ( -- * Stub exports (temporary)
    ActContext(..)
  ) where

import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Context for act templates (temporary stub)
--
-- Will be replaced by mode-specific context building
data ActContext = ActContext
  { acFunction      :: Maybe Text
  , acAnchors       :: [Text]
  , acPhotoAnalysis :: Maybe Text
  , acUnsurePile    :: [Text]
  , acItem          :: Maybe Text
  , acTrigger       :: Maybe Text
  , acAlternative   :: Maybe Text
  , acSplitCats     :: [Text]
  , acItemsProcessed :: Int
  , acSessionMinutes :: Int
  } deriving (Show, Generic)

instance ToJSON ActContext where
  toJSON ActContext{..} = object
    [ "function" .= acFunction
    , "anchors" .= acAnchors
    , "photo_analysis" .= acPhotoAnalysis
    , "unsure_pile" .= acUnsurePile
    , "item" .= acItem
    , "trigger" .= acTrigger
    , "alternative" .= acAlternative
    , "split_categories" .= acSplitCats
    , "items_processed" .= acItemsProcessed
    , "session_minutes" .= acSessionMinutes
    ]
