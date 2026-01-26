{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for PR body rendering.
--
-- This module is separate from FilePR.hs due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module ExoMonad.Control.ExoTools.FilePR.Context
  ( PRBodyContext(..)
  , PRDepContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | Context for PR body template.
data PRBodyContext = PRBodyContext
  { issue_number :: Text
  , description :: Maybe Text
  , testing :: Text
  , compromises :: Maybe Text
  , dependencies :: [PRDepContext]
  , dependents :: [PRDepContext]
  } deriving stock (Show, Eq, Generic)

-- | Dependency context for PR body.
data PRDepContext = PRDepContext
  { id :: Text
  , title :: Text
  } deriving stock (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) PRBodyContext where
  toGVal ctx = dict
    [ "issue_number" ~> issue_number ctx
    , "description" ~> description ctx
    , "testing" ~> testing ctx
    , "compromises" ~> compromises ctx
    , "dependencies" ~> dependencies ctx
    , "dependents" ~> dependents ctx
    ]

instance ToGVal (Run SourcePos (Writer Text) Text) PRDepContext where
  toGVal dep = dict
    [ "id" ~> dep.id
    , "title" ~> dep.title
    ]
