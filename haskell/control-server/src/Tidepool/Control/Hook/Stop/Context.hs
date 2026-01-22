{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for Stop hook.
--
-- This module is separate from the Stop hook handler due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module Tidepool.Control.Hook.Stop.Context
  ( StopContext(..)
  , StopPRContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, (~>))
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | Context for the Stop hook template.
data StopContext = StopContext
  { bead_id :: Maybe Text
    -- ^ Bead ID if on a bd-* branch
  , branch :: Text
    -- ^ Current branch name
  , dirty_files :: [Text]
    -- ^ Files with uncommitted changes
  , commits_ahead :: Int
    -- ^ Number of commits ahead of origin/main
  , pr :: Maybe StopPRContext
    -- ^ PR info if one exists for this branch
  , clean :: Bool
    -- ^ True if no action needed (no dirty files, has PR or not on bead branch)
  } deriving stock (Show, Eq, Generic)

-- | PR context for Stop hook.
data StopPRContext = StopPRContext
  { number :: Int
  , url :: Text
  , pending_comments :: Int
    -- ^ Number of unresolved review comments
  } deriving stock (Show, Eq, Generic)

instance ToGVal (Run SourcePos (Writer Text) Text) StopContext where
  toGVal ctx = dict
    [ "bead_id" ~> bead_id ctx
    , "branch" ~> branch ctx
    , "dirty_files" ~> dirty_files ctx
    , "commits_ahead" ~> commits_ahead ctx
    , "pr" ~> pr ctx
    , "clean" ~> clean ctx
    ]

instance ToGVal (Run SourcePos (Writer Text) Text) StopPRContext where
  toGVal p = dict
    [ "number" ~> number p
    , "url" ~> url p
    , "pending_comments" ~> pending_comments p
    ]
