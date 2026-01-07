{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for the ClaudeCode test graph.
--
-- This module is separate from Templates.hs so that TH splices can reference
-- these types (TH staging requires types to be in previously compiled modules).
--
-- Context types map to Jinja template variables:
--   ExploreContext fields → {{ directory }}, {{ objective }}
--   ActionContext fields → {{ summary }}, {{ files }}, etc.
module ClaudeCodeTest.Context
  ( ExploreContext(..)
  , ActionContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)


-- | Context for the explore template.
--
-- Field names correspond to template variables:
--   {{ directory }} - path to explore
--   {{ objective }} - what to look for
data ExploreContext = ExploreContext
  { directory :: Text
    -- ^ Directory path to explore
  , objective :: Text
    -- ^ What to look for / analyze
  }
  deriving (Show, Eq, Generic)

-- | ToGVal instance for ginger template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) ExploreContext where
  toGVal ctx = dict
    [ ("directory", toGVal ctx.directory)
    , ("objective", toGVal ctx.objective)
    ]


-- | Context for the action template.
--
-- Field names correspond to template variables:
--   {{ summary }} - what was found
--   {{ file_count }} - number of files
--   {{ files }} - list of file paths
--   {{ recommendation }} - suggested action
--   {{ action }} - what to do
data ActionContext = ActionContext
  { summary :: Text
    -- ^ Summary from exploration phase
  , fileCount :: Int
    -- ^ Number of files found
  , files :: [Text]
    -- ^ List of file paths
  , recommendation :: Text
    -- ^ Recommendation from exploration
  , action :: Text
    -- ^ Action to take
  }
  deriving (Show, Eq, Generic)

-- | ToGVal instance for ginger template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) ActionContext where
  toGVal ctx = dict
    [ ("summary", toGVal ctx.summary)
    , ("fileCount", toGVal ctx.fileCount)
    , ("files", list $ map toGVal ctx.files)
    , ("recommendation", toGVal ctx.recommendation)
    , ("action", toGVal ctx.action)
    ]
