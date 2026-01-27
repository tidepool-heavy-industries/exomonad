{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.GHTools.Types
  ( -- * List Tool
    GHIssueListArgs(..)
  , GHIssueListResult(..)

    -- * Show Tool
  , GHIssueShowArgs(..)
  , GHIssueShowResult(..)

    -- * Create Tool
  , GHIssueCreateArgs(..)
  , GHIssueCreateResult(..)

    -- * Update Tool
  , GHIssueUpdateArgs(..)
  , GHIssueUpdateResult(..)

    -- * Close Tool
  , GHIssueCloseArgs(..)
  , GHIssueCloseResult(..)

    -- * Reopen Tool
  , GHIssueReopenArgs(..)
  , GHIssueReopenResult(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import ExoMonad.Effects.GitHub (Issue)

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE LIST TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_list tool.
data GHIssueListArgs = GHIssueListArgs
  { gilaRepo   :: Maybe Text   -- ^ owner/repo
  , gilaStatus :: Maybe Text   -- ^ open, closed
  , gilaLabels :: Maybe [Text] -- ^ Filter by labels
  , gilaLimit  :: Maybe Int    -- ^ Max results
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_list tool.
data GHIssueListResult = GHIssueListResult
  { gilrIssues :: [Issue]
  , gilrCount  :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueListResult where
  toJSON res = object
    [ "issues" .= gilrIssues res
    , "count"  .= gilrCount res
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE SHOW TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_show tool.
data GHIssueShowArgs = GHIssueShowArgs
  { gisaRepo   :: Maybe Text
  , gisaNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_show tool.
data GHIssueShowResult = GHIssueShowResult
  { gisrIssue :: Maybe Issue
  , gisrFound :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueShowResult where
  toJSON res = object
    [ "issue" .= gisrIssue res
    , "found" .= gisrFound res
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CREATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_create tool.
data GHIssueCreateArgs = GHIssueCreateArgs
  { gcaRepo      :: Maybe Text
  , gcaTitle     :: Text
  , gcaBody      :: Maybe Text
  , gcaLabels    :: Maybe [Text]
  , gcaAssignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_create tool.
data GHIssueCreateResult = GHIssueCreateResult
  { gcrNumber :: Int
  , gcrSuccess :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCreateResult where
  toJSON res = object
    [ "number"  .= gcrNumber res
    , "success" .= gcrSuccess res
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE UPDATE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_update tool.
data GHIssueUpdateArgs = GHIssueUpdateArgs
  { guaRepo      :: Maybe Text
  , guaNumber    :: Int
  , guaTitle     :: Maybe Text
  , guaBody      :: Maybe Text
  , guaStatus    :: Maybe Text     -- ^ open, closed
  , guaLabels    :: Maybe [Text]
  , guaAssignees :: Maybe [Text]
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_update tool.
data GHIssueUpdateResult = GHIssueUpdateResult
  { gurSuccess :: Bool
  , gurNumber  :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueUpdateResult where
  toJSON res = object
    [ "success" .= gurSuccess res
    , "number"  .= gurNumber res
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE CLOSE TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_close tool.
data GHIssueCloseArgs = GHIssueCloseArgs
  { gclaRepo   :: Maybe Text
  , gclaNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_close tool.
data GHIssueCloseResult = GHIssueCloseResult
  { gclrSuccess :: Bool
  , gclrNumber  :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueCloseResult where
  toJSON res = object
    [ "success" .= gclrSuccess res
    , "number"  .= gclrNumber res
    ]

-- ════════════════════════════════════════════════════════════════════════════
-- GH ISSUE REOPEN TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Arguments for gh_issue_reopen tool.
data GHIssueReopenArgs = GHIssueReopenArgs
  { graRepo   :: Maybe Text
  , graNumber :: Int
  }
  deriving stock (Show, Eq, Generic)

-- | Result of gh_issue_reopen tool.
data GHIssueReopenResult = GHIssueReopenResult
  { grrSuccess :: Bool
  , grrNumber  :: Int
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON GHIssueReopenResult where
  toJSON res = object
    [ "success" .= grrSuccess res
    , "number"  .= grrNumber res
    ]
