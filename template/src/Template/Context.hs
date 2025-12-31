{-# LANGUAGE OverloadedStrings #-}

-- | Context types for template rendering.
--
-- This module must be compiled BEFORE Template/Templates.hs due to TH staging.
-- The ginger TH splice validates template variables against the ToGVal instance.
module Template.Context
  ( ProcessContext(..)
  , HistoryMessage(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import Text.Ginger.GVal (ToGVal(..), dict, list)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | A simplified message for template rendering.
--
-- This is a projection of the full Message type, containing only
-- the role and text content for easy template access.
data HistoryMessage = HistoryMessage
  { role :: Text    -- ^ "user" or "assistant"
  , content :: Text -- ^ Text content of the message
  }
  deriving (Show, Eq)

-- | ToGVal instance for history messages.
instance ToGVal (Run SourcePos (Writer Text) Text) HistoryMessage where
  toGVal msg = dict
    [ ("role", toGVal msg.role)
    , ("content", toGVal msg.content)
    ]

-- | Context for the process template.
--
-- Field names must match template variables: {{ input }}, {{ history }}
data ProcessContext = ProcessContext
  { input :: Text              -- ^ The input text to process
  , history :: [HistoryMessage] -- ^ Conversation history (simplified)
  }
  deriving (Show, Eq)

-- | ToGVal instance for ginger template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) ProcessContext where
  toGVal ctx = dict
    [ ("input", toGVal ctx.input)
    , ("history", list $ map toGVal ctx.history)
    ]
