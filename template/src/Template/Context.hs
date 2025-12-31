{-# LANGUAGE OverloadedStrings #-}

-- | Context types for template rendering.
--
-- This module must be compiled BEFORE Template/Templates.hs due to TH staging.
-- The ginger TH splice validates template variables against the ToGVal instance.
module Template.Context
  ( ProcessContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import Text.Ginger.GVal (ToGVal(..), dict)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | Context for the process template.
--
-- Field names must match template variables: {{ input }}
data ProcessContext = ProcessContext
  { input :: Text  -- ^ The input text to process
  }
  deriving (Show, Eq)

-- | ToGVal instance for ginger template rendering.
instance ToGVal (Run SourcePos (Writer Text) Text) ProcessContext where
  toGVal ctx = dict
    [ ("input", toGVal ctx.input)
    ]
