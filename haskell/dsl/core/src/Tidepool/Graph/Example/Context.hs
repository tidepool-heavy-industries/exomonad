{-# LANGUAGE OverloadedStrings #-}

-- | Example context types for TemplateDef demonstrations.
--
-- This module is separate from Example.hs so that TH splices in Example.hs
-- can reference these types (TH staging requires types to be in previously
-- compiled modules).
module Tidepool.Graph.Example.Context
  ( ClassifyContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)

-- | Context type for the classify template.
--
-- This record's fields are accessible in the Jinja template as @{{ topic }}@
-- and @{{ categories }}@.
--
-- Note: Field names must match template variable names for ginger TH validation.
data ClassifyContext = ClassifyContext
  { topic :: Text       -- ^ What the message is about
  , categories :: Text  -- ^ Available classification categories
  }
  deriving (Show, Eq, Generic)

-- | ToGVal instance for ginger template rendering.
--
-- This maps Haskell record fields to template variables.
instance ToGVal (Run SourcePos (Writer Text) Text) ClassifyContext where
  toGVal ctx = dict
    [ ("topic", toGVal ctx.topic)
    , ("categories", toGVal ctx.categories)
    ]
