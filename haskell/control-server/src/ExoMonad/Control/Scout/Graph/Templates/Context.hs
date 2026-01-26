{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template context types for DocGen graph LLM nodes.
--
-- This module is separate from Templates.hs due to TH staging requirements.
-- The context types and ToGVal instances must be compiled before the
-- TH splices that reference them.
module ExoMonad.Control.Scout.Graph.Templates.Context
  ( -- * Template Context
    SelectContext(..)
  ) where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Ginger.GVal (ToGVal(..), dict)
import Text.Ginger.Run.Type (Run)
import Text.Parsec.Pos (SourcePos)


-- | Context for the symbol selection prompt.
--
-- Contains all information needed to render the @select_symbols.jinja@ template.
-- The LLM uses this to decide which type candidates are relevant to the topic.
--
-- Note: Field names match template variable names for ginger TH validation.
data SelectContext = SelectContext
  { topic :: Text
    -- ^ Topic being explored (e.g., "the scoring system")
  , symbol_name :: Text
    -- ^ Name of the symbol being analyzed
  , signature :: Text
    -- ^ Type signature from LSP hover
  , doc_comment :: Maybe Text
    -- ^ Documentation comment (if available)
  , candidates :: [Text]
    -- ^ Candidate type names extracted from signature
  } deriving stock (Show, Eq, Generic)

-- | ToGVal instance for template rendering.
--
-- Maps Haskell record fields to template variables.
instance ToGVal (Run SourcePos (Writer Text) Text) SelectContext where
  toGVal ctx = dict
    [ ("topic", toGVal ctx.topic)
    , ("symbol_name", toGVal ctx.symbol_name)
    , ("signature", toGVal ctx.signature)
    , ("doc_comment", toGVal ctx.doc_comment)
    , ("candidates", toGVal ctx.candidates)
    ]
