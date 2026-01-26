{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Template context and rendering for FunctionGemma scoring.
--
-- Follows the Turn 1-5 format from training-generator:
--   Turn 1: Function declaration (handled by interpreter)
--   Turn 2: User request with edge context
--   Turn 3: Model response (rubric)
module ExoMonad.Control.Scout.Templates
  ( -- * Scoring Context
    ScoringContext(..)
  , mkScoringContext

    -- * Template Rendering
  , renderScoringPrompt
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import ExoMonad.Control.Scout.EdgeTypes
  (EdgeContext(..), EdgeType(..), edgeTypeToText)


-- | Context for the FunctionGemma scoring template.
--
-- Contains all information needed to render the Turn 2 prompt.
-- Type-specific fields are populated based on EdgeType.
data ScoringContext = ScoringContext
  { scQuery      :: Text
    -- ^ Natural language query for relevance scoring
  , scEdgeType   :: Text
    -- ^ Edge type as human-readable text
  , scLocation   :: Text
    -- ^ File:line location
  , scHover      :: Text
    -- ^ LSP hover info (type signature, docs)
  , scSnippet    :: Text
    -- ^ Code context (5-10 surrounding lines)
  , scDepth      :: Int
    -- ^ Exploration depth (0 = entry point)
  -- Type-specific context (optional)
  , scTypeInfo   :: Maybe Text
    -- ^ For type refs: the type name
  , scPatterns   :: Maybe [Text]
    -- ^ For pattern matches: the cases
  , scConstraints :: Maybe [Text]
    -- ^ For instances: the constraints
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create scoring context from query and edge context.
mkScoringContext :: Text -> EdgeContext -> ScoringContext
mkScoringContext query edge = ScoringContext
  { scQuery       = query
  , scEdgeType    = edgeTypeToText (ecEdgeType edge)
  , scLocation    = ecLocation edge
  , scHover       = ecHover edge
  , scSnippet     = ecSnippet edge
  , scDepth       = ecDepth edge
  , scTypeInfo    = ecTypeName edge
  , scPatterns    = ecPatterns edge
  , scConstraints = ecConstraints edge
  }


-- | Render the Turn 2 prompt for FunctionGemma.
--
-- This is the user request that asks the model to rate the edge.
-- Turn 1 (function declaration) is prepended by the interpreter.
renderScoringPrompt :: ScoringContext -> Text
renderScoringPrompt ctx = T.unlines
  [ "Rate this " <> scEdgeType ctx <> " for the query:"
  , "<escape>" <> scQuery ctx <> "<escape>"
  , ""
  , "Location: " <> scLocation ctx
  , "Depth: " <> T.pack (show $ scDepth ctx)
  , ""
  , "Code:"
  , "<escape>"
  , scSnippet ctx
  , "<escape>"
  , ""
  , "Hover info:"
  , "<escape>" <> scHover ctx <> "<escape>"
  , ""
  , renderTypeInfo ctx
  , renderPatterns ctx
  , renderConstraints ctx
  , "Assess relevance (1-5), risk (1-5), and applicable tags."
  ]

-- | Render type info section if present.
renderTypeInfo :: ScoringContext -> Text
renderTypeInfo ctx = case scTypeInfo ctx of
  Nothing -> ""
  Just t  -> "Type context: " <> t <> "\n"

-- | Render patterns section if present.
renderPatterns :: ScoringContext -> Text
renderPatterns ctx = case scPatterns ctx of
  Nothing -> ""
  Just ps -> "Pattern cases:\n" <> T.unlines (map ("  - " <>) ps) <> "\n"

-- | Render constraints section if present.
renderConstraints :: ScoringContext -> Text
renderConstraints ctx = case scConstraints ctx of
  Nothing -> ""
  Just cs -> "Instance constraints: " <> T.intercalate ", " cs <> "\n"
