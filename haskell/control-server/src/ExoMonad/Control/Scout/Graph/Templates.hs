{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Template definitions for DocGen graph LLM nodes.
--
-- This module defines the typed Jinja templates used by the DocGen graph.
-- The main template is @SelectTpl@ for the @dgSelect@ node which asks the
-- LLM to select relevant type dependencies from candidates.
--
-- = Template Context
--
-- The @SelectContext@ type (defined in Templates.Context) provides the data
-- rendered into the template:
--
-- @
-- SelectContext
--   { scTopic = "the scoring system"
--   , scSymbolName = "compositeScore"
--   , scSignature = "ScoreConfig -> ScoreEdgeOutput -> Double"
--   , scDocComment = Just "Calculate composite score from config and edge output."
--   , scCandidates = ["ScoreConfig", "ScoreEdgeOutput", "Double"]
--   }
-- @
--
-- = Usage
--
-- The template is used in the dgSelect LLM node:
--
-- @
-- dgSelect :: mode :- LLMNode 'API
--     :@ Input SelectInput
--     :@ Template SelectTpl
--     :@ Schema SelectOutput
-- @
module ExoMonad.Control.Scout.Graph.Templates
  ( -- * Template Context (re-export)
    SelectContext(..)

    -- * Template Definition
  , SelectTpl
  ) where

import Text.Parsec.Pos (SourcePos)

import ExoMonad.Graph.Template
  ( TemplateDef(..)
  , TypedTemplate
  , typedTemplateFile
  )

-- Import context from separate module (TH staging requirement)
import ExoMonad.Control.Scout.Graph.Templates.Context (SelectContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TEMPLATE DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Template marker type for symbol selection.
data SelectTpl

-- | Compiled template (validated at compile time via TH).
selectCompiled :: TypedTemplate SelectContext SourcePos
selectCompiled = $(typedTemplateFile ''SelectContext "templates/scout/select_symbols.jinja")

instance TemplateDef SelectTpl where
  type TemplateContext SelectTpl = SelectContext
  type TemplateConstraint SelectTpl es = ()

  templateName = "select_symbols"
  templateDescription = "Select relevant type dependencies from candidates"
  templateCompiled = selectCompiled
  buildContext = error "SelectTpl: Use handler's before function to build context"
