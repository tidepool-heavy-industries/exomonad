{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the types-first workflow graph.
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from TypesFirstDev.Context (TH staging requirement).
--
-- Note: Sequential version only includes TypesTpl. Tests/Impl templates will be
-- added when parallel agent support is implemented.
module TypesFirstDev.Templates
  ( -- * Template Types
    TypesTpl

    -- * Compiled Templates
  , typesCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Context (TypesContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- Types Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the types template at build time.
typesCompiled :: TypedTemplate TypesContext SourcePos
typesCompiled = $(typedTemplateFile ''TypesContext "templates/types.jinja")

-- | Template type marker for the types node.
data TypesTpl

instance TemplateDef TypesTpl where
  type TemplateContext TypesTpl = TypesContext
  type TemplateConstraint TypesTpl es = ()

  templateName = "types"
  templateDescription = "Write type definitions and signatures for a data structure"
  templateCompiled = typesCompiled

  -- Handler uses explicit context builder, so buildContext is not used.
  buildContext = error "TypesTpl.buildContext should not be called for ClaudeCode handlers"


