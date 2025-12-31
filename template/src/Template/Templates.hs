{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the agent.
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from Template.Context (TH staging requirement).
module Template.Templates
  ( ProcessTpl
  , processCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import Template.Context (ProcessContext(..))

-- | Compile the process template at build time.
--
-- This TH splice validates that all {{ variable }} references in the template
-- match fields in ProcessContext's ToGVal instance.
processCompiled :: TypedTemplate ProcessContext SourcePos
processCompiled = $(typedTemplateFile ''ProcessContext "templates/process.jinja")

-- | Template type for the process node.
data ProcessTpl

instance TemplateDef ProcessTpl where
  type TemplateContext ProcessTpl = ProcessContext
  type TemplateConstraint ProcessTpl es = ()

  templateName = "process"
  templateDescription = "Process input and produce structured output"
  templateCompiled = processCompiled

  buildContext = error "buildContext called without handler - use LLMBefore"
