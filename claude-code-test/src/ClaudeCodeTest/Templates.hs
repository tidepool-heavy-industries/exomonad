{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the ClaudeCode test graph.
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from ClaudeCodeTest.Context (TH staging requirement).
module ClaudeCodeTest.Templates
  ( -- * Template Types
    ExploreTpl
  , ActionTpl

    -- * Compiled Templates
  , exploreCompiled
  , actionCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import ClaudeCodeTest.Context (ExploreContext(..), ActionContext(..))


-- ============================================================================
-- Explore Template
-- ============================================================================

-- | Compile the explore template at build time.
--
-- This TH splice validates that all {{ variable }} references in the template
-- match fields in ExploreContext's ToGVal instance.
exploreCompiled :: TypedTemplate ExploreContext SourcePos
exploreCompiled = $(typedTemplateFile ''ExploreContext "templates/explore.jinja")

-- | Template type marker for the explore node.
data ExploreTpl

instance TemplateDef ExploreTpl where
  type TemplateContext ExploreTpl = ExploreContext
  type TemplateConstraint ExploreTpl es = ()

  templateName = "explore"
  templateDescription = "Explore a directory and gather information about its contents"
  templateCompiled = exploreCompiled

  -- Handler uses explicit context builder in ClaudeCodeLLMHandler, so buildContext is not used.
  buildContext = error "ExploreTpl.buildContext should not be called for ClaudeCode handlers"


-- ============================================================================
-- Action Template
-- ============================================================================

-- | Compile the action template at build time.
actionCompiled :: TypedTemplate ActionContext SourcePos
actionCompiled = $(typedTemplateFile ''ActionContext "templates/action.jinja")

-- | Template type marker for the action node.
data ActionTpl

instance TemplateDef ActionTpl where
  type TemplateContext ActionTpl = ActionContext
  type TemplateConstraint ActionTpl es = ()

  templateName = "action"
  templateDescription = "Take a follow-up action based on exploration results"
  templateCompiled = actionCompiled

  -- Handler uses explicit context builder in ClaudeCodeLLMHandler, so buildContext is not used.
  buildContext = error "ActionTpl.buildContext should not be called for ClaudeCode handlers"
