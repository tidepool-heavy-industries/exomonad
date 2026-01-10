{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the V2 tree-based TDD decomposition workflow.
--
-- Only two templates:
-- * Scaffolding - tests + stubs + rubric (the heavy lifting)
-- * Implement - glue layer or full impl (after children complete)
--
-- Key insight: Tests are part of scaffolding, not a separate phase.
-- This is TDD: write failing tests first, then implement to pass them.
module TypesFirstDev.Templates.V2
  ( -- * Template Types
    V2ScaffoldingTpl
  , V2ImplementTpl

    -- * Compiled Templates
  , v2ScaffoldingCompiled
  , v2ImplementCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Types.V2
  ( ScaffoldingCtx(..)
  , ImplementCtx(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- SCAFFOLDING TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the V2 scaffolding template at build time.
v2ScaffoldingCompiled :: TypedTemplate ScaffoldingCtx SourcePos
v2ScaffoldingCompiled = $(typedTemplateFile ''ScaffoldingCtx "templates/v2/scaffolding.jinja")

-- | Template type marker for the V2 scaffolding node.
--
-- Scaffolding does ALL of:
-- * Write TDD tests (QuickCheck properties for acceptance criteria)
-- * Write stubs (types + function signatures with undefined)
-- * Identify subsystems (facets that may become child specs)
-- * Make a commit
data V2ScaffoldingTpl

instance TemplateDef V2ScaffoldingTpl where
  type TemplateContext V2ScaffoldingTpl = ScaffoldingCtx
  type TemplateConstraint V2ScaffoldingTpl es = ()

  templateName = "v2-scaffolding"
  templateDescription = "TDD scaffolding: tests + stubs + rubric + commit"
  templateCompiled = v2ScaffoldingCompiled

  buildContext = error "V2ScaffoldingTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENT TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the V2 implement template at build time.
v2ImplementCompiled :: TypedTemplate ImplementCtx SourcePos
v2ImplementCompiled = $(typedTemplateFile ''ImplementCtx "templates/v2/implement.jinja")

-- | Template type marker for the V2 implement node.
--
-- Implementation receives the scaffolding result and optionally child results.
-- * If leaf: implements all stubs directly to pass tests
-- * If glue: implements orchestration using now-real subsystems
data V2ImplementTpl

instance TemplateDef V2ImplementTpl where
  type TemplateContext V2ImplementTpl = ImplementCtx
  type TemplateConstraint V2ImplementTpl es = ()

  templateName = "v2-implement"
  templateDescription = "Implement glue layer or full impl, iterate until CI passes"
  templateCompiled = v2ImplementCompiled

  buildContext = error "V2ImplementTpl.buildContext should not be called for ClaudeCode handlers"
