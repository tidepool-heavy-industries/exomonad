{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the types-first workflow graph.
--
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from TypesFirstDev.Context (TH staging requirement).
module TypesFirstDev.Templates
  ( -- * Template Types
    TypesTpl
  , TestsTpl
  , ImplTpl
  , ImplSkeletonTpl
  , TestSkeletonTpl

    -- * Compiled Templates
  , typesCompiled
  , testsCompiled
  , implCompiled
  , implSkeletonCompiled
  , testSkeletonCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..), SkeletonContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- Types Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the types template at build time.
typesCompiled :: TypedTemplate TypesContext SourcePos
typesCompiled = $(typedTemplateFile ''TypesContext "templates/prompts/types.jinja")

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


-- ════════════════════════════════════════════════════════════════════════════
-- Tests Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the tests template at build time.
testsCompiled :: TypedTemplate TestsContext SourcePos
testsCompiled = $(typedTemplateFile ''TestsContext "templates/prompts/tests.jinja")

-- | Template type marker for the tests node.
data TestsTpl

instance TemplateDef TestsTpl where
  type TemplateContext TestsTpl = TestsContext
  type TemplateConstraint TestsTpl es = ()

  templateName = "tests"
  templateDescription = "Write QuickCheck property tests for the data structure"
  templateCompiled = testsCompiled

  buildContext = error "TestsTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- Impl Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the impl template at build time.
implCompiled :: TypedTemplate ImplContext SourcePos
implCompiled = $(typedTemplateFile ''ImplContext "templates/prompts/impl.jinja")

-- | Template type marker for the impl node.
data ImplTpl

instance TemplateDef ImplTpl where
  type TemplateContext ImplTpl = ImplContext
  type TemplateConstraint ImplTpl es = ()

  templateName = "impl"
  templateDescription = "Write implementations for the type signatures"
  templateCompiled = implCompiled

  buildContext = error "ImplTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- Impl Skeleton Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the impl-skeleton template at build time.
implSkeletonCompiled :: TypedTemplate SkeletonContext SourcePos
implSkeletonCompiled = $(typedTemplateFile ''SkeletonContext "templates/impl-skeleton.jinja")

-- | Template type marker for the impl skeleton node.
data ImplSkeletonTpl

instance TemplateDef ImplSkeletonTpl where
  type TemplateContext ImplSkeletonTpl = SkeletonContext
  type TemplateConstraint ImplSkeletonTpl es = ()

  templateName = "impl-skeleton"
  templateDescription = "Generate implementation skeleton with stubs"
  templateCompiled = implSkeletonCompiled

  buildContext = error "ImplSkeletonTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- Test Skeleton Template
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the test-skeleton template at build time.
testSkeletonCompiled :: TypedTemplate SkeletonContext SourcePos
testSkeletonCompiled = $(typedTemplateFile ''SkeletonContext "templates/test-skeleton.jinja")

-- | Template type marker for the test skeleton node.
data TestSkeletonTpl

instance TemplateDef TestSkeletonTpl where
  type TemplateContext TestSkeletonTpl = SkeletonContext
  type TemplateConstraint TestSkeletonTpl es = ()

  templateName = "test-skeleton"
  templateDescription = "Generate test skeleton with pending stubs"
  templateCompiled = testSkeletonCompiled

  buildContext = error "TestSkeletonTpl.buildContext should not be called for ClaudeCode handlers"
