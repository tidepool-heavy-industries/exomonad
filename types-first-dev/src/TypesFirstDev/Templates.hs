{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the types-first workflow graph.
--
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from TypesFirstDev.Context (TH staging requirement).
module TypesFirstDev.Templates
  ( -- * Template Types (Pure Library)
    TypesTpl
  , TestsTpl
  , ImplTpl
  , FixTpl
  , ImplSkeletonTpl
  , TestSkeletonTpl

    -- * Template Types (Servant)
  , ServantTypesTpl
  , ServantTestsTpl
  , ServantImplTpl
  , ServantImplSkeletonTpl
  , ServantTestSkeletonTpl

    -- * Template Types (v3 Stubs Workflow)
  , ServantStubsTpl
  , ServantTestsV3Tpl

    -- * Compiled Templates (Pure Library)
  , typesCompiled
  , testsCompiled
  , implCompiled
  , fixCompiled
  , implSkeletonCompiled
  , testSkeletonCompiled

    -- * Compiled Templates (Servant)
  , servantTypesCompiled
  , servantTestsCompiled
  , servantImplCompiled
  , servantImplSkeletonCompiled
  , servantTestSkeletonCompiled

    -- * Compiled Templates (v3 Stubs Workflow)
  , servantStubsCompiled
  , servantTestsV3Compiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Context (TypesContext(..), TestsContext(..), ImplContext(..), SkeletonContext(..), StubsContext(..), TestsContextV3(..), FixContext(..))


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
-- Fix Template (TDD validation loop)
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the fix template at build time.
fixCompiled :: TypedTemplate FixContext SourcePos
fixCompiled = $(typedTemplateFile ''FixContext "templates/prompts/fix.jinja")

-- | Template type marker for the fix node.
data FixTpl

instance TemplateDef FixTpl where
  type TemplateContext FixTpl = FixContext
  type TemplateConstraint FixTpl es = ()

  templateName = "fix"
  templateDescription = "Fix implementation based on test failures"
  templateCompiled = fixCompiled

  buildContext = error "FixTpl.buildContext should not be called for ClaudeCode handlers"


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


-- ════════════════════════════════════════════════════════════════════════════
-- SERVANT TEMPLATES
-- ════════════════════════════════════════════════════════════════════════════

-- ────────────────────────────────────────────────────────────────────────────
-- Servant Types Template
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant types template at build time.
servantTypesCompiled :: TypedTemplate TypesContext SourcePos
servantTypesCompiled = $(typedTemplateFile ''TypesContext "templates/servant/prompts/types.jinja")

-- | Template type marker for the servant types node.
data ServantTypesTpl

instance TemplateDef ServantTypesTpl where
  type TemplateContext ServantTypesTpl = TypesContext
  type TemplateConstraint ServantTypesTpl es = ()

  templateName = "servant-types"
  templateDescription = "Design Servant API type and data types"
  templateCompiled = servantTypesCompiled

  buildContext = error "ServantTypesTpl.buildContext should not be called for ClaudeCode handlers"


-- ────────────────────────────────────────────────────────────────────────────
-- Servant Tests Template
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant tests template at build time.
servantTestsCompiled :: TypedTemplate TestsContext SourcePos
servantTestsCompiled = $(typedTemplateFile ''TestsContext "templates/servant/prompts/tests.jinja")

-- | Template type marker for the servant tests node.
data ServantTestsTpl

instance TemplateDef ServantTestsTpl where
  type TemplateContext ServantTestsTpl = TestsContext
  type TemplateConstraint ServantTestsTpl es = ()

  templateName = "servant-tests"
  templateDescription = "Write integration tests using servant-client"
  templateCompiled = servantTestsCompiled

  buildContext = error "ServantTestsTpl.buildContext should not be called for ClaudeCode handlers"


-- ────────────────────────────────────────────────────────────────────────────
-- Servant Impl Template
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant impl template at build time.
servantImplCompiled :: TypedTemplate ImplContext SourcePos
servantImplCompiled = $(typedTemplateFile ''ImplContext "templates/servant/prompts/impl.jinja")

-- | Template type marker for the servant impl node.
data ServantImplTpl

instance TemplateDef ServantImplTpl where
  type TemplateContext ServantImplTpl = ImplContext
  type TemplateConstraint ServantImplTpl es = ()

  templateName = "servant-impl"
  templateDescription = "Implement Servant handler functions"
  templateCompiled = servantImplCompiled

  buildContext = error "ServantImplTpl.buildContext should not be called for ClaudeCode handlers"


-- ────────────────────────────────────────────────────────────────────────────
-- Servant Impl Skeleton Template
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant impl-skeleton template at build time.
servantImplSkeletonCompiled :: TypedTemplate SkeletonContext SourcePos
servantImplSkeletonCompiled = $(typedTemplateFile ''SkeletonContext "templates/servant/impl-skeleton.jinja")

-- | Template type marker for the servant impl skeleton node.
data ServantImplSkeletonTpl

instance TemplateDef ServantImplSkeletonTpl where
  type TemplateContext ServantImplSkeletonTpl = SkeletonContext
  type TemplateConstraint ServantImplSkeletonTpl es = ()

  templateName = "servant-impl-skeleton"
  templateDescription = "Generate Servant server skeleton with undefined handlers"
  templateCompiled = servantImplSkeletonCompiled

  buildContext = error "ServantImplSkeletonTpl.buildContext should not be called for ClaudeCode handlers"


-- ────────────────────────────────────────────────────────────────────────────
-- Servant Test Skeleton Template
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant test-skeleton template at build time.
servantTestSkeletonCompiled :: TypedTemplate SkeletonContext SourcePos
servantTestSkeletonCompiled = $(typedTemplateFile ''SkeletonContext "templates/servant/test-skeleton.jinja")

-- | Template type marker for the servant test skeleton node.
data ServantTestSkeletonTpl

instance TemplateDef ServantTestSkeletonTpl where
  type TemplateContext ServantTestSkeletonTpl = SkeletonContext
  type TemplateConstraint ServantTestSkeletonTpl es = ()

  templateName = "servant-test-skeleton"
  templateDescription = "Generate Servant integration test skeleton"
  templateCompiled = servantTestSkeletonCompiled

  buildContext = error "ServantTestSkeletonTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- v3 STUBS WORKFLOW TEMPLATES
-- ════════════════════════════════════════════════════════════════════════════

-- ────────────────────────────────────────────────────────────────────────────
-- Servant Stubs Template (v3)
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant stubs template at build time.
servantStubsCompiled :: TypedTemplate StubsContext SourcePos
servantStubsCompiled = $(typedTemplateFile ''StubsContext "templates/servant/prompts/stubs.jinja")

-- | Template type marker for the servant stubs node.
data ServantStubsTpl

instance TemplateDef ServantStubsTpl where
  type TemplateContext ServantStubsTpl = StubsContext
  type TemplateConstraint ServantStubsTpl es = ()

  templateName = "servant-stubs"
  templateDescription = "Write Servant stubs with undefined handlers, return semantic descriptions"
  templateCompiled = servantStubsCompiled

  buildContext = error "ServantStubsTpl.buildContext should not be called for ClaudeCode handlers"


-- ────────────────────────────────────────────────────────────────────────────
-- Servant Tests Template v3 (driven by semantic descriptions)
-- ────────────────────────────────────────────────────────────────────────────

-- | Compile the servant tests v3 template at build time.
servantTestsV3Compiled :: TypedTemplate TestsContextV3 SourcePos
servantTestsV3Compiled = $(typedTemplateFile ''TestsContextV3 "templates/servant/prompts/tests-v3.jinja")

-- | Template type marker for the servant tests v3 node.
data ServantTestsV3Tpl

instance TemplateDef ServantTestsV3Tpl where
  type TemplateContext ServantTestsV3Tpl = TestsContextV3
  type TemplateConstraint ServantTestsV3Tpl es = ()

  templateName = "servant-tests-v3"
  templateDescription = "Write Servant integration tests from semantic descriptions"
  templateCompiled = servantTestsV3Compiled

  buildContext = error "ServantTestsV3Tpl.buildContext should not be called for ClaudeCode handlers"
