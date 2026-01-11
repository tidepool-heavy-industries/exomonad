{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for V3 graph nodes.
--
-- Compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from TypesFirstDev.V3.Context (TH staging requirement).
module TypesFirstDev.V3.Templates
  ( -- * Template Types
    ScaffoldTpl
  , TDDWriteTestsTpl
  , TDDReviewImplTpl
  , ImplTpl
  , MergerTpl
  , RebaserTpl

    -- * Compiled Templates
  , scaffoldCompiled
  , tddWriteTestsCompiled
  , tddReviewImplCompiled
  , implCompiled
  , mergerCompiled
  , rebaserCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.V3.Context
  ( ScaffoldTemplateCtx(..)
  , TDDWriteTestsTemplateCtx(..)
  , TDDReviewImplTemplateCtx(..)
  , ImplTemplateCtx(..)
  , MergerTemplateCtx(..)
  , RebaserTemplateCtx(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- SCAFFOLD TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the scaffold template at build time.
scaffoldCompiled :: TypedTemplate ScaffoldTemplateCtx SourcePos
scaffoldCompiled = $(typedTemplateFile ''ScaffoldTemplateCtx "templates/v3/scaffold.jinja")

-- | Template type marker for the scaffold node.
data ScaffoldTpl

instance TemplateDef ScaffoldTpl where
  type TemplateContext ScaffoldTpl = ScaffoldTemplateCtx
  type TemplateConstraint ScaffoldTpl es = ()

  templateName = "v3-scaffold"
  templateDescription = "Analyze spec, create interface + contract suite + test plan"
  templateCompiled = scaffoldCompiled
  buildContext = error "ScaffoldTpl.buildContext should not be called"


-- ════════════════════════════════════════════════════════════════════════════
-- TDD WRITETESTS TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the TDD write tests template at build time.
tddWriteTestsCompiled :: TypedTemplate TDDWriteTestsTemplateCtx SourcePos
tddWriteTestsCompiled = $(typedTemplateFile ''TDDWriteTestsTemplateCtx "templates/v3/tdd-write-tests.jinja")

-- | Template type marker for the TDD write tests node.
data TDDWriteTestsTpl

instance TemplateDef TDDWriteTestsTpl where
  type TemplateContext TDDWriteTestsTpl = TDDWriteTestsTemplateCtx
  type TemplateConstraint TDDWriteTestsTpl es = ()

  templateName = "v3-tdd-write-tests"
  templateDescription = "Write failing tests for all criteria in batch"
  templateCompiled = tddWriteTestsCompiled
  buildContext = error "TDDWriteTestsTpl.buildContext should not be called"


-- ════════════════════════════════════════════════════════════════════════════
-- TDD REVIEWIMPL TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the TDD review impl template at build time.
tddReviewImplCompiled :: TypedTemplate TDDReviewImplTemplateCtx SourcePos
tddReviewImplCompiled = $(typedTemplateFile ''TDDReviewImplTemplateCtx "templates/v3/tdd-review-impl.jinja")

-- | Template type marker for the TDD review impl node.
data TDDReviewImplTpl

instance TemplateDef TDDReviewImplTpl where
  type TemplateContext TDDReviewImplTpl = TDDReviewImplTemplateCtx
  type TemplateConstraint TDDReviewImplTpl es = ()

  templateName = "v3-tdd-review-impl"
  templateDescription = "Review implementation, verify tests pass correctly"
  templateCompiled = tddReviewImplCompiled
  buildContext = error "TDDReviewImplTpl.buildContext should not be called"


-- ════════════════════════════════════════════════════════════════════════════
-- IMPL TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the impl template at build time.
implCompiled :: TypedTemplate ImplTemplateCtx SourcePos
implCompiled = $(typedTemplateFile ''ImplTemplateCtx "templates/v3/impl.jinja")

-- | Template type marker for the impl node.
data ImplTpl

instance TemplateDef ImplTpl where
  type TemplateContext ImplTpl = ImplTemplateCtx
  type TemplateConstraint ImplTpl es = ()

  templateName = "v3-impl"
  templateDescription = "Make all failing tests pass"
  templateCompiled = implCompiled
  buildContext = error "ImplTpl.buildContext should not be called"


-- ════════════════════════════════════════════════════════════════════════════
-- MERGER TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the merger template at build time.
mergerCompiled :: TypedTemplate MergerTemplateCtx SourcePos
mergerCompiled = $(typedTemplateFile ''MergerTemplateCtx "templates/v3/merger.jinja")

-- | Template type marker for the merger node.
data MergerTpl

instance TemplateDef MergerTpl where
  type TemplateContext MergerTpl = MergerTemplateCtx
  type TemplateConstraint MergerTpl es = ()

  templateName = "v3-merger"
  templateDescription = "File MR to parent after TDD approval"
  templateCompiled = mergerCompiled
  buildContext = error "MergerTpl.buildContext should not be called"


-- ════════════════════════════════════════════════════════════════════════════
-- REBASER TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the rebaser template at build time.
rebaserCompiled :: TypedTemplate RebaserTemplateCtx SourcePos
rebaserCompiled = $(typedTemplateFile ''RebaserTemplateCtx "templates/v3/rebaser.jinja")

-- | Template type marker for the rebaser node.
data RebaserTpl

instance TemplateDef RebaserTpl where
  type TemplateContext RebaserTpl = RebaserTemplateCtx
  type TemplateConstraint RebaserTpl es = ()

  templateName = "v3-rebaser"
  templateDescription = "Adapt to sibling changes after their merge"
  templateCompiled = rebaserCompiled
  buildContext = error "RebaserTpl.buildContext should not be called"
