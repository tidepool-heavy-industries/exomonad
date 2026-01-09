{-# LANGUAGE TemplateHaskell #-}

-- | Template definitions for the hybrid TDD workflow graph.
--
-- This module compiles Jinja templates at build time and defines TemplateDef instances.
-- Must import context types from TypesFirstDev.Context.Hybrid (TH staging requirement).
module TypesFirstDev.Templates.Hybrid
  ( -- * Template Types
    HTypesTpl
  , HTypeAdversaryTpl
  , HTypesFixTpl
  , HConflictResolveTpl
  , HFixTpl
  , HMutationAdversaryTpl

    -- * Compiled Templates
  , hTypesCompiled
  , hTypeAdversaryCompiled
  , hTypesFixCompiled
  , hConflictResolveCompiled
  , hFixCompiled
  , hMutationAdversaryCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Context.Hybrid
  ( TypesTemplateCtx(..)
  , TypeAdversaryTemplateCtx(..)
  , TypesFixTemplateCtx(..)
  , ConflictResolveTemplateCtx(..)
  , FixTemplateCtx(..)
  , MutationTemplateCtx(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the hybrid types template at build time.
hTypesCompiled :: TypedTemplate TypesTemplateCtx SourcePos
hTypesCompiled = $(typedTemplateFile ''TypesTemplateCtx "templates/hybrid/types.jinja")

-- | Template type marker for the hybrid types node.
data HTypesTpl

instance TemplateDef HTypesTpl where
  type TemplateContext HTypesTpl = TypesTemplateCtx
  type TemplateConstraint HTypesTpl es = ()

  templateName = "hybrid-types"
  templateDescription = "Design types, signatures, and function specs for TDD"
  templateCompiled = hTypesCompiled

  -- Handler uses explicit context builder, so buildContext is not used.
  buildContext = error "HTypesTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- TYPE ADVERSARY TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the type adversary template at build time.
hTypeAdversaryCompiled :: TypedTemplate TypeAdversaryTemplateCtx SourcePos
hTypeAdversaryCompiled = $(typedTemplateFile ''TypeAdversaryTemplateCtx "templates/hybrid/type-adversary.jinja")

-- | Template type marker for the type adversary node.
data HTypeAdversaryTpl

instance TemplateDef HTypeAdversaryTpl where
  type TemplateContext HTypeAdversaryTpl = TypeAdversaryTemplateCtx
  type TemplateConstraint HTypeAdversaryTpl es = ()

  templateName = "hybrid-type-adversary"
  templateDescription = "RED TEAM: Find holes in type system design"
  templateCompiled = hTypeAdversaryCompiled

  buildContext = error "HTypeAdversaryTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- TYPES FIX TEMPLATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the types fix template at build time.
hTypesFixCompiled :: TypedTemplate TypesFixTemplateCtx SourcePos
hTypesFixCompiled = $(typedTemplateFile ''TypesFixTemplateCtx "templates/hybrid/types-fix.jinja")

-- | Template type marker for the types fix node.
data HTypesFixTpl

instance TemplateDef HTypesFixTpl where
  type TemplateContext HTypesFixTpl = TypesFixTemplateCtx
  type TemplateConstraint HTypesFixTpl es = ()

  templateName = "hybrid-types-fix"
  templateDescription = "Fix type system holes found by adversary"
  templateCompiled = hTypesFixCompiled

  buildContext = error "HTypesFixTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- CONFLICT RESOLVE TEMPLATE (WS3)
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the conflict resolve template at build time.
hConflictResolveCompiled :: TypedTemplate ConflictResolveTemplateCtx SourcePos
hConflictResolveCompiled = $(typedTemplateFile ''ConflictResolveTemplateCtx "templates/hybrid/conflict-resolve.jinja")

-- | Template type marker for the conflict resolve node.
data HConflictResolveTpl

instance TemplateDef HConflictResolveTpl where
  type TemplateContext HConflictResolveTpl = ConflictResolveTemplateCtx
  type TemplateConstraint HConflictResolveTpl es = ()

  templateName = "hybrid-conflict-resolve"
  templateDescription = "Resolve git merge conflicts via LLM"
  templateCompiled = hConflictResolveCompiled

  buildContext = error "HConflictResolveTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- FIX TEMPLATE (WS4)
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the fix template at build time.
hFixCompiled :: TypedTemplate FixTemplateCtx SourcePos
hFixCompiled = $(typedTemplateFile ''FixTemplateCtx "templates/hybrid/fix.jinja")

-- | Template type marker for the fix node.
data HFixTpl

instance TemplateDef HFixTpl where
  type TemplateContext HFixTpl = FixTemplateCtx
  type TemplateConstraint HFixTpl es = ()

  templateName = "hybrid-fix"
  templateDescription = "Fix implementation based on test failures"
  templateCompiled = hFixCompiled

  buildContext = error "HFixTpl.buildContext should not be called for ClaudeCode handlers"


-- ════════════════════════════════════════════════════════════════════════════
-- MUTATION ADVERSARY TEMPLATE (WS4)
-- ════════════════════════════════════════════════════════════════════════════

-- | Compile the mutation adversary template at build time.
hMutationAdversaryCompiled :: TypedTemplate MutationTemplateCtx SourcePos
hMutationAdversaryCompiled = $(typedTemplateFile ''MutationTemplateCtx "templates/hybrid/mutation-adversary.jinja")

-- | Template type marker for the mutation adversary node.
data HMutationAdversaryTpl

instance TemplateDef HMutationAdversaryTpl where
  type TemplateContext HMutationAdversaryTpl = MutationTemplateCtx
  type TemplateConstraint HMutationAdversaryTpl es = ()

  templateName = "hybrid-mutation-adversary"
  templateDescription = "RED TEAM: Find weaknesses in test suite via mutation testing"
  templateCompiled = hMutationAdversaryCompiled

  buildContext = error "HMutationAdversaryTpl.buildContext should not be called for ClaudeCode handlers"
