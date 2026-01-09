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

    -- * Compiled Templates
  , hTypesCompiled
  , hTypeAdversaryCompiled
  , hTypesFixCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.Context.Hybrid
  ( TypesTemplateCtx(..)
  , TypeAdversaryTemplateCtx(..)
  , TypesFixTemplateCtx(..)
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
