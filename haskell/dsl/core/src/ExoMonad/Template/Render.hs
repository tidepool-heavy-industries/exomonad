-- | Pure template rendering for the typed LLM DSL.
--
-- This module provides simple, pure template rendering that can be used
-- independently of the graph machinery. Templates are rendered from
-- context values directly, without requiring effects.
--
-- = Design
--
-- This is separate from graph template handling because:
--
-- 1. Graph templates require 'buildContext' (effectful context building)
-- 2. Standalone LLM calls just need pure rendering from a context value
-- 3. Templates are reusable for non-LLM purposes (GH issues, PRs, etc.)
--
-- = Usage
--
-- @
-- -- Define a context type
-- data AnalyzeCtx = AnalyzeCtx
--   { document :: Text
--   , options :: [Text]
--   }
--
-- -- Compile the template (at top level)
-- analyzeCompiled :: TypedTemplate AnalyzeCtx SourcePos
-- analyzeCompiled = $(typedTemplateFile ''AnalyzeCtx "templates/analyze.jinja")
--
-- -- Render with a context value
-- let ctx = AnalyzeCtx { document = docText, options = opts }
-- let prompt = render analyzeCompiled ctx
-- @
module ExoMonad.Template.Render
  ( -- * Pure Rendering
    render,
    renderText,

    -- * Template Compilation (re-exports from ginger)
    TypedTemplate,
    typedTemplateFile,
    runTypedTemplate,

    -- * Ginger Context Constraint
    GingerContext,
  )
where

import Control.Monad.Writer (Writer)
import Data.Text (Text)
import Text.Ginger.GVal (ToGVal)
import Text.Ginger.Run.Type (Run)
import Text.Ginger.TH (TypedTemplate, runTypedTemplate, typedTemplateFile)
import Text.Parsec.Pos (SourcePos)

-- ════════════════════════════════════════════════════════════════════════════
-- GINGER CONTEXT CONSTRAINT
-- ════════════════════════════════════════════════════════════════════════════

-- | Constraint for types that can be rendered by ginger templates.
--
-- This is the specific monad stack that ginger uses internally.
-- Types used as template context must satisfy this constraint.
type GingerContext ctx = ToGVal (Run SourcePos (Writer Text) Text) ctx

-- ════════════════════════════════════════════════════════════════════════════
-- PURE RENDERING
-- ════════════════════════════════════════════════════════════════════════════

-- | Render a template with a context value.
--
-- This is a pure function - no effects required. The template is
-- pre-compiled (via 'typedTemplateFile' TH splice), and the context
-- is the Haskell value to render.
--
-- @
-- let prompt = render myTemplate MyContext { field1 = "value", ... }
-- @
--
-- The return type is 'Text' containing the rendered template output.
render ::
  (GingerContext ctx) =>
  TypedTemplate ctx SourcePos ->
  ctx ->
  Text
render template ctx = runTypedTemplate ctx template

-- | Alias for 'render' with more explicit naming.
--
-- Use when you want to emphasize that this produces Text output.
renderText ::
  (GingerContext ctx) =>
  TypedTemplate ctx SourcePos ->
  ctx ->
  Text
renderText = render
