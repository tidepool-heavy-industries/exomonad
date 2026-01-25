{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tidepool.Control.StopHook.Templates
  ( renderStopHookTemplate
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TypedTemplate, typedTemplateFile, runTypedTemplate)
import Tidepool.Control.StopHook.Types (StopHookContext, TemplateName)

-- Compile all templates against the unified StopHookContext
-- This ensures that all templates are valid with respect to the context at compile time.

fixBuildErrorsTpl :: TypedTemplate StopHookContext SourcePos
fixBuildErrorsTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/fix-build-errors.jinja")

maxLoopsTpl :: TypedTemplate StopHookContext SourcePos
maxLoopsTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/max-loops.jinja")

buildStuckTpl :: TypedTemplate StopHookContext SourcePos
buildStuckTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/build-stuck.jinja")

fixTestFailuresTpl :: TypedTemplate StopHookContext SourcePos
fixTestFailuresTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/fix-test-failures.jinja")

testStuckTpl :: TypedTemplate StopHookContext SourcePos
testStuckTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/test-stuck.jinja")

nextStageStubTpl :: TypedTemplate StopHookContext SourcePos
nextStageStubTpl = $(typedTemplateFile ''StopHookContext "templates/stop-hook/next-stage-stub.jinja")

-- | Render a template by name using the typed context.
-- This function acts as the bridge between dynamic graph execution (TemplateName)
-- and static type safety (TypedTemplate).
renderStopHookTemplate :: TemplateName -> StopHookContext -> Text
renderStopHookTemplate name ctx = case name of
  "fix-build-errors" -> runTypedTemplate ctx fixBuildErrorsTpl
  "max-loops" -> runTypedTemplate ctx maxLoopsTpl
  "build-stuck" -> runTypedTemplate ctx buildStuckTpl
  "fix-test-failures" -> runTypedTemplate ctx fixTestFailuresTpl
  "test-stuck" -> runTypedTemplate ctx testStuckTpl
  "next-stage-stub" -> runTypedTemplate ctx nextStageStubTpl
  _ -> "Error: Unknown template '" <> name <> "'"
