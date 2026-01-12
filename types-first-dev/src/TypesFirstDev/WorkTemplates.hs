{-# LANGUAGE TemplateHaskell #-}

-- | Template definition for the single Work node.
--
-- Compiles the Jinja template at build time.
module TypesFirstDev.WorkTemplates
  ( -- * Template Type
    WorkTpl

    -- * Compiled Template
  , workCompiled
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TemplateDef(..), TypedTemplate, typedTemplateFile)

import TypesFirstDev.WorkContext (WorkTemplateCtx(..))

-- | Compile the Work template at build time.
workCompiled :: TypedTemplate WorkTemplateCtx SourcePos
workCompiled = $(typedTemplateFile ''WorkTemplateCtx "templates/work.jinja")

-- | Template type marker for the Work node.
data WorkTpl

instance TemplateDef WorkTpl where
  type TemplateContext WorkTpl = WorkTemplateCtx
  type TemplateConstraint WorkTpl es = ()

  templateName = "work"
  templateDescription = "Single recursive work node - scaffold, spawn, await, complete"
  templateCompiled = workCompiled
  buildContext = error "WorkTpl.buildContext should not be called"
