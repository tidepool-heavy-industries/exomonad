{-# LANGUAGE TemplateHaskell #-}

-- | Jinja template declarations
module HumanDrivenDev.MyTaskTemplates
  ( MyTaskTpl
  , myTaskTemplate
  ) where

import Text.Parsec.Pos (SourcePos)

import Tidepool.Graph.Template (TypedTemplate, typedTemplateFile)

import HumanDrivenDev.MyTaskContext (MyTaskContext)

-- | Compiled template with type checking
myTaskTemplate :: TypedTemplate MyTaskContext SourcePos
myTaskTemplate = $(typedTemplateFile ''MyTaskContext "templates/my_task.jinja")

-- | Template type marker for MyTask node
data MyTaskTpl
