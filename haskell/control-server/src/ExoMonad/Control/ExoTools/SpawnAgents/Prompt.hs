{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.ExoTools.SpawnAgents.Prompt
  ( renderInitialPrompt
  ) where

import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)
import ExoMonad.Graph.Template (TypedTemplate, typedTemplateFile, runTypedTemplate)
import ExoMonad.Control.ExoTools.SpawnAgents.Types (InitialPromptContext)

initialPromptTpl :: TypedTemplate InitialPromptContext SourcePos
initialPromptTpl = $(typedTemplateFile ''InitialPromptContext "templates/spawn/initial-prompt.jinja")

renderInitialPrompt :: InitialPromptContext -> Text
renderInitialPrompt ctx = runTypedTemplate ctx initialPromptTpl
