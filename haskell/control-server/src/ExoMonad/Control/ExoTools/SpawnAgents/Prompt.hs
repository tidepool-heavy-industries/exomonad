{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Control.ExoTools.SpawnAgents.Prompt
  ( renderInitialPrompt,
  )
where

import Data.Text (Text)
import ExoMonad.Control.ExoTools.SpawnAgents.Types (InitialPromptContext)
import ExoMonad.Graph.Template (TypedTemplate, runTypedTemplate, typedTemplateFile)
import Text.Parsec.Pos (SourcePos)

initialPromptTpl :: TypedTemplate InitialPromptContext SourcePos
initialPromptTpl = $(typedTemplateFile ''InitialPromptContext "templates/spawn/initial-prompt.jinja")

renderInitialPrompt :: InitialPromptContext -> Text
renderInitialPrompt ctx = runTypedTemplate ctx initialPromptTpl
