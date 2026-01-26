{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Teaching.Teacher
  ( FineTrainingTeacher(..)
  , baseSystemPrompt
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Typeclass for effects that can be taught via Haiku
--
-- Each effect that supports teaching mode must provide domain-specific guidance
-- that helps Haiku understand how to perform the task. This guidance is appended
-- to the base system prompt.
--
-- Usage:
-- @
-- instance FineTrainingTeacher MyEffect where
--   teacherGuidance = "When analyzing code, focus on..."
-- @
--
-- Use TypeApplications to get guidance:
-- @
-- let guidance = teacherGuidance @MyEffect
-- @
class FineTrainingTeacher effect where
  -- | Domain-specific guidance for teaching this effect
  --
  -- This text is appended to 'baseSystemPrompt' when calling Haiku.
  -- It should explain:
  -- - The task's purpose and context
  -- - What makes a good vs bad output
  -- - Domain-specific heuristics or principles
  -- - What to prioritize or avoid
  teacherGuidance :: Text

-- | Base system prompt for all teaching sessions
--
-- This establishes Haiku's role as a semantic code analysis assistant.
-- Effect-specific guidance is appended to this via 'FineTrainingTeacher'.
baseSystemPrompt :: Text
baseSystemPrompt = T.unlines
  [ "You are a semantic code analysis assistant."
  , ""
  , "Your role is to help understand codebases by analyzing:"
  , "- Type relationships and dependencies"
  , "- Architectural patterns and boundaries"
  , "- Impact of potential changes"
  , "- Semantic connections between components"
  , ""
  , "When using tools, explain your reasoning step-by-step before invoking them."
  , "Focus on semantic relationships, not surface-level text matching."
  ]
