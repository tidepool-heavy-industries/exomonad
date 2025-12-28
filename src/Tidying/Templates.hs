{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Tidying Templates
--
-- Renders system prompts based on phase and context.
-- Re-exports schemas from Output module.
--
-- = Design
--
-- Unlike the DM agent which has complex mood-based templates,
-- Tidying uses a simpler two-step approach:
--
-- 1. ORIENT prompt: classify what situation the user is in
-- 2. ACT prompt: generate the appropriate response
--
-- Most responses are canned (don't need LLM), so the templates
-- are only used for:
-- - Photo analysis (needs vision)
-- - Orient classification (needs semantic understanding)
-- - Complex actions: first instruction, decision aid, summary, etc.

module Tidying.Templates
  ( -- * System prompts
    renderOrientPrompt
  , renderActPrompt
  , renderPhotoAnalysisPrompt

    -- * Schemas (re-exported)
  , orientOutputSchema
  , actOutputSchema

    -- * Phase-specific templates (for future TypedTemplate)
  , surveyingGuidance
  , sortingGuidance
  , splittingGuidance
  , refiningGuidance
  , decisionSupportGuidance
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Tidying.State
import Tidying.Context (TidyingContext(..), PilesSummary(..), PhotoAnalysis(..))
import Tidying.Action
import Tidying.Output (orientOutputSchema, actOutputSchema)

-- ══════════════════════════════════════════════════════════════
-- ORIENT PROMPT
-- ══════════════════════════════════════════════════════════════

-- | Render the ORIENT system prompt
-- This prompt helps the LLM classify what situation the user is in
renderOrientPrompt :: TidyingContext -> Text
renderOrientPrompt ctx = T.unlines
  [ "You are classifying what situation a user is in during a tidying session."
  , ""
  , "# Current State"
  , ""
  , "Phase: " <> T.pack (show ctx.tcPhase)
  , "Function of space: " <> maybe "(not yet established)" id ctx.tcFunction
  , "Anchors (things that stay): " <> formatList ctx.tcAnchors
  , "Items processed: " <> T.pack (show ctx.tcItemsProcessed)
  , "Unsure pile size: " <> T.pack (show unsureCount)
  , case ctx.tcLastAnxiety of
      Nothing -> ""
      Just trigger -> "User was anxious about: " <> trigger <> " (avoid this topic)"
  , ""
  , phaseGuidance ctx.tcPhase
  , ""
  , "# User Input"
  , ""
  , case ctx.tcPhotoAnalysis of
      Nothing -> ""
      Just pa -> "Photo shows: " <> pa.paRoomType <> ", " <> pa.paChaosLevel <> " clutter"
                 <> maybe "" (\t -> ". First target: " <> t) pa.paFirstTarget
  , case ctx.tcUserText of
      Nothing -> "(user sent photo only)"
      Just txt -> "User says: \"" <> txt <> "\""
  , ""
  , "# Task"
  , ""
  , "Classify the situation. Look for:"
  , "- Overwhelm signals: \"idk\", \"too much\", \"where to start\""
  , "- Stop signals: \"stop\", \"tired\", \"later\""
  , "- Continue signals: \"next\", \"more\", \"keep going\""
  , "- Item descriptions: what they're holding"
  , "- Anxiety: \"freaks me out\", \"hate\", avoiding something"
  , "- Stuck/uncertain: \"not sure\", \"maybe\", hesitation"
  , ""
  , "Output your classification as JSON."
  ]
  where
    formatList [] = "(none)"
    formatList xs = T.intercalate ", " xs
    PilesSummary{psUnsureCount = unsureCount} = ctx.tcPiles

-- | Phase-specific guidance for orient
phaseGuidance :: Phase -> Text
phaseGuidance phase = case phase of
  Surveying -> surveyingGuidance
  Sorting -> sortingGuidance
  Splitting -> splittingGuidance
  Refining -> refiningGuidance
  DecisionSupport -> decisionSupportGuidance

surveyingGuidance :: Text
surveyingGuidance = T.unlines
  [ "# Surveying Phase"
  , ""
  , "We're establishing the space. Key questions:"
  , "1. What is this space FOR? (function)"
  , "2. What definitely STAYS? (anchors)"
  , ""
  , "If user hasn't told us function yet, situation is 'need_function'."
  , "If they seem overwhelmed and we have photos, situation is 'overwhelmed'."
  , "If they give us function info, extract it."
  ]

sortingGuidance :: Text
sortingGuidance = T.unlines
  [ "# Sorting Phase"
  , ""
  , "Main loop: user picks up items, we classify."
  , ""
  , "Item classifications:"
  , "- Trash/donate → 'item_trash'"
  , "- Has a clear home → 'item_belongs' (include location)"
  , "- Not sure yet → 'item_unsure'"
  , "- Need help deciding → 'item_stuck'"
  , ""
  , "Other situations:"
  , "- Just completed an action → 'action_done'"
  , "- Unsure pile growing large (5+) → 'unsure_growing'"
  , "- Main pile done → 'main_done'"
  ]

splittingGuidance :: Text
splittingGuidance = T.unlines
  [ "# Splitting Phase"
  , ""
  , "User is sorting their unsure pile into categories."
  , "Listen for what categories they're creating."
  , ""
  , "When they report progress → 'action_done'"
  ]

refiningGuidance :: Text
refiningGuidance = T.unlines
  [ "# Refining Phase"
  , ""
  , "Working through a specific sub-category."
  , "Same item classification as Sorting."
  ]

decisionSupportGuidance :: Text
decisionSupportGuidance = T.unlines
  [ "# Decision Support Phase"
  , ""
  , "Helping user decide on a stuck item."
  , "Listen for:"
  , "- They made a decision → classify item"
  , "- Still stuck → 'item_stuck'"
  , "- Want to defer → 'item_unsure'"
  ]

-- ══════════════════════════════════════════════════════════════
-- ACT PROMPT
-- ══════════════════════════════════════════════════════════════

-- | Render the ACT system prompt for actions that need LLM
-- Only called for actions that aren't canned responses
renderActPrompt :: TidyingContext -> Action -> Text
renderActPrompt ctx action = T.unlines
  [ "You are a tidying coach giving terse, directive guidance."
  , ""
  , "# Style"
  , ""
  , "- ONE instruction at a time"
  , "- Under 20 words"
  , "- Directive, not suggestive (\"Do X\" not \"Maybe try X\")"
  , "- No praise, no filler"
  , "- Goal: keep them moving"
  , ""
  , "# Context"
  , ""
  , "Function of space: " <> maybe "(unknown)" id ctx.tcFunction
  , "Items processed: " <> T.pack (show ctx.tcItemsProcessed)
  , case ctx.tcPhotoAnalysis of
      Nothing -> ""
      Just pa -> "Photo shows: " <> pa.paChaosLevel <> " " <> pa.paRoomType
  , ""
  , actionGuidance ctx action
  , ""
  , "Output your response as JSON with 'response' field."
  ]

-- | Action-specific guidance (takes context for DecisionAid)
actionGuidance :: TidyingContext -> Action -> Text
actionGuidance ctx action = case action of
  FirstInstruction -> T.unlines
    [ "# First Instruction"
    , ""
    , "Get them moving with their first action."
    , "Pick the most obvious, lowest-friction target."
    , "Usually: chair with stuff, pile on floor, surface edge."
    , ""
    , "Example: \"Grab one thing off the chair. What is it?\""
    ]

  InstructSplit cats -> T.unlines
    [ "# Split Instruction"
    , ""
    , "Their unsure pile is growing. Time to split it."
    , "Suggest 2-3 natural categories based on what's there."
    , ""
    , "Current unsure items suggest these categories: " <> T.intercalate ", " cats
    , ""
    , "Example: \"Pause. Split your unsure pile: cables in one spot, papers in another.\""
    ]

  DecisionAid item -> T.unlines
    [ "# Decision Aid"
    , ""
    , "User is stuck on: " <> item
    , "Function of space: " <> maybe "unknown" id ctx.tcFunction
    , ""
    , "Reframe around function. Ask one clarifying question."
    , ""
    , "Example: \"This space is for design work. Does the sketchbook help with that?\""
    ]

  PivotAway trigger alt -> T.unlines
    [ "# Pivot Away"
    , ""
    , "User expressed anxiety about: " <> trigger
    , "Pivot to something else: " <> alt
    , ""
    , "Acknowledge briefly, then redirect."
    , ""
    , "Example: \"Boxes stay closed for now. Let's do the desk instead.\""
    ]

  AckProgress _ -> T.unlines
    [ "# Acknowledge Progress"
    , ""
    , "Brief acknowledgment, then point to what's next."
    , "Don't overpraise. Stay factual."
    , ""
    , "Example: \"Desk clear. Let's hit the unsure pile.\""
    ]

  Summary -> T.unlines
    [ "# Session Summary"
    , ""
    , "Wrap up the session."
    , "- What's now usable"
    , "- Brief acknowledgment (not excessive)"
    , "- What's left (no pressure)"
    , ""
    , "Under 25 words. Factual."
    , ""
    , "Example: \"Chair and desk usable. Nice progress. Papers are for next time.\""
    ]

  _ -> "Respond briefly and keep momentum."

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS PROMPT
-- ══════════════════════════════════════════════════════════════

-- | Prompt for analyzing photos (vision)
-- Returns description of what's visible
renderPhotoAnalysisPrompt :: Text
renderPhotoAnalysisPrompt = T.unlines
  [ "Describe what you see in this photo of a space being tidied."
  , ""
  , "Focus on:"
  , "- What room/area is this?"
  , "- What objects/categories are visible?"
  , "- What's the chaos level? (clear / moderate / cluttered / buried)"
  , "- What's blocking function? (can't sit, can't reach, surface unusable)"
  , "- Any obvious first targets? (chair with stuff, pile on floor)"
  , ""
  , "Be factual. 2-3 sentences max."
  , ""
  , "Example: \"Home office, desk with papers and mugs, chair has clothes on it. Moderate clutter. Chair is blocking—can't sit to work.\""
  ]
