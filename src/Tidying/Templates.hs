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
  ( -- * Extract prompt (new extraction-first approach)
    renderExtractPrompt
  , extractSchema

    -- * System prompts (legacy)
  , renderOrientPrompt
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

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T

import Tidying.State
import Tidying.Context (TidyingContext(..), PilesSummary(..), PhotoAnalysis(..))
import Tidying.Action
import Tidying.Output (orientOutputSchema, actOutputSchema, extractSchema)
import Tidying.Types (ItemName(..), Location(..), AnxietyTrigger(..), CategoryName(..))

-- ══════════════════════════════════════════════════════════════
-- EXTRACT PROMPT (new extraction-first approach)
-- ══════════════════════════════════════════════════════════════

-- | Render the EXTRACT system prompt
-- LLM extracts facts from user input; Haskell decides what to do
renderExtractPrompt :: Text
renderExtractPrompt = T.unlines
  [ "Extract information from user message about tidying."
  , ""
  , "Examples:"
  , "- \"desk messy\" → {\"intent\":\"start\"}"
  , "- \"my computer area\" → {\"intent\":\"start\"}"
  , "- \"old magazine\" → {\"intent\":\"item\",\"item\":\"old magazine\"}"
  , "- \"broken headphones\" → {\"intent\":\"item\",\"item\":\"broken headphones\"}"
  , "- \"trash it\" → {\"intent\":\"decided\",\"choice\":\"trash\"}"
  , "- \"throw away\" → {\"intent\":\"decided\",\"choice\":\"trash\"}"
  , "- \"keep\" → {\"intent\":\"decided\",\"choice\":\"keep\"}"
  , "- \"drawer\" → {\"intent\":\"decided\",\"choice\":\"place\",\"place\":\"drawer\"}"
  , "- \"put in closet\" → {\"intent\":\"decided\",\"choice\":\"place\",\"place\":\"closet\"}"
  , "- \"not sure\" → {\"intent\":\"help\"}"
  , "- \"idk\" → {\"intent\":\"help\"}"
  , "- \"next\" → {\"intent\":\"continue\"}"
  , "- \"done\" → {\"intent\":\"stop\"}"
  , "- \"tired\" → {\"intent\":\"stop\"}"
  , ""
  , "Rules:"
  , "- If they describe a space/area → intent=start"
  , "- If they name an object → intent=item with item name"
  , "- If they say what to do with it → intent=decided with choice"
  , "- If unsure/stuck/help → intent=help"
  , "- If ready for next/continue → intent=continue"
  , "- If done/stop/tired → intent=stop"
  , ""
  , "Output JSON only."
  ]

-- ══════════════════════════════════════════════════════════════
-- ORIENT PROMPT (legacy)
-- ══════════════════════════════════════════════════════════════

-- | Render the ORIENT system prompt (legacy - use renderExtractPrompt instead)
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
  , "Do TWO things:"
  , ""
  , "1. CLASSIFY the situation (required):"
  , "   - Overwhelm signals: \"idk\", \"too much\" → 'overwhelmed'"
  , "   - Stop signals: \"stop\", \"tired\" → 'wants_stop'"
  , "   - Item descriptions → 'item_trash/item_belongs/item_unsure'"
  , "   - General response/info → 'action_done'"
  , ""
  , "2. EXTRACT information (if present):"
  , "   - function_extracted: what the space is FOR"
  , "   - anchors_extracted: things that definitely stay"
  , "   - item_name: if they described an item"
  , ""
  , "IMPORTANT: If user provides ANY info about the space's purpose, extract it!"
  , ""
  , "Output as JSON."
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
  , "IMPORTANT: If user describes the space, EXTRACT THE FUNCTION."
  , ""
  , "Examples of function extraction:"
  , "- 'it's my computer desk' → function_extracted: 'computer work'"
  , "- 'home office for design' → function_extracted: 'design work'"
  , "- 'this is where I sleep' → function_extracted: 'sleeping'"
  , "- 'storage closet' → function_extracted: 'storage'"
  , ""
  , "When you extract function, set situation to 'action_done' (they answered)."
  , "Only use 'need_function' if they gave NO information about what the space is for."
  , ""
  , "If they seem overwhelmed ('idk', 'too much'), situation is 'overwhelmed'."
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
  , "# Available Tools"
  , ""
  , "You have access to these tools for interacting with the user:"
  , ""
  , "- propose_disposition: Propose where an item should go."
  , "  Call with item description and 2-4 ranked choices (best first)."
  , "  User taps to confirm. After confirmation, continue to next item."
  , "  Example: propose_disposition(\"old magazine\", [\"trash\", \"recycle\", \"keep\"])"
  , ""
  , "- ask_space_function: Ask what the space is FOR."
  , "  Use in surveying phase to establish context."
  , ""
  , "- confirm_done: Confirm user wants to end session."
  , "  Use when they say \"done\" or \"stop\"."
  , ""
  , "IMPORTANT: Use propose_disposition for EVERY item you identify."
  , "Don't just describe items - propose dispositions!"
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
      Just pa -> T.unlines
        [ "Photo shows: " <> pa.paChaosLevel <> " " <> pa.paRoomType
        , "Visible items: " <> T.intercalate ", " pa.paVisibleItems
        , maybe "" ("First target: " <>) pa.paFirstTarget
        ]
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
    , "Get them moving by proposing what to do with visible items."
    , case ctx.tcPhotoAnalysis of
        Just pa -> T.unlines
          [ "Photo shows items: " <> T.intercalate ", " pa.paVisibleItems
          , "Start with: " <> maybe "the most obvious item" id pa.paFirstTarget
          , ""
          , "Call propose_disposition for the first obvious item."
          , "After they confirm, continue to the next item."
          ]
        Nothing -> T.unlines
          [ "No photo - ask them what they see."
          , "Example: \"Grab one thing off the chair. What is it?\""
          ]
    ]

  InstructSplit cats -> T.unlines
    [ "# Split Instruction"
    , ""
    , "Their unsure pile is growing. Time to split it."
    , "Suggest 2-3 natural categories based on what's there."
    , ""
    , "Current unsure items suggest these categories: " <> T.intercalate ", " (map (\(CategoryName c) -> c) (NE.toList cats))
    , ""
    , "Example: \"Pause. Split your unsure pile: cables in one spot, papers in another.\""
    ]

  DecisionAid (ItemName item) -> T.unlines
    [ "# Decision Aid"
    , ""
    , "User is stuck on: " <> item
    , "Function of space: " <> maybe "unknown" id ctx.tcFunction
    , ""
    , "Reframe around function. Ask one clarifying question."
    , ""
    , "Example: \"This space is for design work. Does the sketchbook help with that?\""
    ]

  PivotAway (AnxietyTrigger trigger) (Location alt) -> T.unlines
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
