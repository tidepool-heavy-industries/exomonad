{-# LANGUAGE OverloadedStrings #-}

-- | Tidying Templates
--
-- Renders system prompts based on phase and context.
-- Re-exports schemas from Output module.
--
-- = Design
--
-- Unlike the DM agent which has complex mood-based templates,
-- Tidying uses a simpler approach:
--
-- 1. EXTRACT prompt: extract structured facts from user input
-- 2. ACT prompt: generate the appropriate response
--
-- Most responses are canned (don't need LLM), so the templates
-- are only used for:
-- - Photo analysis (needs vision)
-- - Extraction (needs semantic understanding)
-- - Complex actions: first instruction, decision aid, summary, etc.

module Tidying.Templates
  ( -- * Extract prompt
    renderExtractPrompt
  , extractSchema

    -- * System prompts
  , renderActPrompt
  , renderPhotoAnalysisPrompt

    -- * Schemas (re-exported)
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Tidying.State
import Tidying.Context (TidyingContext(..), PilesSummary(..), PhotoAnalysis(..))
import Tidying.Action
import Tidying.Output (actOutputSchema, extractSchema)
import Tidying.Types (ItemName(..), Location(..), AnxietyTrigger(..), CategoryName(..), chaosLevelToText)

-- ══════════════════════════════════════════════════════════════
-- EXTRACT PROMPT (new extraction-first approach)
-- ══════════════════════════════════════════════════════════════

-- | Render the EXTRACT system prompt
-- LLM extracts facts from user input; Haskell decides what to do
renderExtractPrompt :: Text
renderExtractPrompt = T.unlines
  [ "Extract information from user message about tidying."
  , ""
  , "# Function extraction (what space is FOR):"
  , "- \"work\" → {\"intent\":\"start\",\"function\":\"workspace\"}"
  , "- \"coding\" → {\"intent\":\"start\",\"function\":\"workspace\"}"
  , "- \"gaming\" → {\"intent\":\"start\",\"function\":\"gaming\"}"
  , "- \"work and gaming\" → {\"intent\":\"start\",\"function\":\"workspace\"}"
  , "- \"art stuff\" → {\"intent\":\"start\",\"function\":\"creative\"}"
  , "- \"sleep\" → {\"intent\":\"start\",\"function\":\"bedroom\"}"
  , "- \"storage\" → {\"intent\":\"start\",\"function\":\"storage\"}"
  , ""
  , "# Starting/describing space:"
  , "- \"desk messy\" → {\"intent\":\"start\"}"
  , "- \"my computer area\" → {\"intent\":\"start\"}"
  , ""
  , "# Item handling:"
  , "- \"old magazine\" → {\"intent\":\"item\",\"item\":\"old magazine\"}"
  , "- \"broken headphones\" → {\"intent\":\"item\",\"item\":\"broken headphones\"}"
  , "- \"trash it\" → {\"intent\":\"decided\",\"choice\":\"trash\"}"
  , "- \"keep\" → {\"intent\":\"decided\",\"choice\":\"keep\"}"
  , "- \"drawer\" → {\"intent\":\"decided\",\"choice\":\"place\",\"place\":\"drawer\"}"
  , "- \"not sure\" → {\"intent\":\"help\"}"
  , "- \"next\" → {\"intent\":\"continue\"}"
  , "- \"done\" → {\"intent\":\"stop\"}"
  , ""
  , "Rules:"
  , "- If they say what space is FOR (work, gaming, art, etc) → extract function"
  , "- If they describe a space/area → intent=start"
  , "- If they name an object → intent=item"
  , "- If they say what to do with it → intent=decided"
  , "- If done/stop/tired → intent=stop"
  , ""
  , "Output JSON only."
  ]

-- ══════════════════════════════════════════════════════════════
-- PHASE-SPECIFIC GUIDANCE (for templates)
-- ══════════════════════════════════════════════════════════════

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
  , "Function of space: " <> fromMaybe "(unknown)" ctx.tcFunction
  , "Items processed: " <> T.pack (show ctx.tcItemsProcessed)
  , case ctx.tcPhotoAnalysis of
      Nothing -> ""
      Just pa -> T.unlines
        [ "Photo shows: " <> chaosLevelToText pa.paChaosLevel <> " " <> pa.paRoomType
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
  AskFunction -> T.unlines
    [ "# Ask Space Function"
    , ""
    , "We need to know what this space is FOR."
    , ""
    , "Call ask_space_function with context-specific choices."
    , "Base the options on what they told you (desk → work/gaming/art)."
    , ""
    , "Example for 'desk messy':"
    , "  prompt: 'What do you use this desk for?'"
    , "  choices: [{label: 'Work - computer stuff', value: 'workspace'},"
    , "            {label: 'Gaming', value: 'gaming'},"
    , "            {label: 'Art/crafts', value: 'creative'}]"
    ]

  AskAnchors -> T.unlines
    [ "# Ask About Anchors"
    , ""
    , "What definitely STAYS in this space?"
    , "Ask briefly what items belong here no matter what."
    , ""
    , "Example: \"What definitely stays? The desk, the computer...\""
    ]

  FirstInstruction -> T.unlines
    [ "# First Instruction"
    , ""
    , "Get them moving by proposing what to do with visible items."
    , case ctx.tcPhotoAnalysis of
        Just pa -> T.unlines
          [ "Photo shows items: " <> T.intercalate ", " pa.paVisibleItems
          , "Start with: " <> fromMaybe "the most obvious item" pa.paFirstTarget
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
    , "Function of space: " <> fromMaybe "unknown" ctx.tcFunction
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

  AskWhatIsIt -> T.unlines
    [ "# Ask What It Is"
    , ""
    , "User picked something up. Find out what it is."
    , "Be brief and curious."
    ]

  AskWhereLive -> T.unlines
    [ "# Ask Where It Lives"
    , ""
    , "User has an item that belongs somewhere."
    , "Help them figure out where. Use propose_disposition if you have context."
    ]

  AskItemDecision item -> T.unlines
    [ "# Item Decision"
    , ""
    , "User mentioned: " <> (\(ItemName n) -> n) item
    , ""
    , "IMPORTANT: Use propose_disposition tool to offer choices."
    , "Consider any context they gave (sentimental, broken, etc)."
    , "Make the options thoughtful, not just 'trash/keep/unsure'."
    ]

  InstructTrash -> T.unlines
    [ "# Trash It"
    , ""
    , "Item is trash. Confirm briefly and keep momentum."
    , "Example: \"Toss it. What's next?\""
    ]

  InstructPlace loc -> T.unlines
    [ "# Place It"
    , ""
    , "Item goes to: " <> (\(Location l) -> l) loc
    , "Confirm the location and move on."
    ]

  InstructUnsure -> T.unlines
    [ "# Unsure Pile"
    , ""
    , "They're not sure about this one. That's fine."
    , "Direct them to set it aside and keep going."
    ]

  InstructNext -> T.unlines
    [ "# Next Item"
    , ""
    , "Keep the momentum. Ask what's next."
    , "Be brief."
    ]

  InstructBag -> T.unlines
    [ "# Bag the Trash"
    , ""
    , "Trash pile is getting big. Time to bag it."
    , "Quick directive."
    ]

  EnergyCheck -> T.unlines
    [ "# Energy Check"
    , ""
    , "Check if they want to continue or stop."
    , "No pressure either way."
    ]

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
