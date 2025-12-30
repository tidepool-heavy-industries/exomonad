-- | DM Templates - compile-time type-checked Jinja templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate'

    -- * Mood-Specific Templates
  , sceneTemplate
  , actionTemplate
  , aftermathTemplate
  , traumaTemplate
  , bargainTemplate

    -- * Typed Jinja Templates
  , dmTurnJinja
  , compressionJinja
  , sceneJinja
  , actionJinja
  , aftermathJinja
  , traumaJinja
  , bargainJinja

    -- * Tool List Type
  , DMTools

    -- * Rendering
  , renderDMTurn
  , renderCompression
  , renderForMood

    -- * Schemas
  , turnOutputSchema
  , compressionOutputSchema
  , bargainSchema
  ) where

import DM.Context
import DM.Output
import DM.State (WorldState, DMMood(..))
import DM.Tools (DMEffects, DMEvent, dmToolList, SetSceneStyle, Choose, SpendDie, Engage, Resolve, Accept)
import Tidepool.Template
import Tidepool.Schema
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

-- | Type alias for the full DM tool list
-- Note: Bargain tools (AcceptBargain, Retreat, PassOut) removed - bargain uses structured output
type DMTools = '[SetSceneStyle, Choose, SpendDie, Engage, Resolve, Accept]

-- ══════════════════════════════════════════════════════════════
-- TYPED JINJA TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | DM turn Jinja template (compile-time validated) - legacy, use mood-specific
dmTurnJinja :: TypedTemplate DMContext SourcePos
dmTurnJinja = $(typedTemplateFile ''DMContext "templates/dm_turn.jinja")

-- | Scene mood template - exploration, hooks, NPC encounters
sceneJinja :: TypedTemplate DMContext SourcePos
sceneJinja = $(typedTemplateFile ''DMContext "templates/scene/main.jinja")

-- | Action mood template - dice mechanics, position/effect
actionJinja :: TypedTemplate DMContext SourcePos
actionJinja = $(typedTemplateFile ''DMContext "templates/action/main.jinja")

-- | Aftermath mood template - consequences, costs, complications
aftermathJinja :: TypedTemplate DMContext SourcePos
aftermathJinja = $(typedTemplateFile ''DMContext "templates/aftermath/main.jinja")

-- | Trauma mood template - breaking point, gaining permanent scar
traumaJinja :: TypedTemplate DMContext SourcePos
traumaJinja = $(typedTemplateFile ''DMContext "templates/trauma/main.jinja")

-- | Bargain mood template - out of dice, make a deal
bargainJinja :: TypedTemplate DMContext SourcePos
bargainJinja = $(typedTemplateFile ''DMContext "templates/bargain/main.jinja")

-- | Compression Jinja template (compile-time validated)
compressionJinja :: TypedTemplate CompressionContext SourcePos
compressionJinja = $(typedTemplateFile ''CompressionContext "templates/compression.jinja")

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | Main DM turn template with all available tools
dmTurnTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Compression template (no tools needed - uses unit state/event)
compressionTemplate' :: Template CompressionContext CompressionOutput () () '[] '[]
compressionTemplate' = Template
  { templateJinja = compressionJinja
  , templateOutputSchema = compressionOutputSchema
  , templateTools = TNil
  }

-- | Scene template - exploration state
sceneTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
sceneTemplate = Template
  { templateJinja = sceneJinja
  , templateOutputSchema = sceneSchema
  , templateTools = dmToolList
  }

-- | Action template - dice resolution state
actionTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
actionTemplate = Template
  { templateJinja = actionJinja
  , templateOutputSchema = actionSchema
  , templateTools = dmToolList
  }

-- | Aftermath template - consequence state
aftermathTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
aftermathTemplate = Template
  { templateJinja = aftermathJinja
  , templateOutputSchema = aftermathSchema
  , templateTools = dmToolList
  }

-- | Trauma template - breaking point, stress reset
traumaTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
traumaTemplate = Template
  { templateJinja = traumaJinja
  , templateOutputSchema = traumaSchema
  , templateTools = dmToolList
  }

-- | Bargain template - out of dice, make deals
-- Note: Uses BargainLLMOutput (not TurnOutput) - loop handles this separately
bargainTemplate :: Template DMContext BargainLLMOutput DMEvent WorldState DMEffects '[]
bargainTemplate = Template
  { templateJinja = bargainJinja
  , templateOutputSchema = bargainSchema
  , templateTools = TNil  -- No tools - structured output handles everything
  }

-- ══════════════════════════════════════════════════════════════
-- RENDERING
-- ══════════════════════════════════════════════════════════════

-- | Render DM turn template with context (legacy - use renderForMood)
renderDMTurn :: DMContext -> Text
renderDMTurn = render dmTurnTemplate

-- | Render compression template with context
renderCompression :: CompressionContext -> Text
renderCompression = render compressionTemplate'

-- | Render the appropriate template based on current mood
renderForMood :: DMMood -> DMContext -> Text
renderForMood mood ctx = case mood of
  MoodScene _     -> render sceneTemplate ctx
  MoodAction _ _  -> render actionTemplate ctx
  MoodAftermath _ -> render aftermathTemplate ctx
  MoodTrauma _    -> render traumaTemplate ctx
  MoodBargain _   -> render bargainTemplate ctx

-- ══════════════════════════════════════════════════════════════
-- SCHEMA WRAPPERS (typed Schema for each mood)
-- ══════════════════════════════════════════════════════════════

sceneSchema :: Schema TurnOutput
sceneSchema = Schema
  { schemaJSON = schemaToValue sceneOutputSchema
  , schemaDescription = "Scene: narration + affordances. No stress/heat."
  }

actionSchema :: Schema TurnOutput
actionSchema = Schema
  { schemaJSON = schemaToValue actionOutputSchema
  , schemaDescription = "Action: frame tension, call spend_die. Mechanics in tool."
  }

aftermathSchema :: Schema TurnOutput
aftermathSchema = Schema
  { schemaJSON = schemaToValue aftermathOutputSchema
  , schemaDescription = "Aftermath: land consequences, echo forward."
  }

traumaSchema :: Schema TurnOutput
traumaSchema = Schema
  { schemaJSON = schemaToValue traumaOutputSchema
  , schemaDescription = "Trauma: breaking point. Assign scar, reset stress."
  }

-- | Schema for bargain output (different type from TurnOutput)
-- Note: This schema is for BargainLLMOutput, not TurnOutput
-- The loop handles bargain mode separately
bargainSchema :: Schema BargainLLMOutput
bargainSchema = Schema
  { schemaJSON = schemaToValue bargainOutputJSONSchema
  , schemaDescription = "Bargain: generate 2-4 bargain options for player choice."
  }

-- | Legacy schema alias (for dmTurnTemplate backwards compat)
turnOutputSchema :: Schema TurnOutput
turnOutputSchema = sceneSchema  -- Default to scene

-- | Schema for scene compression output
compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = Schema
  { schemaJSON = schemaToValue compressionOutputJSONSchema
  , schemaDescription = "Scene compression output for persisting durable changes"
  }

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT SCHEMAS (MOOD-SPECIFIC)
-- Each mood has exactly the fields it needs, nothing more.
-- ══════════════════════════════════════════════════════════════

-- | SCENE: Presenting fiction, offering affordances
-- No stress/heat - those flow through dice
sceneOutputSchema :: JSONSchema
sceneOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "1-2 tweets (280 chars each). Prose poetry—line breaks as breath. Fragments complete. Sentences end before—"
      (emptySchema TString))
  , ("coinDelta", describeField "coinDelta"
      "Direct transactions only (bought drink, paid informant). Usually 0."
      (emptySchema TInteger))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 short next actions (3-8 words). One safe, one risky."
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "suggestedActions"]

-- | ACTION: Build tension before dice via spend_die tool
-- No diceAction in structured output - the spend_die tool handles dice
actionOutputSchema :: JSONSchema
actionOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "1 tweet (280 chars). Prose poetry—the precipice. Line breaks as held breath. Then spend_die."
      (emptySchema TString))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 next actions based on outcome"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "suggestedActions"]

-- | AFTERMATH: Consequences landed, echoing forward
-- Coin for loot/payout, descriptions for future echoing
aftermathOutputSchema :: JSONSchema
aftermathOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "1 tweet (280 chars). Prose poetry—consequence lands. Line breaks as aftermath. End on friction."
      (emptySchema TString))
  , ("coinDelta", describeField "coinDelta"
      "Loot found, job payout, stolen goods. 0 if none."
      (emptySchema TInteger))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 next actions: recover, press on, deal with fallout"
      (arraySchema (emptySchema TString)))
  , ("costDescription", describeField "costDescription"
      "If costly: describe the price paid for echoing later. Omit if clean."
      (emptySchema TString))
  , ("threatDescription", describeField "threatDescription"
      "If unresolved threat: describe it for surfacing later. Omit if resolved."
      (emptySchema TString))
  ]
  ["narration", "suggestedActions"]

-- | TRAUMA: Breaking point - stress reset, scar gained
traumaOutputSchema :: JSONSchema
traumaOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "2-3 tweets (≤840 chars). Interior microfiction—the breaking. Sensory fragments. This is earned."
      (emptySchema TString))
  , ("traumaAssigned", describeField "traumaAssigned"
      "REQUIRED. The scar: Cold, Haunted, Obsessed, Paranoid, Reckless, Soft, Vicious, Volatile (or invent)"
      (emptySchema TString))  -- String not enum - allow creative trauma names
  , ("suggestedActions", describeField "suggestedActions"
      "What now? Usually: rest, regroup, process"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "traumaAssigned", "suggestedActions"]  -- traumaAssigned is REQUIRED

-- | BARGAIN: Out of dice, generate bargain options for player choice
-- No tools needed - structured output defines the options, system presents via requestChoice
bargainOutputJSONSchema :: JSONSchema
bargainOutputJSONSchema = objectSchema
  [ ("bargainNarration", describeField "bargainNarration"
      "1 tweet (280 chars). Prose poetry—the city waits. Line breaks as desperation."
      (emptySchema TString))
  , ("bargainOptions", describeField "bargainOptions"
      "2-4 bargain options. Each has a cost and dice gained."
      (arraySchema bargainOptionSchema))
  , ("bargainCanRetreat", describeField "bargainCanRetreat"
      "Can they slip away? False if cornered."
      (emptySchema TBoolean))
  , ("bargainRetreatDesc", describeField "bargainRetreatDesc"
      "If retreat possible: how they escape (1 sentence)."
      (emptySchema TString))
  ]
  ["bargainNarration", "bargainOptions", "bargainCanRetreat"]
  where
    bargainOptionSchema = objectSchema
      [ ("bloLabel", describeField "bloLabel"
          "Short label: 'Take 2 stress for a die'"
          (emptySchema TString))
      , ("bloHint", describeField "bloHint"
          "Evocative hint (3-8 words): 'Cheap, but it hurts'"
          (emptySchema TString))
      , ("bloCostType", describeField "bloCostType"
          "Type of cost"
          (enumSchema ["stress", "heat", "clock", "faction", "trauma"]))
      , ("bloCostAmount", describeField "bloCostAmount"
          "Amount for stress/heat (1-3), ticks for clock (1-2)"
          (emptySchema TInteger))
      , ("bloCostTarget", describeField "bloCostTarget"
          "Clock ID or Faction ID if applicable"
          (emptySchema TString))
      , ("bloDiceGained", describeField "bloDiceGained"
          "Dice to add (1-3)"
          (emptySchema TInteger))
      , ("bloNarrative", describeField "bloNarrative"
          "1-2 sentences revealed AFTER choice. Consequences land."
          (emptySchema TString))
      ]
      ["bloLabel", "bloHint", "bloCostType", "bloCostAmount", "bloDiceGained", "bloNarrative"]

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT SCHEMA
-- ══════════════════════════════════════════════════════════════

compressionOutputJSONSchema :: JSONSchema
compressionOutputJSONSchema = objectSchema
  [ ("summary", describeField "summary"
      "One paragraph summary of what happened in the scene"
      (emptySchema TString))
  , ("keyMoments", describeField "keyMoments"
      "Array of key moments from the scene (3-5 items)"
      (arraySchema (emptySchema TString)))
  , ("consequenceSeeds", describeField "consequenceSeeds"
      "Comma-separated seeds for future consequences"
      (emptySchema TString))
  , ("stressChange", describeField "stressChange"
      "Net stress change from this scene"
      (emptySchema TInteger))
  , ("coinChange", describeField "coinChange"
      "Net coin change from this scene"
      (emptySchema TInteger))
  , ("newRumors", describeField "newRumors"
      "Rumors that emerged from this scene - gossip, news, hearsay spreading through the city"
      (arraySchema rumorInitSchema))
  ]
  ["summary", "keyMoments", "consequenceSeeds", "stressChange", "coinChange"]
  where
    rumorInitSchema = objectSchema
      [ ("riContent", describeField "riContent"
          "What the rumor says - a piece of gossip or news spreading through the city"
          (emptySchema TString))
      , ("riSpread", describeField "riSpread"
          "How widely known: whisper (few know), tavern (local gossip), common_knowledge (widely known)"
          (enumSchema ["whisper", "tavern", "common_knowledge"]))
      , ("riTrue", describeField "riTrue"
          "Is the rumor true? Omit if unknown."
          (emptySchema TBoolean))
      ]
      ["riContent", "riSpread"]
