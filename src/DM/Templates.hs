-- | DM Templates - compile-time type-checked Jinja templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate'

    -- * Mood-Specific Templates
  , sceneTemplate
  , actionTemplate
  , aftermathTemplate
  , downtimeTemplate
  , traumaTemplate
  , bargainTemplate

    -- * Typed Jinja Templates
  , dmTurnJinja
  , compressionJinja
  , sceneJinja
  , actionJinja
  , aftermathJinja
  , downtimeJinja
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
  ) where

import DM.Context
import DM.Output
import DM.State (WorldState, DMMood(..))
import DM.Tools (DMEffects, DMEvent, dmToolList, SetSceneStyle, Choose, SpendDie, Engage, Resolve, Accept, AcceptBargain, Retreat, PassOut)
import Tidepool.Template
import Tidepool.Schema
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

-- | Type alias for the full DM tool list
type DMTools = '[SetSceneStyle, Choose, SpendDie, Engage, Resolve, Accept, AcceptBargain, Retreat, PassOut]

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

-- | Downtime mood template - recovery montage, time compression
downtimeJinja :: TypedTemplate DMContext SourcePos
downtimeJinja = $(typedTemplateFile ''DMContext "templates/downtime/main.jinja")

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

-- | Downtime template - recovery montage state
downtimeTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
downtimeTemplate = Template
  { templateJinja = downtimeJinja
  , templateOutputSchema = downtimeSchema
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
bargainTemplate :: Template DMContext TurnOutput DMEvent WorldState DMEffects DMTools
bargainTemplate = Template
  { templateJinja = bargainJinja
  , templateOutputSchema = bargainSchema
  , templateTools = dmToolList
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
  MoodDowntime _  -> render downtimeTemplate ctx
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

downtimeSchema :: Schema TurnOutput
downtimeSchema = Schema
  { schemaJSON = schemaToValue downtimeOutputSchema
  , schemaDescription = "Downtime: recovery montage. The ONLY place healing happens."
  }

traumaSchema :: Schema TurnOutput
traumaSchema = Schema
  { schemaJSON = schemaToValue traumaOutputSchema
  , schemaDescription = "Trauma: breaking point. Assign scar, reset stress."
  }

bargainSchema :: Schema TurnOutput
bargainSchema = Schema
  { schemaJSON = schemaToValue bargainOutputSchema
  , schemaDescription = "Bargain: out of dice. Present desperate options."
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
      "Narrative prose. NPC dialogue inline. End on an affordance or hook."
      (emptySchema TString))
  , ("coinDelta", describeField "coinDelta"
      "Direct transactions only (bought drink, paid informant). Usually 0."
      (emptySchema TInteger))
  , ("continueScene", describeField "continueScene"
      "True to continue, false to end scene"
      (emptySchema TBoolean))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 short next actions (3-8 words). One safe, one risky."
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "continueScene", "suggestedActions"]

-- | ACTION: Dice resolution via structured output
-- diceAction is REQUIRED - player chooses, game applies
actionOutputSchema :: JSONSchema
actionOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "Frame the tension BEFORE dice. After player chooses, this becomes aftermath."
      (emptySchema TString))
  , ("diceAction", describeField "diceAction"
      "REQUIRED. Precommit outcomes for each die in the pool."
      (objectSchema
        [ ("situation", describeField "situation" "What's at stake" (emptySchema TString))
        , ("position", describeField "position" "Controlled, Risky, or Desperate"
            (enumSchema ["Controlled", "Risky", "Desperate"]))
        , ("outcomes", describeField "outcomes"
            "One outcome per die in pool. outcomes[i] matches pool[i]."
            (arraySchema $ objectSchema
              [ ("dieValue", describeField "dieValue" "The die value (must match pool)" (emptySchema TInteger))
              , ("hint", describeField "hint" "3-8 word preview shown during choice" (emptySchema TString))
              , ("stressCost", describeField "stressCost" "Stress cost (0-3 typical)" (emptySchema TInteger))
              , ("heatCost", describeField "heatCost" "Heat cost (0-2 typical)" (emptySchema TInteger))
              , ("coinDelta", describeField "coinDelta" "Coin change" (emptySchema TInteger))
              , ("narrative", describeField "narrative" "1-3 sentences revealed after choice" (emptySchema TString))
              ]
              ["dieValue", "hint", "stressCost", "heatCost", "coinDelta", "narrative"]))
        ]
        ["situation", "position", "outcomes"]))
  , ("continueScene", describeField "continueScene"
      "True to continue after resolution"
      (emptySchema TBoolean))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 next actions based on outcome"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "diceAction", "continueScene", "suggestedActions"]

-- | AFTERMATH: Consequences landed, echoing forward
-- Coin for loot/payout, descriptions for future echoing
aftermathOutputSchema :: JSONSchema
aftermathOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "What the outcome means. One paragraph, then agency back to player."
      (emptySchema TString))
  , ("coinDelta", describeField "coinDelta"
      "Loot found, job payout, stolen goods. 0 if none."
      (emptySchema TInteger))
  , ("continueScene", describeField "continueScene"
      "True to continue, false to end"
      (emptySchema TBoolean))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 next actions: recover, press on, deal with fallout"
      (arraySchema (emptySchema TString)))
  , ("costDescription", describeField "costDescription"
      "If costly: describe the price paid for echoing later. Null if clean."
      (nullableSchema (emptySchema TString)))
  , ("threatDescription", describeField "threatDescription"
      "If unresolved threat: describe it for surfacing later. Null if resolved."
      (nullableSchema (emptySchema TString)))
  ]
  ["narration", "continueScene", "suggestedActions"]

-- | DOWNTIME: Recovery montage - the ONLY place healing happens
downtimeOutputSchema :: JSONSchema
downtimeOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "Montage of passing time. Compress hours/days into moments."
      (emptySchema TString))
  , ("stressHealed", describeField "stressHealed"
      "Stress recovered (positive number). 1-3 for short rest, 4-6 for extended."
      (emptySchema TInteger))
  , ("heatDecay", describeField "heatDecay"
      "Heat that fades with time (positive number). 0-2 typical."
      (emptySchema TInteger))
  , ("diceRecovered", describeField "diceRecovered"
      "Dice restored to pool. Full rest = full pool."
      (emptySchema TInteger))
  , ("timeElapsed", describeField "timeElapsed"
      "How much time passed: 'a few hours', 'three days', 'a week'"
      (emptySchema TString))
  , ("hookDescription", describeField "hookDescription"
      "What pulls them back to action. A knock, a rumor, a clock ticking."
      (emptySchema TString))
  ]
  ["narration", "stressHealed", "diceRecovered", "timeElapsed", "hookDescription"]

-- | TRAUMA: Breaking point - stress reset, scar gained
traumaOutputSchema :: JSONSchema
traumaOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "The moment they break. Visceral, specific, unforgettable."
      (emptySchema TString))
  , ("traumaAssigned", describeField "traumaAssigned"
      "REQUIRED. The scar: Cold, Haunted, Obsessed, Paranoid, Reckless, Soft, Vicious, Volatile (or invent)"
      (emptySchema TString))  -- String not enum - allow creative trauma names
  , ("suggestedActions", describeField "suggestedActions"
      "What now? Usually: rest, regroup, process"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "traumaAssigned", "suggestedActions"]  -- traumaAssigned is REQUIRED

-- | BARGAIN: Out of dice, desperate measures
-- Lean - tools handle the mechanics
bargainOutputSchema :: JSONSchema
bargainOutputSchema = objectSchema
  [ ("narration", describeField "narration"
      "Present the desperate situation. What can they bargain with?"
      (emptySchema TString))
  , ("suggestedActions", describeField "suggestedActions"
      "Options: accept the cost, retreat if possible, or collapse"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "suggestedActions"]

-- ══════════════════════════════════════════════════════════════
-- COMPRESSION OUTPUT SCHEMA
-- ══════════════════════════════════════════════════════════════

compressionOutputJSONSchema :: JSONSchema
compressionOutputJSONSchema = objectSchema
  [ ("summary", describeField "summary"
      "One paragraph summary of what happened in the scene"
      (emptySchema TString))
  , ("keyMoments", describeField "keyMoments"
      "Comma-separated list of the most important moments"
      (emptySchema TString))
  , ("consequenceSeeds", describeField "consequenceSeeds"
      "Comma-separated seeds for future consequences"
      (emptySchema TString))
  , ("stressChange", describeField "stressChange"
      "Net stress change from this scene"
      (emptySchema TInteger))
  , ("coinChange", describeField "coinChange"
      "Net coin change from this scene"
      (emptySchema TInteger))
  ]
  ["summary", "keyMoments", "consequenceSeeds", "stressChange", "coinChange"]
