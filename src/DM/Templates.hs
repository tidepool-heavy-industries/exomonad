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

    -- * Typed Jinja Templates
  , dmTurnJinja
  , compressionJinja
  , sceneJinja
  , actionJinja
  , aftermathJinja
  , downtimeJinja

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
import DM.Tools
import Tidepool.Template
import Tidepool.Schema
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

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

-- | Compression Jinja template (compile-time validated)
compressionJinja :: TypedTemplate CompressionContext SourcePos
compressionJinja = $(typedTemplateFile ''CompressionContext "templates/compression.jinja")

-- ══════════════════════════════════════════════════════════════
-- TIDEPOOL TEMPLATES
-- ══════════════════════════════════════════════════════════════

-- | Main DM turn template with all available tools
dmTurnTemplate :: Template DMContext TurnOutput DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept]
dmTurnTemplate = Template
  { templateJinja = dmTurnJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Compression template (no tools needed - uses unit state/event)
compressionTemplate' :: Template CompressionContext CompressionOutput () () '[]
compressionTemplate' = Template
  { templateJinja = compressionJinja
  , templateOutputSchema = compressionOutputSchema
  , templateTools = TNil
  }

-- | Scene template - exploration state
sceneTemplate :: Template DMContext TurnOutput DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept]
sceneTemplate = Template
  { templateJinja = sceneJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Action template - dice resolution state
actionTemplate :: Template DMContext TurnOutput DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept]
actionTemplate = Template
  { templateJinja = actionJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Aftermath template - consequence state
aftermathTemplate :: Template DMContext TurnOutput DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept]
aftermathTemplate = Template
  { templateJinja = aftermathJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Downtime template - recovery montage state
downtimeTemplate :: Template DMContext TurnOutput DMEvent WorldState '[Choose, SpendDie, Engage, Resolve, Accept]
downtimeTemplate = Template
  { templateJinja = downtimeJinja
  , templateOutputSchema = turnOutputSchema
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
  MoodTrauma _    -> render sceneTemplate ctx  -- TODO: trauma template

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

-- | Schema for the main turn output structure
turnOutputSchema :: Schema TurnOutput
turnOutputSchema = Schema
  { schemaJSON = schemaToValue turnOutputJSONSchema
  , schemaDescription = "DM turn output with narration and world state mutations"
  }

-- | Schema for scene compression output
compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = Schema
  { schemaJSON = schemaToValue compressionOutputJSONSchema
  , schemaDescription = "Scene compression output for persisting durable changes"
  }

-- ══════════════════════════════════════════════════════════════
-- TURN OUTPUT SCHEMA
-- ══════════════════════════════════════════════════════════════

turnOutputJSONSchema :: JSONSchema
turnOutputJSONSchema = objectSchema
  [ ("narration", describeField "narration"
      "Narrative prose describing what happens, including any NPC dialogue."
      (emptySchema TString))
  , ("stressDelta", describeField "stressDelta"
      "Change in stress (-9 to +9, 0 if no change)"
      (emptySchema TInteger))
  , ("coinDelta", describeField "coinDelta"
      "Change in coin (0 if no change)"
      (emptySchema TInteger))
  , ("heatDelta", describeField "heatDelta"
      "Change in heat (0 to +4). Violence, loud actions, Bluecoat attention."
      (emptySchema TInteger))
  , ("continueScene", describeField "continueScene"
      "True to continue the scene, false to end it"
      (emptySchema TBoolean))
  , ("suggestedActions", describeField "suggestedActions"
      "2-3 short suggested next actions for the player (3-8 words each)"
      (arraySchema (emptySchema TString)))
  ]
  ["narration", "stressDelta", "coinDelta", "heatDelta", "continueScene", "suggestedActions"]

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
