{-# LANGUAGE TemplateHaskell #-}
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
import DM.Output (TurnOutput, CompressionOutput, turnOutputJSONSchema, compressionOutputJSONSchema)
import DM.State (WorldState, DMMood(..))
import DM.Tools
import Tidepool.Template
import Tidepool.Schema
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

-- | Type alias for the full DM tool list
type DMTools = '[Choose, SpendDie, Engage, Resolve, Accept, AcceptBargain, Retreat, PassOut]

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
dmTurnTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
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
sceneTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
sceneTemplate = Template
  { templateJinja = sceneJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Action template - dice resolution state
actionTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
actionTemplate = Template
  { templateJinja = actionJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Aftermath template - consequence state
aftermathTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
aftermathTemplate = Template
  { templateJinja = aftermathJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Downtime template - recovery montage state
downtimeTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
downtimeTemplate = Template
  { templateJinja = downtimeJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Trauma template - breaking point, stress reset
traumaTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
traumaTemplate = Template
  { templateJinja = traumaJinja
  , templateOutputSchema = turnOutputSchema
  , templateTools = dmToolList
  }

-- | Bargain template - out of dice, make deals
bargainTemplate :: Template DMContext TurnOutput DMEvent WorldState DMTools
bargainTemplate = Template
  { templateJinja = bargainJinja
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
  MoodTrauma _    -> render traumaTemplate ctx
  MoodBargain _   -> render bargainTemplate ctx

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

