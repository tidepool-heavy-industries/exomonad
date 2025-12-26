-- | DM Templates
module DM.Templates
  ( -- * Templates
    dmTurnTemplate
  , compressionTemplate
  
    -- * Rendering
  , renderDMContext
  , renderCompressionContext
  
    -- * Partials
  , renderNpcCard
  , renderClockCard
  , renderThreadCard
  , renderRumorCard
  , renderFactionCard
  ) where

import DM.State
import DM.Context
import DM.Output
import DM.Tools
import Tidepool.Template
import Tidepool.Schema
import Data.Text (Text)

-- ══════════════════════════════════════════════════════════════
-- TEMPLATES
-- ══════════════════════════════════════════════════════════════

dmTurnTemplate :: Template DMContext TurnOutput '[ThinkAsDM, SpeakAsNPC, AskPlayer, Choose]
dmTurnTemplate = Template
  { templateRender = renderDMContext
  , templateOutputSchema = turnOutputSchema
  , templateTools = ToolSchemas
  }

compressionTemplate :: Template CompressionContext CompressionOutput '[]
compressionTemplate = Template
  { templateRender = renderCompressionContext
  , templateOutputSchema = compressionOutputSchema
  , templateTools = ToolSchemas
  }

-- ══════════════════════════════════════════════════════════════
-- SCHEMAS
-- ══════════════════════════════════════════════════════════════

turnOutputSchema :: Schema TurnOutput
turnOutputSchema = error "TODO: turnOutputSchema - build JSON Schema with field descriptions for TurnOutput"

compressionOutputSchema :: Schema CompressionOutput
compressionOutputSchema = error "TODO: compressionOutputSchema - build JSON Schema for CompressionOutput"

-- ══════════════════════════════════════════════════════════════
-- RENDERING
-- ══════════════════════════════════════════════════════════════

renderDMContext :: DMContext -> Text
renderDMContext ctx = error "TODO: renderDMContext - render mustache template with DMContext"
  -- Should produce:
  -- - Scene header (location, stakes, tone)
  -- - Session goals
  -- - NPCs present (using renderNpcCard)
  -- - Scene beats so far
  -- - Clocks (visible and hidden)
  -- - Threads
  -- - Rumors
  -- - Factions in play
  -- - Tool instructions
  -- - Output schema reminder

renderCompressionContext :: CompressionContext -> Text
renderCompressionContext ctx = error "TODO: renderCompressionContext - render compression template"
  -- Should produce:
  -- - All scene beats
  -- - Faction states
  -- - NPC states
  -- - Active clocks
  -- - Active threads
  -- - Extraction instructions

-- ══════════════════════════════════════════════════════════════
-- PARTIALS
-- ══════════════════════════════════════════════════════════════

renderNpcCard :: NpcWithDisposition -> Text
renderNpcCard nwd = error "TODO: renderNpcCard - name, disposition, wants, voice notes"

renderClockCard :: Clock -> Text
renderClockCard clock = error "TODO: renderClockCard - emoji, name, filled/segments"

renderThreadCard :: Thread -> Text
renderThreadCard thread = error "TODO: renderThreadCard - hook, tension"

renderRumorCard :: Rumor -> Text
renderRumorCard rumor = error "TODO: renderRumorCard - content, spread level, truth"

renderFactionCard :: FactionSummary -> Text
renderFactionCard faction = error "TODO: renderFactionCard - name, attitude, current goal"
